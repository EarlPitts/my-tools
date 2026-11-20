{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module EverybodyCodes.Utils (
  getInput,
  getCookie,
) where

import Control.Lens
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), makeIV)
import Crypto.Data.Padding (Format (PKCS7), unpad)
import Crypto.Error (CryptoFailable, eitherCryptoError)
import qualified Data.Aeson.Key as K
import Data.Aeson.Lens (key, _Integer, _String)
import Data.Aeson.Types
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Network.HTTP.Req

getInput :: ByteString -> Int -> Int -> Int -> IO Text
getInput sessionCookie event day part = do
  let headers = (header "Cookie" sessionCookie)
      eventTxt = T.show event
      dayTxt = T.show day
      partTxt = T.show part
  seed <- getSeed headers
  cipher <- getCipher headers seed eventTxt dayTxt partTxt
  keyStr <- getKey headers eventTxt dayTxt partTxt
  let iv = T.take 16 keyStr
  case decryptCBC (encodeUtf8 keyStr) (encodeUtf8 iv) (encodeUtf8 cipher) of
    Left e -> fail $ "Couldn't decrypt: " <> e
    Right output -> case decodeUtf8' output of
      Left e -> fail $ "Couldn't decode: " <> show e
      Right txt -> pure txt

sendReq :: Option Https -> Url Https -> IO (JsonResponse Value)
sendReq headers url =
  runReq defaultHttpConfig $
    req GET url NoReqBody jsonResponse headers

getSeed :: Option Https -> IO Integer
getSeed headers = do
  resp <- sendReq headers (https "api.everybody.codes" /: "user" /: "me")
  pure $ (responseBody resp) ^?! (key "seed") . _Integer

getKey :: Option Https -> Text -> Text -> Text -> IO Text
getKey headers event day part = do
  resp <- sendReq headers (https "api.everybody.codes" /: "event" /: event /: "quest" /: day)
  pure $ responseBody resp ^?! key (K.fromText $ "key" <> part) . _String

getCipher :: Option Https -> Integer -> Text -> Text -> Text -> IO Text
getCipher headers seed event day part = do
  resp <- sendReq headers (https "everybody.codes" /: "assets" /: event /: day /: "input" /: ((T.show seed) <> ".json"))
  pure $ responseBody resp ^?! key (K.fromText part) . _String

decryptCBC :: ByteString -> ByteString -> ByteString -> Either String ByteString
decryptCBC key ivBytes b16 = do
  ciphertext <- convertFromBase Base16 b16
  cipher <-
    either (Left . show) Right $
      eitherCryptoError (cipherInit key :: CryptoFailable AES256)
  iv <- maybe (Left "invalid IV") Right (makeIV ivBytes)
  maybe (Left "invalid padding") Right $
    unpad (PKCS7 16) (cbcDecrypt (cipher :: AES256) iv ciphertext)

getCookie :: IO ByteString
getCookie = BS.readFile "cookie.txt"
