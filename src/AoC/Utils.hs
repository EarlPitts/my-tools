module AoC.Utils where

import Text.Parsec
import Text.Parsec.String

readInput = readFile "input.txt"
readExample = readFile "example.txt"

readInputList = words <$> readFile "input.txt"

solve :: (String -> Int) -> IO Int
solve solution = solution <$> readInput

solveList :: ([String] -> Int) -> IO Int
solveList solution = solution <$> readInputList

iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m [a]
iterateUntilM p f a = do
  a' <- f a
  if p a'
    then return [a']
    else (a' :) <$> iterateUntilM p f a'

fixpoint :: (Eq a) => [a] -> a
fixpoint (x : y : as) = if x == y then x else fixpoint (y : as)

indexGrid :: [[a]] -> [[(Int, Int, a)]]
indexGrid grid =
  [ [(r, c, elem) | (c, elem) <- zip [0 ..] row]
  | (r, row) <- zip [0 ..] grid
  ]

int :: Parser Int
int = read <$> many1 digit
