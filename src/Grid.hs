module Grid where

import Control.Comonad
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Zipper as Z

newtype Grid a = Grid {runGrid :: Z.Zipper (Z.Zipper a)} deriving (Eq, Functor)

moveUp :: Grid a -> Grid a
moveUp (Grid z) = Grid $ Z.moveLeft z

moveDown :: Grid a -> Grid a
moveDown (Grid z) = Grid $ Z.moveRight z

moveLeft :: Grid a -> Grid a
moveLeft (Grid z) = Grid $ Z.moveLeft <$> z

moveRight :: Grid a -> Grid a
moveRight (Grid z) = Grid $ Z.moveRight <$> z

moveN :: (Grid a -> Grid a) -> Int -> (Grid a -> Grid a)
moveN move n = foldr (.) id (replicate n move)

moveRightN, moveLeftN, moveDownN, moveUpN :: Int -> (Grid a -> Grid a)
moveRightN = moveN moveRight
moveLeftN = moveN moveLeft
moveDownN = moveN moveDown
moveUpN = moveN moveUp

setPos :: (Int, Int) -> Grid a -> Grid a
setPos (x, y) = moveRightN y . moveDownN x

safeUp :: Grid a -> Maybe (Grid a)
safeUp (Grid z) = Grid <$> Z.safeLeft z

safeDown :: Grid a -> Maybe (Grid a)
safeDown (Grid z) = Grid <$> Z.safeRight z

safeLeft :: Grid a -> Maybe (Grid a)
safeLeft (Grid z) = case Z.safeLeft $ Z.focus z of
  Nothing -> Nothing
  Just _ -> Just $ Grid (Z.moveLeft <$> z)

safeRight :: Grid a -> Maybe (Grid a)
safeRight (Grid z) = case Z.safeRight $ Z.focus z of
  Nothing -> Nothing
  Just _ -> Just $ Grid (Z.moveRight <$> z)

update :: (a -> a) -> Grid a -> Grid a
update f (Grid z) = Grid $ Z.update (Z.update f) z

toLists :: Grid a -> [[a]]
toLists (Grid z) = Z.toList $ Z.toList <$> z

fromLists :: [[a]] -> Grid a
fromLists = Grid . Z.fromList . fmap Z.fromList

focus :: Grid a -> a
focus = Z.focus . Z.focus . runGrid

getPos :: Grid a -> (Int, Int)
getPos (Grid (Z.Zipper l (Z.Zipper l' _ _) _)) = (length l, length l')

findPos :: (a -> Bool) -> Grid a -> (Int, Int)
findPos p g = (row, col)
  where
    ls = toLists g
    row = fst $ fromJust $ find snd $ zip [0 ..] (fmap (any p) ls)
    col = fst $ fromJust $ find snd $ zip [0 ..] (fmap p (ls !! row))

adjacent :: Grid a -> [Grid a]
adjacent g = mapMaybe ($ g) [up, down, left, right, upRight, upLeft, downRight, downLeft]
  where
    up = safeUp
    down = safeDown
    left = safeLeft
    right = safeRight
    upRight = safeRight >=> safeUp
    upLeft = safeLeft >=> safeUp
    downRight = safeRight >=> safeDown
    downLeft = safeLeft >=> safeDown

instance (Show a) => Show (Grid a) where
  -- show (Grid z) = intercalate "\n" (Z.toList $ show <$> z)
  show (Grid z) = intercalate "\n" $ case show <$> z of
    Z.Zipper l m r -> reverse l <> [m <> " <"] <> r

instance Comonad Grid where
  extract = focus
  duplicate (Grid z) = Grid <$> Grid layers
    where
      layer u = Z.Zipper (lefts u) u (rights u)
      lefts z@(Z.Zipper l (Z.Zipper l' _ _) _) = take (length l') (tail $ iterate (fmap Z.moveLeft) z)
      rights z@(Z.Zipper _ (Z.Zipper _ _ r') r) = take (length r') (tail $ iterate (fmap Z.moveRight) z)
      layers = layer (layer z)
