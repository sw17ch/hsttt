module Game.TTT.Logic
  ( applyMove
  , mkNewGame
  , freeCells
  , result
  ) where

import Game.TTT.Types
import Game.TTT.Util
import Data.List
import Data.Maybe

applyMove :: Game -> Move -> Game
applyMove (Game rows) (Move mm mx my) = Game $ mapWithIndex withRow rows
  where
    withRow :: [Mark] -> Int -> [Mark]
    withRow row y = mapWithIndex (\m x -> withCell m x y) row

    withCell :: Mark -> Int -> Int -> Mark
    withCell m x y | x == mx && y == my = case m of
                                            Empty -> mm
                                            _     -> error $ errMsg m
                   | otherwise = m

    errMsg :: Mark -> String
    errMsg m = error $ "Already set to " ++ show m ++ "!"

mkNewGame :: Game
mkNewGame = Game $ threeOf row
  where
    row = threeOf Empty
    threeOf = replicate 3

gameRows :: Game -> [[Mark]]
gameRows (Game rs) = rs

gameCols :: Game -> [[Mark]]
gameCols (Game rs) = transpose rs

gameDiags :: Game -> [[Mark]]
gameDiags (Game rs) = [lr, rl]
  where
    lr = shift rs
    rl = shift $ map reverse rs
    shift lls = let dropped = mapWithIndex (flip drop) lls
                in map head dropped

-- Collect all the different ways to win.  Filter only those strips with all
-- marks identical.  Reduce those strips of identical marks to a single unique
-- element.  Stick all unique marks into one list.
gameWinner :: Game -> Maybe Player
gameWinner g | MarkX `elem` same = Just PlayerX
             | MarkO `elem` same = Just PlayerO
             | otherwise = Nothing
  where
    same = concatMap nub $ filter allEq strips

    strips = gameRows g ++ gameCols g ++ gameDiags g

gameDraw :: Game -> Bool
gameDraw g = let l = length $ freeCells g
             in 0 == l

result :: Game -> Result
result g =
  case gameWinner g of
    (Just p) -> Winner p
    Nothing -> if gameDraw g
                  then Draw
                  else Continue g

freeCells :: Game -> [(Int, Int)]
freeCells (Game rs) = catMaybes $ concat $ mapWithIndex rowContext rs
  where
    rowContext :: [Mark] -> Int -> [Maybe (Int, Int)]
    rowContext r y = mapWithIndex (`cellContext` y) r

    cellContext :: Mark -> Int -> Int -> Maybe (Int, Int)
    cellContext Empty y x = Just (x, y)
    cellContext _ _ _ = Nothing
