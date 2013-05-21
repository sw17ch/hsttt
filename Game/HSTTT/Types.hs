module Game.HSTTT.Types
  ( Game(..)
  , Move(..)
  , Player(..)
  , Mark(..)
  , Result(..)
  ) where

import Data.List

data Game = Game [[Mark]]         deriving (Eq)
data Move = Move Mark Int Int     deriving (Eq)
data Player = PlayerX | PlayerO   deriving (Eq)
data Mark = MarkX | MarkO | Empty deriving (Eq)
data Result = Winner Player
            | Continue Game
            | Draw

instance Show Game where
  show (Game rs) = unlines $ intersperse hdiv $ map showLine rs
    where
      hdiv = "-----"
      showLine ls = intersperse '|' $ concatMap show ls

instance Show Mark where
  show MarkX = "X"
  show MarkO = "O"
  show Empty = " "

instance Show Player where
  show PlayerX = "Player X"
  show PlayerO = "Player O"

instance Show Result where
  show Draw = "Draw!"
  show (Winner p) = "Winner is " ++ show p
  show (Continue _) = "Game isn't done yet."
