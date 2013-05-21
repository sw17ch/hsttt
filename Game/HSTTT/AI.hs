module Game.HSTTT.AI where

import Game.HSTTT.Logic
import Game.HSTTT.Types

-- |The AI is currently very simple. It finds the first free cell and uses it
-- as its move.
aiMove :: Game -> (Int, Int)
aiMove g = head $ freeCells g
