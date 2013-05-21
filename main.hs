module Main where

import Game.HSTTT

main :: IO ()
main = runGame mkNewGame >>= print

getPlayerMove :: Game -> IO Move
getPlayerMove _ = do
  [x,y] <- getLine
  return $ Move MarkX (read [x]) (read [y])

-- |The AI is currently very simple. It finds the first free cell and uses it
-- as its move.
getAIMove :: Game -> IO Move
getAIMove g = do
  putStrLn $ "AI move: " ++ show x ++ show y
  return $ Move MarkO x y
  where
    (x, y) = head $ freeCells g

runGame :: Game -> IO Result
runGame g = do
  r <- runTurn g

  case r of
    Continue g' -> runGame g'
    _ -> return r

runTurn :: Game -> IO Result
runTurn g = do
  r <- runMove "human" g getPlayerMove

  case r of
    Continue g' -> runMove "AI" g' getAIMove
    _ -> return r

  where
    runMove n b mover = do
      putStrLn $ n ++ " turn:"
      print b
      m <- mover b
      return $ result $ applyMove b m
