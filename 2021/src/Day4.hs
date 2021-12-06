module Day4 where

import Util

type GameBoard = [[Int]]
-- Can't use set here because we need the last added for final score calculation :/
type Numbers = [Int]
data GameState = GameState { boards:: [GameBoard], calledNumbers :: Numbers, numbersToCall :: [Int] }

parseInts :: [String] -> [Int]
parseInts = map parseInt

loadBoard :: [String] -> GameBoard
loadBoard inStr = go 0 inStr
  where
    go depth inStr
      | depth == 5 = []
      | otherwise  = (parseInts . words . head) inStr : go (depth + 1) (tail inStr)

loadBoards :: [String] -> [GameBoard]
loadBoards xs = map loadBoard (split (=="") xs)

loadInitialGameState :: String -> GameState
loadInitialGameState inStr = GameState {
  boards = loadBoards $ (tail . tail . lines) inStr,
  calledNumbers = [],
  numbersToCall = (parseInts . split (==',') . head . lines) inStr
}

check :: (Int -> Int) -> Numbers -> Bool
check = go 0
  where
    go index indexFunc cn
      | index == 5 = True
      | otherwise  = elem (indexFunc index) cn && go (index + 1) indexFunc cn

transpose :: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

checkRows :: Numbers -> GameBoard -> Bool
checkRows cn = any (\n -> check (n !!) cn)

checkCols :: Numbers -> GameBoard -> Bool
checkCols cn = checkRows cn . transpose

boardIsWinner :: Numbers -> GameBoard -> Bool
boardIsWinner cn b = checkCols cn b || checkRows cn b

anyBoardIsWinner :: Numbers -> [GameBoard] -> Bool
anyBoardIsWinner = any . boardIsWinner

allButOneBoardIsWinner :: Numbers -> [GameBoard] -> Bool
allButOneBoardIsWinner cn bs = length (filter (not . boardIsWinner cn) bs) == 1

allBoardsAreWinner :: Numbers -> [GameBoard] -> Bool
allBoardsAreWinner = all . boardIsWinner

filterBoards :: (GameBoard -> Bool) -> [GameBoard] -> GameBoard
filterBoards filterFunc = head . filter filterFunc

firstWinner :: Numbers -> [GameBoard] -> GameBoard
firstWinner cn = filterBoards (boardIsWinner cn)

firstNonWinner :: Numbers -> [GameBoard] -> GameBoard
firstNonWinner cn = filterBoards (not . boardIsWinner cn)

getFirstWinningBoard :: [Int] -> [GameBoard] -> (Numbers, GameBoard)
getFirstWinningBoard = go []
  where
    go cn numbersToCall bs
      | anyBoardIsWinner cn bs = (cn, firstWinner cn bs)
      | otherwise              = go (head numbersToCall : cn) (tail numbersToCall) bs

getLastWinningBoard :: [Int] -> [GameBoard] -> (Numbers, GameBoard)
getLastWinningBoard = go []
  where
    go cn numbersToCall bs
      | allBoardsAreWinner cn bs     = (cn, head bs)
      | allButOneBoardIsWinner cn bs = go (head numbersToCall : cn) (tail numbersToCall) [firstNonWinner cn bs]
      | otherwise                    = go (head numbersToCall : cn) (tail numbersToCall) bs

calcBoardScore :: Numbers -> GameBoard -> Int
calcBoardScore cn b = head cn * sum (filter (`notElem` cn) (concat b))

calcPart1 :: GameState -> Int
calcPart1 state = uncurry calcBoardScore $ getFirstWinningBoard (numbersToCall state) (boards state)

calcPart2 :: GameState -> Int
calcPart2 state = uncurry calcBoardScore $ getLastWinningBoard (numbersToCall state) (boards state)

showDay :: (Integer -> Int -> IO ()) -> String -> IO ()
showDay printPartResult filename = do
  inStr <- readInput filename
  let state = loadInitialGameState inStr
  printPartResult 1 $ calcPart1 state
  printPartResult 2 $ calcPart2 state
