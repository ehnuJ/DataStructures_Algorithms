{--
Name: Joon Hee Ooten
Last modified: 12/6/23
--}

import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import qualified Data.List as List
import Data.Maybe

-- determines the player type and actions
data Player = One | Two deriving (Show, Eq)

-- alternates between players 1 and 2 after every turn
change :: Player -> Player
change One = Two
change Two = One

type Board = Seq.Seq Int

-- initial board
initial :: Board
initial = Seq.fromList [5, 4, 3, 2, 1]

-- method for verifying validity of move
move :: Board -> (Int, Int) -> Maybe Board
move board (row, stars)
  | and [(Seq.index board row) >= stars,
          row < 5] = Just (Seq.adjust (\x -> x - stars) row board)
  | otherwise = Nothing

-- method to turn the board into asterisks
display :: Board -> String
display board = List.intercalate "\n" (zipWith (++) numbers (stars board))
                where numbers = ["1. ", "2. ", "3. ", "4. ", "5. "]
                      stars board = [(concat . take n) (repeat "* ")
                                    | n <- Fol.toList board]

-- method for gameplay
-- initiates player turn
-- receives user input for row and removal of stars
-- if valid, stars are removed and game state is checked
play :: Board -> Player -> IO ()
play board player = do putStrLn ("\nIt's your turn, Player " ++ (show player) ++ "!") 
                       putStrLn "Choose a row to remove stars!"
                       row <- getLine
                       putStrLn "How many stars do you want to remove?"
                       stars <- getLine
                       let newBoard = move board ((read row) - 1, read stars)
                       if newBoard == Nothing
                         then do putStrLn "Not valid movement"
                                 play board player
                         else isOver (fromJust newBoard) (change player)

-- method to determine if board is empty
-- continues game if not empty, otherwise ends game
isOver :: Board -> Player -> IO()
isOver board player = do if board == Seq.fromList [0, 0, 0, 0, 0]
                           then putStrLn ("Congratulations, Player " ++ (show player)
                                         ++ ", you win!")
                           else do putStrLn ""
                                   putStrLn (display board)
                                   play board player

-- main method to control IO
main :: IO ()
main = nim

-- initiates welcome message and start of turn
nim :: IO ()
nim = do putStrLn "Welcome to nim!"
         putStrLn (display initial)
         play initial One