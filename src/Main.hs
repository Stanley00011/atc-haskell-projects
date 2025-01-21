module Main where

import System.IO (hFlush, stdout)
import Data.Maybe (isNothing, fromMaybe)

-- Define the Board type as a 3x3 grid of Maybe Char values.
type Board = [[Maybe Char]]

-- Define the GameState type to store the board and the current player's turn.
data GameState = GameState
  { board :: Board
  , currentPlayer :: Char
  }

-- The initial empty board (all cells set to Nothing).
defaultBoard :: Board
defaultBoard = replicate 3 (replicate 3 Nothing)

-- The initial game state, starting with an empty board and 'X' as the first player.
defaultState :: GameState
defaultState = GameState defaultBoard 'X'

-- The main function initializes the game by welcoming the user and starting the game loop.
main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe!"
  gameLoop defaultState

-- Main Game Loop: Handles the gameplay logic, including input, validation, and game state updates.
gameLoop :: GameState -> IO ()
gameLoop state = do
  printBoard (board state) -- Display the current state of the board.
  putStrLn $ "Player " ++ [currentPlayer state] ++ "'s turn."
  putStrLn "Enter your move (row and column, e.g., 1 2): "
  hFlush stdout
  input <- getLine
  case parseMove input of
    Just (row, col) ->
      if isValidMove (board state) row col
        then do
          let newBoard = makeMove (board state) row col (currentPlayer state)
          if isWin newBoard (currentPlayer state)
            then do
              printBoard newBoard
              putStrLn $ "Player " ++ [currentPlayer state] ++ " wins!" -- Announce the winner.
              restartPrompt
            else if isDraw newBoard
              then do
                printBoard newBoard
                putStrLn "It's a draw!" -- Announce a draw if no moves are left.
                restartPrompt
              else gameLoop $ GameState newBoard (nextPlayer $ currentPlayer state)
        else do
          putStrLn "Invalid move! Try again." -- Notify user if move is invalid.
          gameLoop state
    Nothing -> do
      putStrLn "Invalid input! Enter row and column as two numbers." -- Handle invalid input format.
      gameLoop state

-- Parse User Input: Converts user input into row and column indices.
parseMove :: String -> Maybe (Int, Int)
parseMove input =
  case map reads (words input) of
    [[(row, "")], [(col, "")]] -> Just (row - 1, col - 1) -- Adjust input to zero-based index.
    _ -> Nothing

-- Check Valid Move: Ensures the selected cell is within bounds and empty.
isValidMove :: Board -> Int -> Int -> Bool
isValidMove b row col =
  row >= 0 && row < 3 && col >= 0 && col < 3 && isNothing (b !! row !! col)

-- Make Move: Updates the board with the current player's move at the specified location.
makeMove :: Board -> Int -> Int -> Char -> Board
makeMove b row col player =
  take row b ++ [take col (b !! row) ++ [Just player] ++ drop (col + 1) (b !! row)] ++ drop (row + 1) b

-- Check Win: Determines if the current player has a winning line (row, column, or diagonal).
isWin :: Board -> Char -> Bool
isWin b player =
  let winLine line = all (== Just player) line
   in any winLine (b ++ transpose b ++ diagonals b) -- Combines rows, columns, and diagonals for checks.

-- Check Draw: Verifies if the board is full and no winner exists.
isDraw :: Board -> Bool
isDraw b = all (all (/= Nothing)) b

-- Get Next Player: Alternates between 'X' and 'O'.
nextPlayer :: Char -> Char
nextPlayer 'X' = 'O'
nextPlayer 'O' = 'X'

-- Restart Game: Prompts the user to restart or exit the game.
restartPrompt :: IO ()
restartPrompt = do
  putStrLn "Do you want to play again? (yes/no): "
  hFlush stdout
  input <- getLine
  case input of
    "yes" -> gameLoop defaultState -- Restart the game with default state.
    "no" -> putStrLn "Goodbye!" -- Exit the game.
    _ -> do
      putStrLn "Invalid input. Please enter 'yes' or 'no'."
      restartPrompt

-- Display Board: Prints the board to the console in a readable format.
printBoard :: Board -> IO ()
printBoard = mapM_ (putStrLn . formatRow)
  where
    formatRow = unwords . map (fromMaybe " " . fmap (:[]))

-- Get Diagonals: Extracts the two diagonals for win-checking.
diagonals :: Board -> [[Maybe Char]]
diagonals b = [[b !! i !! i | i <- [0 .. 2]], [b !! i !! (2 - i) | i <- [0 .. 2]]]

-- Transpose Board: Swaps rows and columns to facilitate column checks.
transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)

