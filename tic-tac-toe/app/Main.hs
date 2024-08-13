import Text.Read (readMaybe)

data Position = X | O | Empty deriving (Eq, Show)
type Board = [[Position]]
data Game = Game { board :: Board, player :: Position }
data GameState = GameState { game :: Game, winner :: Maybe Position }

createBoard :: Board
createBoard = replicate 3 (replicate 3 Empty)

printBoard :: Board -> IO ()
printBoard = mapM_ print

currentPlayer :: Board -> Position
currentPlayer board = if xs > os then O else X
    where xs = length $ concatMap (filter (== X)) board
          os = length $ concatMap (filter (== O)) board

isValidMovie :: Board -> (Int, Int) -> Bool
isValidMovie board (x, y) = x >= 0 && x < 3 && y >= 0 && y < 3 && board !! x !! y == Empty

handleMove :: Board -> (Int, Int) -> Position -> Board
handleMove board (x, y) player =
   if isValidMovie board (x, y) then
       let (left, row: right) = splitAt x board
           (leftRow, _: rightRow) = splitAt y row
       in left ++ (leftRow ++ player : rightRow) : right
   else board

gameLoop :: Game -> IO ()
gameLoop game = do
    printBoard $ board game
    putStrLn $ "Player " ++ show (player game) ++ "'s turn"
    putStrLn "Enter row and column (e.g., 1 2):"
    input <- getInput
    case input of
        Just (row, col) -> do
            let newBoard = handleMove (board game) (row, col) (player game)
            let newPlayer = currentPlayer newBoard
            gameLoop $ Game newBoard newPlayer
        Nothing -> do
            putStrLn "Invalid input. Please enter two integers separated by a space."
            gameLoop game

getInput :: IO (Maybe (Int, Int))
getInput = do
  input <- getLine
  let parts = words input
  case parts of
    [row, col] -> return $ do
      row' <- readMaybe row
      col' <- readMaybe col
      Just (row', col')
    _ -> return Nothing
main :: IO ()
main = do
    putStrLn "Welcome to Tic Tac Toe"
    putStrLn "Player X starts"
    gameLoop $ Game createBoard X
