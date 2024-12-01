module Game where

import System.IO (hFlush, stdout)

data Player = Player {
    playerName :: String,
    playerHealth :: Int,
    playerAlive :: Bool
} deriving (Show, Eq)

data GameState = GameState {
    players :: [Player],
    doorOpen :: Bool,
    elementalPresent :: Bool,
    gameOver :: Bool
} deriving (Show)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

newPlayer :: String -> Player
newPlayer name = Player {
    playerName = name,
    playerHealth = 100,
    playerAlive = True
}

createInitialState :: [String] -> GameState
createInitialState names = GameState {
    players = map newPlayer names,
    doorOpen = False,
    elementalPresent = True,
    gameOver = False
}

pullLever :: GameState -> GameState
pullLever state = state { doorOpen = True }

elementalAttack :: Player -> Player
elementalAttack player = player {
    playerHealth = playerHealth player - 80,
    playerAlive = playerHealth player - 80 > 0
}

showStatus :: GameState -> IO ()
showStatus state = do
    putStrLn "\nCurrent state:"
    putStrLn $ "Door is: " ++ if doorOpen state then "open" else "closed"
    putStrLn "Players:"
    mapM_ (\p -> putStrLn $ "- " ++ playerName p ++ 
                           " (Health: " ++ show (playerHealth p) ++ 
                           ", " ++ if playerAlive p then "Alive" else "Dead" ++ ")")
          (players state)

data Command = Pull | Enter | Run | Help | Quit
    deriving (Show, Eq, Read)

parseCommand :: String -> Maybe Command
parseCommand input = case map (\ x -> (read x :: Command)) (filter (not . null) [input]) of
    [cmd] -> Just cmd
    _ -> Nothing

showHelp :: IO ()
showHelp = do
    putStrLn "Available commands:"
    putStrLn "  Pull  - Pull the lever to open the door"
    putStrLn "  Enter - Enter the room"
    putStrLn "  Run   - Try to run away"
    putStrLn "  Help  - Show this help message"
    putStrLn "  Quit  - Exit the game"

gameLoop :: GameState -> IO ()
gameLoop state = do
    showStatus state
    
    if gameOver state
        then return ()
        else do
            input <- prompt "\nWhat would you like to do? (type Help for commands): "
            case parseCommand input of
                Just Help -> do
                    showHelp
                    gameLoop state
                    
                Just Pull -> 
                    if not (doorOpen state)
                        then do
                            putStrLn "\nYou pull the lever. The door opens!"
                            gameLoop (pullLever state)
                        else do
                            putStrLn "\nThe door is already open!"
                            gameLoop state
                            
                Just Enter ->
                    if not (doorOpen state)
                        then do
                            putStrLn "\nThe door is closed! You need to pull the lever first."
                            gameLoop state
                        else do
                            putStrLn "\nYou enter the room... The fire elemental attacks!"
                            let (targetPlayer:otherPlayers) = players state
                            let newState = state { 
                                players = elementalAttack targetPlayer : otherPlayers,
                                gameOver = True
                            }
                            gameLoop newState
                            
                Just Run -> do
                    putStrLn "\nYou try to run away..."
                    if doorOpen state
                        then do
                            putStrLn "You successfully escape!"
                            gameLoop state { gameOver = True }
                        else do
                            putStrLn "The door is closed! You can't escape!"
                            gameLoop state
                            
                Just Quit -> do
                    putStrLn "\nThanks for playing!"
                    return ()
                    
                Nothing -> do
                    putStrLn "\nInvalid command! Type 'Help' for available commands."
                    gameLoop state

setupGame :: IO GameState
setupGame = do
    putStrLn "Welcome to the Room Game!"
    putStrLn "Enter names for the three players:"
    
    name1 <- prompt "Player 1 name: "
    name2 <- prompt "Player 2 name: "
    name3 <- prompt "Player 3 name: "
    
    let initialState = createInitialState [name1, name2, name3]
    putStrLn "\nGame started! You're in front of a door with a lever."
    putStrLn "There's a fire elemental inside..."
    return initialState

main :: IO ()
main = do
    initialState <- setupGame
    showHelp
    gameLoop initialState