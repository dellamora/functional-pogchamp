binarySearch :: Ord a => [a] -> a -> Maybe Int
binarySearch xs target = 
    let search low high
            | low > high = Nothing
            | otherwise =
                let mid = (low + high) `div` 2
                in case compare target (xs !! mid) of 
                    LT -> search low (mid - 1) 
                    GT -> search (mid + 1) high
                    EQ -> Just mid 
    in search 0 (length xs - 1)

main :: IO ()

main = do 
    let list = [1,2,5,7,9,11]
    print $ binarySearch list 5
    print $ binarySearch list 6  



    -- Create a game with three NPC's, a fire elemental a lever 
-- that open the elemental's cage so the players can enter and fith it

-- Rules 
-- woman npc open the cage clicking on the lever 
-- first player that enter the cage loses 80 hp

-- first thing we will do is just print a dor that when click will be open using /home/fran/dellamora/functional-pogchamp/sprites/tiles/wall/door_spritesheet.png a door sprite
