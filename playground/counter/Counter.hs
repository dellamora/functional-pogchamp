myCount :: [a] -> Int
myCount [] = 0
myCount (_:xs) = 1 + myCount xs

main :: IO ()
main = do 
    let myList = [1,2,3,4,5]
    let count = myCount myList
    print count