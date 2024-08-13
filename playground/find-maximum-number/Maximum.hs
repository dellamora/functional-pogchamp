myMaximum :: (Ord a) => [a] -> a
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

main :: IO ()
main = do 
    let myList = [1, 5, 2, 9, 3, 7]
    let maxNumber = myMaximum myList
    print maxNumber
