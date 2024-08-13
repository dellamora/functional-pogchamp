
mySum :: [Int] -> Int
main :: IO ()


mySum [] = 0
mySum (x: xs) = x + mySum xs

main = do 
    let result = mySum [1,2,3,4,5]
    print result