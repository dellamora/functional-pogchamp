import Data.List (sort)

findMedianSortedArrays :: [Int] -> [Int] -> Double
findMedianSortedArrays nums1 nums2
    | m > n     = findMedianSortedArrays nums2 nums1  
    | otherwise = binarySearch 0 m
    where
        m = length nums1
        n = length nums2
        total = m + n
        half = (total + 1) `div` 2

        binarySearch :: Int -> Int -> Double
        binarySearch low high
            | low <= high =
                let cut1 = (low + high) `div` 2
                    cut2 = half - cut1
                    l1 = if cut1 == 0 then minBound else nums1 !! (cut1 - 1)
                    l2 = if cut2 == 0 then minBound else nums2 !! (cut2 - 1)
                    r1 = if cut1 == m then maxBound else nums1 !! cut1
                    r2 = if cut2 == n then maxBound else nums2 !! cut2
                in
                    if l1 <= r2 && l2 <= r1 then
                        if even total then
                            fromIntegral (max l1 l2 + min r1 r2) / 2
                        else
                            fromIntegral $ max l1 l2
                    else if l1 > r2 then
                        binarySearch low (cut1 - 1)
                    else
                        binarySearch (cut1 + 1) high
            | otherwise = error "Input arrays are not sorted"
            | otherwise = error "Input arrays are not sorted"

testMedian :: [Int] -> [Int] -> IO ()
testMedian nums1 nums2 = do
    putStrLn $ "nums1 = " ++ show nums1
    putStrLn $ "nums2 = " ++ show nums2
    putStrLn $ "Median: " ++ show (findMedianSortedArrays nums1 nums2)
    putStrLn $ "Verification: " ++ show (naiveMedian nums1 nums2)
    putStrLn ""

naiveMedian :: [Int] -> [Int] -> Double
naiveMedian nums1 nums2 =
    let merged = sort (nums1 ++ nums2)
        len = length merged
        mid = len `div` 2
    in if even len
       then fromIntegral (merged !! (mid - 1) + merged !! mid) / 2
       else fromIntegral (merged !! mid)

main :: IO ()
main = do
    testMedian [1,3] [2]
    testMedian [1,2] [3,4]
    testMedian [] [1]
    testMedian [2] []
    testMedian [1,3,5] [2,4,6]