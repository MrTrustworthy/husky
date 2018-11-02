

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where
        smallerSorted   = quicksort $ filter (<= x) xs
        biggerSorted    = quicksort $ filter (> x) xs

main = do
    print $ quicksort "the quick brown fox jumps over the lazy dog"