module Problems (
    lastOf,
    secondLastOf,
    elementAt,
    lengthOf,
    isPalindrome,
    flattenList,
    flattenList2,
    compress,
    compressConsecutive,
    pack,
    rePack,
    runLength,
    runLengthS,
    decodeRL,
    replicateMe,
    NestedList(..),
    AmountsOf(..)
) where



lastOf :: [a] -> a
lastOf xs = head $ reverse xs

secondLastOf :: [a] -> a
secondLastOf = head . tail . reverse

elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n-1)

lengthOf :: [a] -> Int
lengthOf a = countMe a 0
    where
        countMe [] n        = n
        countMe (x:xs) n    = countMe xs (n+1)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (first:xs) = let
    theLast     = lastOf xs
    others      = reverse $ tail $ reverse xs
    in if first == theLast then isPalindrome others else False

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a]

flattenList :: NestedList a -> [a]
flattenList (Elem x) = [x]
flattenList (List x) = concatMap flattenList x

flattenList2 :: NestedList a -> [a]
flattenList2 (Elem x) = [x]
flattenList2 (List (x:xs)) = flattenList2 x ++ flattenList2 (List xs)
flattenList2 (List []) = []

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress xs = reverse $ acc xs [] where
    acc [] c        = c
    acc (x:xs) c    = if x `elem` c then acc xs c else acc xs ([x] ++ c)

compressConsecutive :: (Eq a) => [a] -> [a]
compressConsecutive [] = []
compressConsecutive xs = reverse $ compressMe xs [] where
    compressMe [] acc       = acc
    compressMe [x] acc      = [x] ++ acc
    compressMe (x:xs) acc
        | x == (head xs)    = compressMe xs acc
        | otherwise         = compressMe xs ([x] ++ acc)

-- 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = doPack xs [] [] where
    doPack [] acc []            = acc
    doPack [] acc streakAcc     = acc ++ [streakAcc]
    doPack (x:xs) acc streakAcc
        | x `elem` streakAcc || length streakAcc == 0   = doPack xs acc (streakAcc ++ [x])
        | otherwise                                     = doPack xs (acc ++ [streakAcc]) [x]

rePack :: [[Char]] -> [Char]
rePack [] = error "Cant repack an empty string list"
rePack xs = doRePack xs "" where
    doRePack [] acc         = acc
    doRePack (x:xs) acc     = acc ++ ":" ++ x ++ (doRePack xs acc)

-- 10
runLength :: Eq a => [a] -> [(Int, a)]
-- runLength xs = [(length x, head x) | x <- (pack xs)]
runLength xs = foldl (\acc x -> acc ++ [(length x, head x)]) [] (pack xs)


-- 11
data AmountsOf a = Single a | Multiple (Int, a) deriving (Show, Eq)

runLengthS :: Eq a => [a] -> [AmountsOf a]
runLengthS xs = [if (length x) > 1 then Multiple (length x, head x) else Single (head x) | x <- (pack xs)]

decodeRL :: Eq a => [AmountsOf a] -> [a]
decodeRL xs = innerDecode xs [] where
    innerDecode [] acc                      = acc
    innerDecode ((Single x):xs) acc         = innerDecode xs (acc ++ [x])
    innerDecode ((Multiple (n, x)):xs) acc  = innerDecode xs (acc ++ (replicate n x))

replicateMe :: [a] -> Int -> [a]
replicateMe xs v = innerReplicate xs v [] where
    innerReplicate [] _ acc     = acc
    innerReplicate (x:xs) v acc = innerReplicate xs v (acc ++ replicate v x)
