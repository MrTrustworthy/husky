
maxOf :: Ord a => [a] -> a
maxOf [] = error "No max of empty list"
maxOf [x] = x
--maxOf (x:xs) = if x > restMax then x else restMax
--maxOf (x:xs)
--    | x > restMax = x
--    | otherwise = restMax
--    where restMax = maxOf xs
maxOf (x:xs) = max x restMax
    where restMax = maxOf xs


replicater :: (Num a, Ord a) => a -> a -> [a]
replicater x n
    | n <= 0 = []
    | otherwise = x : replicater x (n - 1)

filterx :: (a -> Bool) -> [a] -> [a]
filterx p [] = []
filterx p (x:xs)
    | p x = x : filterx p xs
    | otherwise = filterx p xs

isEven x = x `mod` 2 == 0

reduceMe :: Num a => (a -> a -> a) -> [a] -> a -> a
reduceMe _ [] v = v
reduceMe f (x:xs) v = reduceMe f xs $ f x v

main = do
    print (filterx isEven [4,12,4,6,8,9,0,3,5,8,34,2])
    print (reduceMe (\l a -> (l + 1) * (a + 1)) [4,12,4,6,8,9,0,3,5,8,34,2] 0)