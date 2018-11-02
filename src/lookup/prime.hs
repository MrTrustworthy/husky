

isPrime n = checkPrime [2..n-1] n
    where
        checkPrime [] val = True
        checkPrime (x:xs) val = if ((val `mod` x) == 0) then False else checkPrime xs val

main = do
    let number = 13
    print (isPrime number)