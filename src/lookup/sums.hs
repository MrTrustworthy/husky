
main = do
    let numbers = [1..52]
    let mysum = evenSum numbers
    print ("Total is " ++ (show mysum))

evenSum :: Integral a => [a] -> a
evenSum is = accumSum 0 is
    where
        accumSum current [] = current
        -- accumSum current (1:xs) = accumSum 0 xs
        accumSum current (s:xs) = accumSum (if even s then current + s else current) xs