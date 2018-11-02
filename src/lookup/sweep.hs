
data Maybe a = Nothing | Just a

toList :: String -> [Integer]
toList input = read ("[" ++ input ++ "]")

maybeGetLine s = case reads s of
    [(x,"")]    -> Main.Just x
    _           -> Main.Nothing

main = do
    putStrLn "Enter input!"
    input <- getLine
    print $ sum (toList input)
