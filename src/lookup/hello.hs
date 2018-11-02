main = do
    print "What's ya name?"
    name <- getLine
    printname name

printname :: [Char] -> IO()
printname name = print ("Hi " ++ name ++ "!")