

main = do

    putStrLn "Hello World"
    name <- getLine
    if null name
        then return()
        else do

            putStrLn $ "Hello " ++ name
            main
