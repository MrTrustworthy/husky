module Book (
    randomInt
) where

import System.IO
import System.Random

randomInt :: (RandomGen g) => Int -> g -> Int
randomInt v gen = 
    let (num, _) = randomR (0, v) gen
    in num
--head $ randoms (mkStdGen 10)
--    let (number, nextGen) = random (mkStdGen 5)
--    in number

readText = do
    withFile "resources/sample.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        return contents)

main = do
    text <- readFile "resources/sample.txt"
    putStrLn text
