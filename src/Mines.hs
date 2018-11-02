module Mines (

) where

import qualified Data.List as L
import qualified Control.Monad as M
import qualified System.Random as R

type FieldHidden = Bool

-- Field position

data FieldPosition = FieldPosition {
    x :: Int,
    y :: Int 
} deriving (Eq)

instance Show FieldPosition where
    show (FieldPosition x y) = "(" ++ (show x) ++ ":" ++ (show y) ++ ")"

-- Field contents

data FieldContent = IsBomb | HasSurroundingBombs (Maybe Int) deriving (Eq)

instance Show FieldContent where
    show (IsBomb)                       = "X"
    show (HasSurroundingBombs Nothing)  = "O"
    show (HasSurroundingBombs (Just x)) = show x

-- Field related stuff

data Field = Field {
    contents    :: FieldContent,
    position    :: FieldPosition,
    hidden      :: FieldHidden
} deriving (Eq)

instance Show Field where
    show (Field c p h)
        | h         = "[" ++ show p ++ "/?]"
        | otherwise     = "[" ++ show p ++ "/" ++ show c ++ "]"

areNeighboringFields :: Field -> Field -> Bool
areNeighboringFields a b = areNextTo (position a) (position b)
    where
        areNextTo (FieldPosition ax ay) (FieldPosition bx by) =
            let 
                dx = ax - bx
                dy = ay - by
                viable = [1, 0, -1]
            in 
                (dx `elem` viable) && (dy `elem` viable) && not (dx == 0 && dy == 0)

-- Field map and related factories

type FieldMap = [Field]

createFields :: R.RandomGen r => Int -> Int -> r -> FieldMap
createFields v b gen = map createField $ zip (permuteFieldPositions v) (randomBombOneInN b gen)
    where
        createField (pos, c)    = Field c pos True
        permuteFieldPositions v = [ (FieldPosition x y) | x <- [1..v], y <- [1..v] ]
        randomBombOneInN n gen  = map (\b -> if b then IsBomb else HasSurroundingBombs Nothing) (randomOneInN n gen)

updateWithSurroundingAmounts :: FieldMap -> FieldMap
updateWithSurroundingAmounts m = map (\f -> updateBombAmount f m) m
    where
        updateBombAmount f m 
            | (contents f) == IsBomb    = f
            | otherwise                 = f { contents = HasSurroundingBombs (countOfSurroundingBombs f m)}

countOfSurroundingBombs :: Field -> FieldMap -> (Maybe Int)
countOfSurroundingBombs f m = 
    let 
        neighbors       = filter (areNeighboringFields f) m
        bombNeighbors   = filter (\x -> (contents x) == IsBomb) neighbors
        bombAmount      = length bombNeighbors
    in 
        if bombAmount /= 0 then Just bombAmount else Nothing

getFieldMapRepresentation :: FieldMap -> String
getFieldMapRepresentation m = accumRepr m 1 ""
    where
        accumRepr [] _ v                = v
        accumRepr (f:fs) row v 
            | (x (position f)) == row   = v ++ (show f) ++ (accumRepr fs row v)
            | otherwise                 = v ++ "\n" ++ (show f) ++ (accumRepr fs (row + 1) v)

-- Gameplay

uncoverField :: FieldPosition -> FieldMap -> FieldMap
uncoverField pos m = map (\f -> if (position f) == pos then f { hidden = False } else f) m


handleInput :: Int -> Int -> FieldMap -> (Either String FieldMap)
handleInput x y m = 
    let
        pos             = FieldPosition x y
        field           = head $ filter (\f -> (position f) == pos) m
        uncoveredMap    = uncoverField pos m
    in 
       if (contents field) == IsBomb then Left "Triggered a bomb!" else Right uncoveredMap
    

-- Utils

randomOneInN :: R.RandomGen r => Int -> r -> [Bool]
randomOneInN 0 _            = repeat False
randomOneInN n gen          = map (== 0) (randomDice n gen)
    where
        randomDice n gen    = R.randomRs (0, n) gen

-- IO

main :: IO ()
main = do
    gen <- R.getStdGen
    let initialMap = updateWithSurroundingAmounts $ createFields 9 6 gen
    s <- gameLoop initialMap
    putStr s

gameLoop :: FieldMap -> IO (String)
gameLoop fieldMap = do
    putStrLn $ getFieldMapRepresentation fieldMap
    putStrLn "Select tile to uncover"
    x <- fmap read getLine :: IO Int
    y <- fmap read getLine :: IO Int
    putStrLn $ "Uncovering " ++ (show x) ++ ":" ++ (show y)
    case handleInput x y fieldMap of
        (Right map) -> gameLoop map
        (Left s) -> return (s) 