 module Shapes (Point(..), Shape(..), basePoint, baseCircle, surface, distance) where

type Coord = Float

data Bulean = Wahr | Falsch

data Point = Point {
        x :: Coord,
        y :: Coord
    } deriving (Show, Eq)

instance Ord Point where
    a <= b = (x a) * (y a) <= (x b) * (y b)

data Shape = Circle Point Coord | Rectangle Point Point deriving (Show, Eq)

basePoint :: Point
basePoint = Point 0.0 0.0

baseCircle :: Coord -> Shape
baseCircle = Circle basePoint

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle p1 p2) = (abs $ (x p1) - (x p2)) * (abs $ (y p1) - (y p2))

distance :: Point -> Point -> Float
distance p1 p2 = sqrt $ ((x p2) - (x p1)) + ((y p2) - (y p1))

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Point where
    yesno (Point 0.0 0.0) = False  -- why can't I use basePoint here?
    yesno _  = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf val resTrue resFalse = if yesno val then resTrue else resFalse

main = do
    print $ surface (baseCircle 10)
    print $ Rectangle (Point 1.0 2.0) (Point 4.0 5.0)
    print $ surface $ Rectangle (Point 1.0 2.0) (Point 4.0 5.0)
    print $ distance (Point 1.0 1.0) (Point 2.0 2.0)
    print $ (Point 30.0 10.0) < (Point 20.0 20.0)
    print $ yesno (1 :: Int)
    print $ yesnoIf basePoint "Yippieh" "Nada"
    print $ yesnoIf (Point 20.0 20.0) "Yippieh" "Nada"