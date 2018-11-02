

data Point = Point Int Int deriving (Eq, Show)

main = do
    let p1 = Point 1 1
    let p2 = Point 3 4
    let dist = distance p1 p2
    print ("distance is ", dist)

distance :: Point -> Point -> Int
distance p1 p2 = 5