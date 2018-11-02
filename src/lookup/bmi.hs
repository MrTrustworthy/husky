
tellBmi :: (RealFloat a) => a -> a -> String
tellBmi height weight
        | bmi <= thin = "Thin ass"
        | bmi <= normal = "Normal ass"
        | bmi <= fat = "Fatty"
        | otherwise = "Omg you whale"
        where
            bmi = weight / height ^ 2
            thin = 18.5
            normal = 25.0
            fat = 30.0


main = do
    print "Enter your height and weight now"
    height <- getLine
    weight <- getLine
    print $ tellBmi (read height) (read weight)