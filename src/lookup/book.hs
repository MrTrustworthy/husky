
type BookId = Int
type CustomerId = Int
type BookName = String
type Authors = [String]
type ReviewText = String

-- data Maybe a = Just a | Nothing

data PossibleBookRatings = One | Two | Three | Four | Five deriving (Show, Enum, Eq, Ord, Bounded)
type BookRating = Maybe PossibleBookRatings

data BookInfo = BookInfo {
        bookId      :: BookId,
        bookName    :: BookName,
        authors     :: Authors
    } deriving (Show)


data BookReview = BookReview {
        customerId :: CustomerId,
        bookRating :: BookRating,
        reviewText :: ReviewText
    } deriving (Show)


myInfo = BookInfo {bookId = 9780135072455, bookName = "Algebra of Programming", authors = ["Richard Bird", "Oege de Moor"]}
myReview = BookReview {customerId = 4445555, bookRating = Just Three, reviewText = "Good book"}


main = do
    print myInfo
    print myReview
    print $ "name is: " ++ (bookName myInfo)
    print $ "review rating is: " ++ (show (bookRating myReview))
