import Test.Hspec
import Lib
import Problems
import Book
import System.Random

main :: IO ()
main = hspec $ do
    describe "Lib" $ do
        it "Smoke test" $ do
            (4 + 5) `shouldBe` (9 :: Int)
    describe "Problems" $ do
        it "Last of" $ do
            lastOf ['a'..'z'] `shouldBe` 'z'
        it "Second Last" $ do
            secondLastOf ['a'..'z'] `shouldBe` 'y'
        it "Element at" $ do
            elementAt ['a'..'z'] 3 `shouldBe` 'c'
        it "Length of" $ do
            lengthOf ['a'..'z'] `shouldBe` 26
        it "Palindrome" $ do
            isPalindrome [1,2,4,8,16,8,4,2,1] `shouldBe` True
        it "Palindrome False" $ do
            isPalindrome [1,2,3,4,3,3,2,1] `shouldBe` False
        it "Flattened" $ do
            flattenList2 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]
        it "Compressed" $ do
            compress "aaaabccaadeeee" `shouldBe` "abcde"
        it "Compressed consecutive" $ do
            compressConsecutive "aaaabccaaaaadeeee" `shouldBe` "abcade"
        it "Packed" $ do
            pack "aaaabccaaaaadeeee" `shouldBe` ["aaaa","b","cc","aaaaa","d","eeee"]
        it "RunLength" $ do
            runLength "aaaabccaaaaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(5,'a'),(1,'d'),(4,'e')]
        it "RunLength with data type" $ do
            runLengthS "aaaabccaaaaadeeee" `shouldBe` [Multiple (4,'a'),Single 'b',Multiple (2,'c'),Multiple (5,'a'),Single 'd',Multiple (4,'e')]
    describe "Book" $ do
        it "Reads file" $ do
            True `shouldBe` True
        it "Random value" $ do
            gen <- getStdGen
            (randomInt 5 gen) `shouldSatisfy` (\x -> x `elem` [0..5])
        it "Throw error" $ do
            readFile "asdff43fasdfasc23d" `shouldThrow` anyException



