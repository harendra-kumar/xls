import           Data.Xls
import           Test.Hspec

main :: IO ()
main = hspec $ describe "Sanity check" $ do
    it "Test file parsing" $ do
        content <- decodeXlsIO "test/data/test.xls"
        content `shouldBe` testFileContent

testFileContent :: [[[Cell]]]
testFileContent = [
    [[NumericalCell 1.0,TextCell "2.3",TextCell "text"]],
    [[NumericalCell 1.0,TextCell "2.3",TextCell "text"]]
    ]
