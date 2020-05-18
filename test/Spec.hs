import           Data.Xls
import           Test.Hspec

main :: IO ()
main = hspec $ describe "Sanity check" $ do
    it "Test file parsing" $ do
        content <- decodeXlsIO "test/data/test.xls"
        content `shouldBe` testFileContent

testFileContent :: [[[String]]]
testFileContent = [[["1.000000000000000","2.3","text"]],[["1.000000000000000","2.3","text"]]]
