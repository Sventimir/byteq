import Data.Bytecode
import Data.ByteString
import Data.ByteString.Builder
import Data.Int
import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do
  describe "Test bytecode assembling." $ do
    it "parseInt64 after int64BE is identity" $
      property checkParseInt64


checkParseInt64 :: Int64 -> Bool
checkParseInt64 i = parseInt64 (toStrict . toLazyByteString $ int64BE i) == Right (i, mempty)
