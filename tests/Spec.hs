import Test.Hspec

import qualified ScannerSpec
import qualified ParserSpec
import qualified CodeGenSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Scanner"     ScannerSpec.spec
  describe "Parser"      ParserSpec.spec
  describe "CodeGen"     CodeGenSpec.spec