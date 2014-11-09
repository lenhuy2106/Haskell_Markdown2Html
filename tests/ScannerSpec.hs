module ScannerSpec (spec) where

import           Data.Maybe (fromJust)
import           Test.Hspec

import           IR
import           Scanner

scanAndExtractFirst :: String -> Token
scanAndExtractFirst = head . fromJust . scan

spec :: Spec
spec =
    describe "The Scanner" $ do

        it "scans the empty String" $
            scan "" `shouldBe` Just []

        it "scans a newline" $
            scanAndExtractFirst "\n" `shouldBe` T_Newline

        it "scans a blank" $
            scanAndExtractFirst " h a l l o" `shouldBe` T_Blanks 1

        it "scans 10 blanks" $
            scanAndExtractFirst "          h a l l o"
                `shouldBe` T_Blanks 10

        it "scans H1" $
            scanAndExtractFirst "# Hallo" `shouldBe` T_H 1

        it "scans H2" $
            scanAndExtractFirst "## Hallo" `shouldBe` T_H 2

        it "scans H3" $
            scanAndExtractFirst "### Hallo" `shouldBe` T_H 3

        it "scans H4" $
            scanAndExtractFirst "#### Hallo" `shouldBe` T_H 4

        it "scans H5" $
            scanAndExtractFirst "##### Hallo" `shouldBe` T_H 5

        it "scans H6" $
            scanAndExtractFirst "###### Hallo" `shouldBe` T_H 6

        it "scans no H7 gut a Text" $
            scanAndExtractFirst "####### Hallo"
                `shouldBe` T_Text "#######"

        it "scans H4 without a following blank" $
            scanAndExtractFirst "####Hallo" `shouldBe` T_H 4

        it "scans a string" $
            scanAndExtractFirst "Hallo" `shouldBe` T_Text "Hallo"

