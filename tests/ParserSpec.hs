module ParserSpec (spec) where

import           Test.Hspec

import           Data.Maybe (fromJust)
import           IR
import           Parser

spec :: Spec
spec =
    describe "The Parser" $ do

        it "parses the empty stream" $
            fromJust ( parse []) `shouldBe` Sequence []

        it "parses a newline" $
            fromJust ( parse [T_Newline])
                `shouldBe` Sequence [P [Text "\n"]]

        it "parses 2 Texts" $
            fromJust ( parse [T_Text "Hallo", T_Newline])
                `shouldBe` Sequence [P [Text "Hallo", Text "\n"]]

        it "does not parse a header without a following blank" $
            fromJust ( parse [T_H 6, T_Text "Hallo"])
                `shouldBe` Sequence [P [Text "######", Text "Hallo"]]

        it "parses a header" $
            fromJust ( parse [T_H 6, T_Blanks 1, T_Text "Hallo"
                              , T_Blanks 1, T_Text "Welt!", T_Newline])
                `shouldBe` Sequence [ H 6 (Sequence [Text "Hallo"
                                                    , Text " "
                                                    , Text "Welt!"
                                                    ]
                                          )
                                    , P [Text "\n" ]
                                    ]

        it "parses a header without a newline" $
            fromJust ( parse [T_H 6, T_Blanks 1, T_Text "Hallo"
                              , T_Blanks 1, T_Text "Welt!"])
                `shouldBe` Sequence [ H 6 (Sequence [Text "Hallo"
                                                    , Text " "
                                                    , Text "Welt!"
                                                    ]
                                          )
                                    ]

