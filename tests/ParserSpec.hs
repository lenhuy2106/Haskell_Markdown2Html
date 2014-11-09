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

        it "parses 2 Newlines" $
            fromJust ( parse [T_Newline, T_Newline])
                `shouldBe` Sequence [Emptyline]

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

        it "parses a header with two newlines" $
            fromJust ( parse [T_H 6, T_Blanks 1, T_Text "Hallo"
                              , T_Blanks 1, T_Text "Welt!"
                              ,T_Newline, T_Newline])
                `shouldBe` Sequence [ H 6 (Sequence [Text "Hallo"
                                                    , Text " "
                                                    , Text "Welt!"]
                                          )
                                    , Emptyline
                                    ]

        it "parses two Paragraphs" $
            fromJust ( parse [ T_Text "Hallo", T_Newline, T_Text "Welt"
                              , T_Blanks 3, T_Newline, T_Newline
                              , T_Text "Huhu", T_Blanks 1, T_Text "World"
                              , T_Newline])
                `shouldBe` Sequence [ P [Text "Hallo", Text "\n"
                                        , Text "Welt", Text "   "
                                        ]
                                    , Emptyline
                                    , P [Text "Huhu", Text " "
                                        , Text "World", Text "\n"
                                        ]
                                    ]

