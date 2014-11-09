module CodeGenSpec (spec) where

import           Test.Hspec

import           CodeGen
import           IR

spec :: Spec
spec =
    describe "The code generator" $ do
        it "generates HTML for the empty AST" $
            generateHTML (Sequence []) `shouldBe` (htmlHead ++ htmlFooter)

        it "generates HTML for a text" $
            generateHTML' (Text "der Text") `shouldBe` "der Text"

        it "generates HTML for a paragraph consisting of one element" $
            generateHTML' (P [Text "der Text"])
                `shouldBe` "<p>der Text</p>\n"

        it "generates HTML for a paragraph consisting of three elements" $
            generateHTML' (P [Text "der", Text " ", Text "Text"])
                `shouldBe` "<p>der Text</p>\n"

        it "generates HTML for a header" $
            generateHTML' (H 1 $ Sequence [Text "der", Text " "
                                         , Text "Header"])
                `shouldBe` "<h1>der Header</h1>\n"

        it "generate Code for a sequence" $
            generateHTML' (Sequence [ H 1 (Sequence [Text "der"
                                                    , Text " "
                                                    , Text "Header"
                                                    ]
                                          )
                                    , P [Text "Hallo"]
                                    , P [Text "Welt!"]
                                    ])
                `shouldBe`
                    "<h1>der Header</h1>\n<p>Hallo</p>\n<p>Welt!</p>\n"
