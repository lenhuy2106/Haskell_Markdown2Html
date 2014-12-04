module CodeGen where

import           IR

htmlHead, htmlFooter :: String
htmlHead = "<!DOCTYPE html>\n\
 \<html>\n\
 \ <head>\n\
 \  <title>\n\
 \   Generated by md2html\n\
 \  </title>\n\
 \ </head>\n\
 \ <body>\n"
htmlFooter = "</body>\n</html>"

generateHTML :: AST -> String
generateHTML ast = htmlHead ++ generateHTML' ast ++ htmlFooter

generateHTML' :: AST -> String
generateHTML' (Text str) = str
generateHTML' (P ast)  =
    "<p>" ++ concatMap generateHTML' ast ++ "</p>\n"
generateHTML' (H i ast) =
    "<h" ++ show i ++ ">" ++ generateHTML' ast ++ "</h" ++ show i ++ ">\n"
generateHTML' (Sequence as) =
    concatMap generateHTML' as
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
generateHTML' HorizontalLine = "<hr />"
generateHTML' (CB ast) =
    "<pre><code>" ++ concatMap generateHTML' ast ++ "\n</code></pre>\n"
generateHTML' (EM ast) =
    "<em>" ++ concatMap generateHTML' ast ++ "\n</em>\n"
generateHTML' (STRNG ast) =
    "<strong>" ++ concatMap generateHTML' ast ++ "\n</strong>\n"
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
generateHTML' _ = ""
