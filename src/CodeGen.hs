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
generateHTML' (Link str) = 
    let (link,rest) = span (/= ']') str
    in "<a href=\"" ++ tail (tail (init rest)) ++ "\">" ++ tail link ++ "</a>"
generateHTML' (LinkTitle str) = 
    let (link,rest) = span (/= ']') str
        (uri,rest2) = span (/= ' ') rest
    in "<a href=\"" ++ tail (tail uri) ++ "\" title=" ++ tail (init rest2) ++">" ++ tail link ++ "</a>"
generateHTML' (Image str) =
    let (link,rest) = span (/= ']') str
    in "<img src=\"" ++ tail (tail (init rest)) ++ "\" alt=\"" ++ tail link ++ "\"/>"
generateHTML' (ImageTitle str) = 
    let (link,rest) = span (/= ']') str
        (uri,rest2) = span (/= ' ') rest
    in "<img src=\"" ++ tail (tail uri) ++ "\" title=" ++ "\" alt=\"" ++ tail link ++ "\" title=" ++ tail (init rest2) ++" />"
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
generateHTML' (CS ast) =
    "<code>" ++ concatMap generateHTML' ast ++ "\n</code>\n"
generateHTML' (EM ast) =
    "<em>" ++ concatMap generateHTML' ast ++ "\n</em>\n"
generateHTML' (ST ast) =
    "<strong>" ++ concatMap generateHTML' ast ++ "\n</strong>\n"
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
generateHTML' _ = ""
