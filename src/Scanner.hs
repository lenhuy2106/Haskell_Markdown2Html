module Scanner (scan) where

import           Control.Applicative ((<$>))
import           IR                  (Token (..))

-- der Scanner erzeugt aus einem Zeichenstrom vielleicht einen Tokenstrom
scan :: String -> Maybe [Token]

-- ist der Eingabestrom zuende, ist es die Rekursion auch
scan ""           = Just (T_End : []) -- ADDED END TOKEN

---------HARDLINEBREAK----------------------------------------------------------------------------------------------------------------------------------------------------------------------

scan (' ' : ' ' : xs) =
    if xs /= []
        then
            let (spaces, rest) = span (==' ') xs
                first = head rest
            in if first == '\n'
                    then ((T_HardLineBreak "") : ) <$> scan rest
                    else ( T_Blanks ((length spaces) + 2) : ) <$> scan rest
        else
            scan []

scan ('\\' : '\n' : xs) =
    ((T_HardLineBreak "\\"): ) <$> scan xs

---------HORIZONTAL LINE----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Sterne die eine HorizontalLine erzeugen
scan str@('*' : '*' : '*' : '*' : _) =
    let (stars, rest) = span (=='*') str
        count = length stars
    in (if count > 3
           then ( T_HorizontalLine count : )
           else ( T_Text stars : ))
        <$> scan rest

--------ESCAPE CHAR-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

scan ('\\' : '#' : xs) =
    let (escape, rest) = span (=='#') ('#' : xs)
    in (T_Text escape: ) <$> scan rest

scan ('\\' : '*' : xs) =
    (T_Text "*" : ) <$> scan xs

scan ('\\': xs)    =
    (T_Text "\\" : ) <$> scan xs

---------INDENDED CODE BLOCKS----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- four consecutive spaces
scan str@('\n':' ':' ':' ':' ' : _) =
    let (newline, block) = span(=='\n') str
    in let (chunk, rest) = span (==' ') block
           n = length chunk - 4
           in (T_Newline : )
           <$> (T_IndCodeBlock : )
           <$> (T_Blanks n : )  -- add additional spaces
           <$> scan rest

---------CODE SPAN-------------------------------------------------------------------------------------------------------------------------------------------------------------

scan ('`' : xs) =
    let (backticks, rest) = span(=='`') xs
        n = (length backticks) + 1
    in (T_MaybeCS n [] : )
    <$> scan rest

---------EMPHASIS-------------------------------------------------------------------------------------------------------------------------------------------------------------

scan  ('*': '*' : '*' : '*' : xs) =
    (T_Text "****" : )
    <$> scan xs

scan ('*': '*' : xs) =
    (T_MaybeStarST : )
    <$> scan xs

scan ('*' : xs) =
    (T_MaybeStarEM : )
    <$> scan xs

scan  ('_': '_' : '_' : '_' : xs) =
    (T_Text "____" : )
    <$> scan xs

scan ('_': '_' : xs) =
    (T_MaybeLineST : )
    <$> scan xs

scan ('_' : xs) =
    (T_MaybeLineEM : )
    <$> scan xs

--------NEWLINE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- ein newLine escape
scan ('\n' : xs)    = (T_Newline : ) <$> scan xs


-- eine Anzahl Leerzeichen
scan str@(' ' : _) = let (blanks, rest) = span (==' ') str
    in ( T_Blanks (length blanks) : ) <$> scan rest


-- Hashes die eine Überschrift markieren oder Text
scan str@('#' : _) =
    let (hashes, rest) = span (=='#') str
        level = length hashes
    in (if level <= 6
           then ( T_H level : )
           else ( T_Text hashes : ))
        <$> scan rest

---------OTHERS----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Text ohne die vorher erkannten Zeichen
scan str          =
    let (text, rest) = span (`notElem` "# \n \\ ` * _") str
    in (T_Text text : ) <$> scan rest