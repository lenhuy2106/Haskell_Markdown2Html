module Scanner (scan) where

import           Control.Applicative ((<$>))
import           IR                  (Token (..))

-- der Scanner erzeugt aus einem Zeichenstrom vielleicht einen Tokenstrom
scan :: String -> Maybe [Token]

-- ist der Eingabestrom zuende, ist es die Rekursion auch
scan ""           = Just []



--------ESCAPE CHAR-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

scan ('\\' : '#' : xs) =
    let (escape, rest) = span (=='#') ('#' : xs)
    in (T_Text escape: ) <$> scan rest

scan ('\\':xs)    =
    (T_Text "\\" : ) <$> scan xs

---------INDENDED CODE BLOCKS----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- four consecutive spaces
scan str@('\n':' ':' ':' ':' ' : _) =
    let (newline, block) = span(=='\n') str
    in let (chunk, rest) = span (==' ') block
           n = length chunk - 4
           in (T_Newline : )
           <$> (T_IndCodeBlock : )
           <$> (T_Text (replicate n ' ') : )  -- add additional spaces
           <$> scan rest

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

---------HORIZONTAL LINE----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Sterne die eine HorizontalLine erzeugen
scan str@('*' : _) =
    let (stars, rest) = span (=='*') str
        count = length stars
    in (if count > 3
           then ( T_HorizontalLine : )
           else ( T_Text stars : ))
        <$> scan rest

---------OTHERS----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Text ohne die vorher erkannten Zeichen
scan str          =
    let (text, rest) = span (`notElem` "# \n") str
    in (T_Text text : ) <$> scan rest