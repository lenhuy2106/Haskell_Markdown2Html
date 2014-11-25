module Scanner (scan) where

import           Control.Applicative ((<$>))
import           IR                  (Token (..))

-- der Scanner erzeugt aus einem Zeichenstrom vielleicht einen Tokenstrom
scan :: String -> Maybe [Token]

-- ist der Eingabestrom zuende, ist es die Rekursion auch
scan ""           = Just []



-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ein EscapeChar
scan ('\\':'#':xs) = let (escape, rest) = span (=='#') ('#':xs)
    in (T_Text escape: ) <$> scan rest

scan ('\\':xs)    = (T_Text "\\" : )  <$> scan xs

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- ein Zeilenumbruch
scan ('\n':xs)    = (T_Newline : ) <$> scan xs


-- eine Anzahl Leerzeichen
scan str@(' ':_) = let (blanks, rest) = span (==' ') str
    in ( T_Blanks (length blanks) : ) <$> scan rest


-- Hashes die eine Überschrift markieren oder Text
scan str@('#':_) =
    let (hashes, rest) = span (=='#') str
        level = length hashes
    in (if level <= 6
           then ( T_H level : )
           else ( T_Text hashes : ))
        <$> scan rest



-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Sterne die eine HorizontalLine erzeugen
scan str@('*':_) =
    let (stars, rest) = span (=='*') str
        count = length stars
    in (if count > 3
           then ( T_HorizontalLine : )
           else ( T_Text stars : ))
        <$> scan rest




-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Text ohne die vorher erkannten Zeichen
scan str          =
    let (text, rest) = span (`notElem` "# \n") str
    in (T_Text text : ) <$> scan rest