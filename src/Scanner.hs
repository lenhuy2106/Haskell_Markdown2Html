module Scanner (scan) where

import           Control.Applicative ((<$>))
import           IR                  (Token (..))

-- der Scanner erzeugt aus einem Zeichenstrom vielleicht einen Tokenstrom
scan :: String -> Maybe [Token]

-- ist der Eingabestrom zuende, ist es die Rekursion auch
scan ""           = Just []

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

-- Text ohne die vorher erkannten Zeichen
scan str          =
    let (text, rest) = span (`notElem` "# \n") str
    in (T_Text text : ) <$> scan rest
