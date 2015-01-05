module Scanner (scan) where

import           Control.Applicative ((<$>))
import           IR                  (Token (..))
import           Data.Char

-- der Scanner erzeugt aus einem Zeichenstrom vielleicht einen Tokenstrom
scan :: String -> Maybe [Token]

-- ist der Eingabestrom zuende, ist es die Rekursion auch
scan ""           = Just (T_End : []) -- ADDED END TOKEN

---------HARDLINEBREAK----------------------------------------------------------------------------------------------------------------------------------------------------------------------

scan (' ' : ' ' : xs) =
    if xs /= []
        then
            let (spaces, rest)  = span (==' ') xs                               -- more than two spaces
                first           = head rest
            in if first == '\n'                                                 -- has to be a line break
                    then ((T_HardLineBreak "") : ) <$> scan rest
                    else ( T_Blanks ((length spaces) + 2) : ) <$> scan rest
        else
            scan []

scan ('\\' : '\n' : xs) =
    ((T_HardLineBreak "\\"): ) <$> scan xs                                      -- backspaces are the same

---------HORIZONTAL LINE----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Sterne die eine HorizontalLine erzeugen
scan str@('*' : '*' : '*' : '*' : xs) =
    if xs /= []
        then
            let (stars, rest) = span (=='*') str
                count = length stars
                (blanks, rest2) = span (==' ') rest
                first = head (rest2++[' '])                                     -- no empty list prelude error
            in (if count > 3
                   then if first == '\n'
                        then ( T_HorizontalLine count '*' : )
                        else ( T_Text stars : )
                   else ( T_Text stars : ))
                <$> scan rest
        else scan []

-- linien die eine HorizontalLine erzeugen
scan str@('-' : '-' : '-' : '-' : xs) =
    if xs /= []
        then
            let (stars, rest) = span (=='-') str
                count = length stars
                (blanks, rest2) = span (==' ') rest
                first = head (rest2++[' '])
            in (if count > 3
                   then if first == '\n'
                        then ( T_HorizontalLine count '-' : )
                        else ( T_Text stars : )
                   else ( T_Text stars : ))
                <$> scan rest
        else scan []

-- unterstriche die eine HorizontalLine erzeugen
scan str@('_' : '_' : '_' : '_' : xs) =
    if xs /= []
        then
            let (stars, rest) = span (=='_') str
                count = length stars
                (blanks, rest2) = span (==' ') rest
                first = head (rest2++[' '])
            in (if count > 3
                   then if first == '\n'
                        then ( T_HorizontalLine count '_' : )
                        else ( T_Text stars : )
                   else ( T_Text stars : ))
                <$> scan rest
        else scan []

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
           n = length chunk - 4                                 -- without the first 4
           in (T_Newline : )
           <$> (T_IndCodeBlock : )
           <$> (T_Blanks n : )                                  -- add additional spaces
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

scan  ('_': '_' : '_' : '_' : xs) =                                       -- no emphasis
    (T_Text "____" : )
    <$> scan xs

scan ('_': '_' : xs) =
    (T_MaybeLineST : )
    <$> scan xs

scan ('_' : xs) =
    (T_MaybeLineEM : )
    <$> scan xs

---------LIST ITEMS----------------------------------------------------------------------------------------------------------------------------------------------------------------------

scan ('\n': ' ' : ' ' : ' ' : xs) =
    scanListItems 3 xs -- count prefix blanks
scan ('\n': ' ' : ' ' : xs) =
    scanListItems 2 xs
scan ('\n': ' ' : xs) =
    scanListItems 1 xs

scan ('\n': '-' : xs) =
    scanListItems 0 ('-':xs)
scan ('\n': '+' : xs) =
    scanListItems 0 ('+':xs)
scan ('\n': '*' : xs) =
    scanListItems 0 ('*':xs)
scan ('\n': '0' : xs) =
    scanListItems 0 ('0':xs)
scan ('\n': '1' : xs) =
    scanListItems 0 ('1':xs)
scan ('\n': '2' : xs) =
    scanListItems 0 ('2':xs)
scan ('\n': '3' : xs) =
    scanListItems 0 ('3':xs)
scan ('\n': '4' : xs) =
    scanListItems 0 ('4':xs)
scan ('\n': '5' : xs) =
    scanListItems 0 ('5':xs)
scan ('\n': '6' : xs) =
    scanListItems 0 ('6':xs)
scan ('\n': '7' : xs) =
    scanListItems 0 ('7':xs)
scan ('\n': '8' : xs) =
    scanListItems 0 ('8':xs)
scan ('\n': '9' : xs) =
    scanListItems 0 ('9':xs)

--------NEWLINE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- ein newLine escape
-- matched in scanListItems ?
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

---------HELP----------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------METHODS----------------------------------------------------------------------------------------------------------------------------------------------------------------------

scanListItems :: Int -> String -> Maybe [Token]
-- scanListItems prefixB [] =
--    (T_Newline : ) <$> scan []
scanListItems prefixB xs =
    let (spaces, rest) = span (`elem` ".) ") (tail xs)
        suffixB = length spaces
        first = head xs
        second  = xs !! 1
    in if (`elem` "-+*") first
            -- bullet list
            then ((T_ListItemBullet (prefixB + 1 + suffixB) first) : ) <$> scan rest
            -- ordered list
            else if (((`elem` "0123456789") first) && ((`elem` ".)") second))
                then ((T_ListItemOrder (prefixB + 1 + suffixB) (digitToInt first) second) : ) <$> scan rest
                else (T_Newline : ) <$> scan xs
