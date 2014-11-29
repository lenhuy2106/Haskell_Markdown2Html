module Parser ( parse {- nur parse exportieren -} )
    where

import           Control.Applicative ((<$>), (<*>))
import           IR

-- TODO: zusätzl newline am anfang parsen


-- Der Parser versucht aus einer Liste von Token einen AST zu erzeugen
parse :: [Token] -> Maybe AST

-- Die leere Liste ergibt eine leere Sequenz
parse [] = Just $ Sequence []

-- Zwei Zeilenumbrüche hintereinander sind eine leere Zeile, die in eine Sequenz
-- eingefügt wird (TODO: in Zukunft nicht immer, z.B. nicht in einem Codeblock!)
parse (T_Newline:T_Newline:xs) =
        (\(Sequence ast) -> Sequence (Emptyline : ast))
        <$> parse xs

    
---------BLANK LINES AT END-------------------------------------------------------------------------------------------------------------------------------------------------------------

parse (T_Newline : T_Blanks b : []) =
    parse []
    
---------HORIZONTAL LINE----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Vier oder mehr Sterne werden als Token T_HorizontalLine erkannt und hier als HorizontalLine AST weitergegeben
parse (T_HorizontalLine:xs) =
        (\(Sequence ast) -> Sequence (HorizontalLine : ast))
        <$> parse xs

---------HEADER----------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- E26: ein Escapezeichen wird ignoriert und das folgende Zeichen als String gedeutet
-- parse (T_EscapeChar:xs) =
--         parse xs


-- NewLine vor einem Header wird ignoriert
parse (T_Newline : T_H i : xs) =
    parse (T_H i : xs)

-- einen einzelnen Zeilenumbruch ignorieren wir (TODO: aber nicht mehr bei
-- z.B. Code Blocks!)
parse (T_Newline:xs) =
        parse xs -- addP (Text "\n") <$> parse xs

-- einem Header muss ein Text etc. bis zum Zeilenende folgen.
-- Das ergibt zusammen einen Header im AST, er wird einer Sequenz hinzugefügt.
parse (T_H i : xs) =
    let (content, rest) = span (/= T_Newline) xs -- Span splittet vor! Newline, Weitergabe muss ohne geschehen. siehe unten
        neof = length rest > 0
    in case content of
        [] -> case neof of
                True -> (\(Sequence ast) headerAst -> Sequence (H i (unP headerAst) : ast))
                    <$> parse (tail rest)
                    <*> modifyAst []
                False -> (\(Sequence ast) headerAst -> Sequence (H i (unP headerAst) : ast))
                    <$> parse []
                    <*> modifyAst []
      -- Zwischen den ### und dem Content muss mindestens ein Leerzeichen
      -- stehen
        (T_Blanks _ : content') ->
            (\(Sequence ast) headerAst -> Sequence (H i (unP headerAst) : ast))
            <$> parse rest  -- FIXED: parse (tail rest)
            <*> modifyAst content'
        -- kein Leerzeichen == kein Header
        _ -> addP (Text (replicate i '#')) <$> parse xs


---------INDENDED CODE BLOCKS----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- ICB unterbricht kein P
parse (T_Text str : T_Newline : T_IndCodeBlock : xs) =
    parse (T_Text str : T_Text ("\n") : xs)

parse (T_IndCodeBlock : xs) = 
    if xs /= []
        then
            let first = head xs         -- folgendes Token
                rest = tail xs
            in case first of
                T_Text str       -> addICB (Text str)
                                    <$> parse (T_IndCodeBlock : rest)
                T_Blanks n       -> addICB (Text (replicate n ' '))
                                    <$> parse (T_IndCodeBlock : rest)
                T_IndCodeBlock   -> parse (T_IndCodeBlock : rest)
                -- newline: ist nach allen newlines ein gültiges ICB ende?
                T_Newline        -> let (newlinesT, otherT) = span (==T_Newline) rest
                                        firstT = head otherT
                                    in case firstT of              -- wenn gültiges ende
                                        T_Blanks b ->       parse (T_IndCodeBlock : T_Text "\n" : otherT)
                                        T_IndCodeBlock ->   addICB (Text "\n")
                                                            <$> parse (T_IndCodeBlock : rest)
                                        _ ->                parse rest
                _                -> parse xs
        else parse xs

 --  Sequence (ICB (ast1 ++ ast2) : asts)
 -- (newlines, rest) = span (== T_Newline) xs
 -- T_Text ("DEBUG")

 --  let (escape, rest) = span (=='#') ('#' : xs)
 --  in (T_Text escape: ) <$> scan rest


--------OTHERS-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Text
parse (T_Text str : xs)  = addP (Text str) <$> parse xs

-- Removes Trailing Spaces
parse (T_Blanks i : T_Newline : xs) = parse (T_Newline : xs)

-- Blanks werden hier wieder durch Leerzeichen ersetzt
parse (T_Blanks i : xs)  = addP (Text (replicate i ' ')) <$> parse xs


parse tokens = error $ show tokens


-- Hilfsfunktionen für den Parser

-- check End
modifyAst :: [Token] -> Maybe AST
modifyAst [] = parse []
modifyAst tmpcontent = 
    if length tmpcontent > 1
        then
            let lastT = last tmpcontent
                sndLastT = tmpcontent !! ((length tmpcontent) - 2)
            in case lastT of
                T_H i -> case sndLastT of
                    T_Blanks _ -> parse (replaceT (init (init tmpcontent)))
                    T_Text _ -> parse (init (replaceT tmpcontent) ++ [T_Text (replicate i '#')])
                    _ -> parse $ init (replaceT tmpcontent)
                T_Blanks _ -> modifyAst (init tmpcontent)
                _ -> parse $ replaceT tmpcontent
        else parse tmpcontent


replaceT :: [Token] -> [Token]
replaceT [] = []
replaceT tmpcontent =
    let (test:xs) = tmpcontent
    in case test of
        T_H i -> [T_Text (replicate i '#')] ++ replaceT (tail tmpcontent)
        T_Text str -> [test] ++ replaceT (tail tmpcontent)
        _ -> [test] ++ replaceT (tail tmpcontent)

-- Mehrere aufeinander folgende Texte, Blanks, etc. werden zu einem Absatz
-- zusammengefügt.
addP :: AST -> AST -> AST

-- Wenn wir zwei Absätze hintereinander finden, fassen wir diese zusammen
addP (P ast1) (Sequence (P ast2 : asts)) = Sequence (P (ast1 ++ ast2) : asts)
-- Text und dahinter ein P
addP text@(Text _) (Sequence (P ast2 : asts)) = Sequence (P (text : ast2) : asts)
-- Andernfalls bleibt der Absatz alleine und wird vorne in die Sequence
-- eingefügt
addP text@(Text _) (Sequence ast) = Sequence (P [text] : ast)
addP p (Sequence ast) = Sequence (p : ast)
addP p ast = error $ show p ++ "\n" ++ show ast


-- Standardmäßig werden Text, Leerzeichen, etc. in einem P gesammelt
-- in einem Header ist aber kein P, daher wird der P-Knoten hier wieder
-- entfernt.
unP :: AST -> AST
unP (Sequence [P asts]) = Sequence asts
unP (Sequence (P ast : asts )) = Sequence (Sequence ast : asts)
unP ast = ast

--------------------------

addICB :: AST -> AST -> AST

addICB (CB ast1) (Sequence (CB ast2 : asts)) = Sequence (CB (ast1 ++ ast2) : asts)
addICB text@(Text _) (Sequence (CB ast2 : asts)) = Sequence (CB (text : ast2) : asts)
addICB text@(Text _) (Sequence asts) = Sequence (CB [text] : asts)
addICB icb (Sequence asts) = unP (Sequence (icb : asts)) -- unP ?
addICB icb ast = error $ show icb ++ "\n" ++ show ast

