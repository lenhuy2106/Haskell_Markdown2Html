module Parser ( parse {- nur parse exportieren -} )
    where

import           Control.Applicative ((<$>), (<*>))
import           IR

-- TODO: zusätzl newline am anfang parsen
-- error $ show

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

parse (T_End : []) =
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

parse (T_Text str : T_Newline: T_Text str2 : xs) =
    addP (Text (str ++ "\n" ++ str2)) <$> parse xs

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

-- LLv1

-- ICB unterbricht kein P
parse (T_Text str : T_Newline : T_IndCodeBlock : xs) =
    parse (T_Text str : T_Text ("\n") : xs)

parse (T_IndCodeBlock : xs) = 
    if xs /= []
        then
            let first = head xs         -- folgendes Token
                rest = tail xs
            in case first of
                T_Text str       -> addCB (Text str)
                                    <$> parse (T_IndCodeBlock : rest)
                T_Blanks n       -> addCB (Text (replicate n ' '))
                                    <$> parse (T_IndCodeBlock : rest)
                T_IndCodeBlock   -> parse (T_IndCodeBlock : rest)
                -- newline: ist nach allen newlines ein gültiges ICB ende?
                T_Newline        -> let (newlinesT, otherT) = span (==T_Newline) rest
                                        firstT = head otherT
                                    in case firstT of              -- wenn gültiges ende
                                        T_Blanks b ->       parse (T_IndCodeBlock : T_Text "\n" : otherT)
                                        T_IndCodeBlock ->   addCB (Text "\n")
                                                            <$> parse (T_IndCodeBlock : rest)
                                        _ ->                parse rest
                _                -> parse xs
        else parse xs

 --  Sequence (ICB (ast1 ++ ast2) : asts)
 -- (newlines, rest) = span (== T_Newline) xs
 -- T_Text ("DEBUG")

 --  let (escape, rest) = span (=='#') ('#' : xs)
 --  in (T_Text escape: ) <$> scan rest

---------CODE SPAN-------------------------------------------------------------------------------------------------------------------------------------------------------------

-- LLv2

parse ((T_MaybeCS n cs) : x : xs) =
    if x:xs /= []
        then
            case x of
                -- maybe CS
                T_Text str      ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                T_Blanks b      ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                T_Newline       ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                -- yes CS
                T_MaybeCS m []  -> if (n == m)
                                        then parse (T_CodeSpan : (cs ++ [x] ++ xs))
                                        else parse ([T_Text (replicate n '`')] ++ cs ++ [x] ++ xs) -- not same tags
                -- no CS
                _               ->  parse ([T_Text (replicate n '`')] ++ cs ++ [x] ++ xs)
        else parse (x:xs)

-- if yes CS
parse (T_CodeSpan : x : xs) =
    if xs /= []
        then
            case x of
                T_Text str  ->      addCS (Text str) <$> parse (T_CodeSpan : xs)
                T_Blanks b  ->      addCS (Text (replicate b ' ')) <$> parse (T_CodeSpan : xs)
                T_Newline   ->      addCS (Text ("\n")) <$> parse (T_CodeSpan : xs)
                _           ->      parse xs
        else parse xs

---------EMPHASIS-------------------------------------------------------------------------------------------------------------------------------------------------------------

-- LLv3

-- lookahead pivot stack xs yes no
parse (T_MaybeStarEM : xs) =
    case (head xs) of -- if first token is a blank
        T_Blanks b  ->  parse ((T_Text "*") : xs)
        _           ->  lookahead T_MaybeStarEM [] xs [T_EM] [(T_Text "*")]


parse (T_MaybeStarST : xs) =
    case (head xs) of
        T_Blanks b  ->  parse ((T_Text "**") : xs)
        _           ->  lookahead T_MaybeStarST [] xs [T_ST] [(T_Text "**")]

parse (T_MaybeLineEM : xs) =
    case (head xs) of
        T_Blanks b  ->  parse ((T_Text "_") : xs)
        _           ->  lookahead T_MaybeLineEM [] xs [T_EM] [(T_Text "_")]

parse (T_MaybeLineST : xs) =
    case (head xs) of
        T_Blanks b  ->  parse ((T_Text "__") : xs)
        _           ->  lookahead T_MaybeStarEM [] xs [T_EM] [(T_Text "__")]


-- if yes EM
parse (T_EM : x : xs) =
    if xs /= []
        then
            case x of
                T_Text str  ->      addEM (Text str) <$> parse (T_EM : xs)
                T_Blanks b  ->      addEM (Text (replicate b ' ')) <$> parse (T_EM : xs)
                T_Newline   ->      addEM (Text ("\n")) <$> parse (T_EM : xs)
                _           ->      parse xs
        else parse xs

-- if yes STRNG
parse (T_ST : x : xs) =
    if xs /= []
        then
            case x of
                T_Text str  ->      addSTRNG (Text str) <$> parse (T_ST : xs)
                T_Blanks b  ->      addSTRNG (Text (replicate b ' ')) <$> parse (T_ST : xs)
                T_Newline   ->      addSTRNG (Text ("\n")) <$> parse (T_ST : xs)
                _           ->      parse xs
        else parse xs

--------OTHERS-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Text
parse (T_Text str : xs)  = addP (P [(Text str)]) <$> parse xs

-- Removes Trailing Spaces
parse (T_Blanks i : T_Newline : xs) = parse (T_Newline : xs)

-- Blanks werden hier wieder durch Leerzeichen ersetzt
parse (T_Blanks i : xs)  = addP (P[Text (replicate i ' ')]) <$> parse xs


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
-- addP (EM ast1) (Sequence (P ast2 : asts)) = error $ show ast1 ++ "\n" ++ show asts
-- CS, EM, ST kommen in ein P
addP (P ast1) (Sequence (CS ast2 : asts)) = addP (P (ast1 ++ [CS ast2])) (Sequence asts)
addP (P ast1) (Sequence (EM ast2 : asts)) = addP (P (ast1 ++ [EM ast2])) (Sequence asts)
addP (P ast1) (Sequence (ST ast2 : asts)) = addP (P (ast1 ++ [ST ast2])) (Sequence asts)

-- Text und dahinter ein P
addP text@(Text _) (Sequence (P ast2 : asts)) = Sequence (P (text : ast2) : asts)
-- Andernfalls bleibt der Absatz alleine und wird vorne in die Sequence
-- eingefügt
addP text@(Text _) (Sequence ast) = Sequence (P [text] : ast)
-- addP text@(EM em) (Sequence ast) = Sequence (P [text] : ast)
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

addCB :: AST -> AST -> AST
addCB (CB ast1) (Sequence (CB ast2 : asts)) = Sequence (CB (ast1 ++ ast2) : asts)
addCB text@(Text _) (Sequence (CB ast2 : asts)) = Sequence (CB (text : ast2) : asts)
addCB text@(Text _) (Sequence asts) = Sequence (CB [text] : asts)
addCB cb (Sequence asts) = unP (Sequence (cb : asts)) -- unP ?
addCB cb ast = error $ show cb ++ "\n" ++ show ast

addCS :: AST -> AST -> AST
addCS (CS ast1) (Sequence (CS ast2 : asts)) = Sequence (CS (ast1 ++ ast2) : asts)
addCS text@(Text _) (Sequence (CS ast2 : asts)) = Sequence (CS (text : ast2) : asts)
addCS text@(Text _) (Sequence asts) = Sequence (CS [text] : asts)
addCS cs (Sequence asts) = unP (Sequence (cs : asts)) -- unP ?
addCS cs ast = error $ show cs ++ "\n" ++ show ast

addEM :: AST -> AST -> AST
addEM (EM ast1) (Sequence (EM ast2 : asts)) = Sequence (P (EM (ast1 ++ ast2):[]) : asts)
addEM text@(Text _) (Sequence (EM ast2 : asts)) = Sequence (EM (text : ast2) : asts)
addEM text@(Text _) (Sequence asts) = Sequence (EM [text] : asts)
-- addEM em (Sequence asts) = unP (Sequence (em : asts)) -- unP ?
addEM em ast = error $ show em ++ "\n" ++ show ast

addSTRNG :: AST -> AST -> AST
addSTRNG (ST ast1) (Sequence (ST ast2 : asts)) = Sequence (ST (ast1 ++ ast2) : asts)
addSTRNG text@(Text _) (Sequence (ST ast2 : asts)) = Sequence (ST (text : ast2) : asts)
addSTRNG text@(Text _) (Sequence asts) = Sequence (ST [text] : asts)
-- addSTRNG strng (Sequence asts) = unP (Sequence (strng : asts)) -- unP ?
addSTRNG strng ast = error $ show strng ++ "\n" ++ show ast

----------------------------

lookahead :: Token -> [Token] -> [Token] -> [Token] -> [Token] -> Maybe AST
lookahead pivot stack (x:xs) yes no
                | x == a       =  lookahead pivot (stack++[x]) xs yes no -- where clause
                -- yes
                | x == pivot   =  case (last stack) of -- if last Token was a Blank
                                        T_Blanks b  ->  parse (no ++ stack ++ [x] ++ xs)
                                        _           ->  parse (yes ++ stack ++ [x] ++ xs)
                -- no
                | otherwise    =  parse (no ++ stack ++ [x] ++ xs)
                    -- translate pattern to value
                where a = case x of
                            T_Text str      ->  T_Text str
                            T_Newline       ->  T_Newline
                            T_Blanks b      ->  T_Blanks b
                            T_End           ->  T_Newline
                            _               ->  T_End
