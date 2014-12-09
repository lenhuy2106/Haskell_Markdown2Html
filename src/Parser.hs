module Parser ( parse {- nur parse exportieren -} )
    where

import           Control.Applicative ((<$>), (<*>))
import           IR
import           Text.Regex

-- TODO: zusätzl newline am anfang parsen
-- error $ show

-- Der Parser versucht aus einer Liste von Token einen AST zu erzeugen
parse :: [Token] -> Maybe AST

-- Die leere Liste ergibt eine leere Sequenz
parse [] = Just $ Sequence []

-- Zwei Zeilenumbrüche hintereinander sind eine leere Zeile, die in eine Sequenz
-- eingefügt wird (TODO: in Zukunft nicht immer, z.B. nicht in einem Codeblock!)
parse (T_Newline : T_Newline : xs) =
        (\(Sequence ast) -> Sequence (Emptyline : ast))
        <$> parse (T_Newline : xs)

---------BLANK LINES AT END-------------------------------------------------------------------------------------------------------------------------------------------------------------

parse (T_Newline : T_Blanks b : []) =
    parse []

parse (T_End : []) =
    parse []

---------HARDLINEBREAK----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Hardlinebreaks are ignored if empty predecessor
parse (T_Newline : T_HardLineBreak ch : xs) =
        parse xs

parse (T_HardLineBreak ch: T_Blanks b : xs) =
    parse (T_HardLineBreak ch : xs)

parse (T_HardLineBreak ch : xs) =
        addP (P [HardLineBreak])
        <$> parse xs

---------HORIZONTAL LINE----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Vier oder mehr Sterne werden als Token T_HorizontalLine erkannt und hier als HorizontalLine AST weitergegeben
parse (T_HorizontalLine h : xs) =
        (\(Sequence ast) -> Sequence (HorizontalLine : ast))
        <$> parse xs

---------HEADER----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- E26: ein Escapezeichen wird ignoriert und das folgende Zeichen als String gedeutet
-- parse (T_EscapeChar:xs) =
--         parse xs

-- H unterbricht kein P
parse (T_Text str : T_Newline : T_IndCodeBlock : T_Blanks b : T_H h: xs) =
    parse (T_Text str : T_Text ("\n#") : xs)

-- NewLine vor einem Header wird ignoriert
parse (T_Newline : T_H i : xs) =
    parse (T_H i : xs)

-- Blanks vor einem Header wird ignoriert
parse (T_Newline : T_Blanks b : T_H i : xs) =
    parse (T_H i : xs)

-- parse (T_Text str : T_Newline: T_Text str2 : xs) =
{-parse (T_Text str : T_Newline: T_Text str2 : xs) =
    addP (Text (str ++ "\n" ++ str2)) <$> parse xs
-}

parse (T_Newline : T_Blanks b : xs) =
    addP (Text "\n") <$> parse xs

-- einen einzelnen Zeilenumbruch ignorieren wir (TODO: aber nicht mehr bei
-- z.B. Code Blocks!)
parse (T_Newline : xs) =
    addP (Text "\n") <$> parse xs

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
        [T_End] -> (\(Sequence ast) headerAst -> Sequence (H i (unP headerAst) : ast))
            <$> parse []  -- FIXED: parse (tail rest)
            <*> modifyAst []
        -- kein Leerzeichen == kein Header
        _ -> addP (Text (replicate i '#')) <$> parse xs


---------INDENDED CODE BLOCKS----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- LLv1

-- ICB unterbricht kein P
parse (T_Text str : T_Newline : T_IndCodeBlock : xs) =
    parse (T_Text str : T_Newline : xs)

parse (T_IndCodeBlock : xs) =
    if xs /= []
        then
            let first = head xs         -- folgendes Token
                rest = tail xs
            in case first of        -- literal parsing
                T_Text str       -> addCB (Text str)
                                    <$> parse (T_IndCodeBlock : rest)
                T_Blanks n       -> addCB (Text (replicate n ' '))
                                    <$> parse (T_IndCodeBlock : rest)
                T_H n            -> addCB (Text "#")
                                    <$> parse (T_IndCodeBlock : rest)
                T_MaybeStarEM       -> addCB(Text "*")
                                    <$> parse (T_IndCodeBlock : rest)
                T_MaybeStarST       -> addCB(Text "**")
                                    <$> parse (T_IndCodeBlock : rest)
                T_MaybeLineEM       -> addCB(Text "_")
                                    <$> parse (T_IndCodeBlock : rest)
                T_MaybeLineST       -> addCB(Text "__")
                                    <$> parse (T_IndCodeBlock : rest)
                T_HardLineBreak ch  -> addCB(Text ch)
                                    <$> parse (T_IndCodeBlock : rest)
                T_HorizontalLine h  -> addCB(Text (replicate h '*'))
                                    <$> parse (T_IndCodeBlock : rest)
                T_IndCodeBlock   -> parse (T_IndCodeBlock : rest)
                -- newline: ist nach allen newlines ein gültiges ICB ende?
                T_Newline        -> let (newlinesT, otherT) = span (==T_Newline) rest
                                        firstT = head otherT
                                    in case firstT of              -- wenn gültiges ende
                                        T_Blanks b      ->   parse (T_IndCodeBlock : T_Text "\n" : otherT)
                                        T_IndCodeBlock  ->   addCB (Text "\n")
                                                             <$> parse (T_IndCodeBlock : rest)
                                        _               ->   parse rest
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
                T_Text str          ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                T_Blanks b          ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                T_Newline           ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                T_HardLineBreak ch  ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                T_MaybeStarEM       ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                T_MaybeStarST       ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                T_MaybeLineEM       ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                T_MaybeLineST       ->  parse ((T_MaybeCS n (cs ++ [x])) : xs)
                -- yes CS
                T_MaybeCS m []      -> if (n == m)
                                        then parse (T_CodeSpan : (cs ++ [x] ++ xs))
                                        else parse ((T_MaybeCS n (cs ++ [T_Text (replicate m '`')])) : xs)
                                                  --  ([T_Text (replicate n '`')] ++ cs ++ [x] ++ xs) -- not same tags
                -- no CS
                _                   ->  parse ([T_Text (replicate n '`')] ++ cs ++ [x] ++ xs)
        else parse (x:xs)

-- if yes CS
parse (T_CodeSpan : x : xs) =
    if xs /= []
        then
            case x of -- allowed tokens
                T_Text str          ->  addCS (Text str) <$> parse (T_CodeSpan : xs)
                T_Blanks b          ->  addCS (Text (replicate b ' ')) <$> parse (T_CodeSpan : xs)
                T_Newline           ->  addCS (Text ("\n")) <$> parse (T_CodeSpan : xs)
                T_H h               ->  addCS (Text (replicate h '#')) <$> parse (T_CodeSpan : xs)
                T_HardLineBreak ch  ->  addCS (Text (ch++" ")) <$> parse (T_CodeSpan : xs)
                T_MaybeStarEM       ->  addCS (Text "*") <$> parse (T_CodeSpan : xs)
                T_MaybeStarST       ->  addCS (Text "**") <$> parse (T_CodeSpan : xs)
                T_MaybeLineEM       ->  addCS (Text "_") <$> parse (T_CodeSpan : xs)
                T_MaybeLineST       ->  addCS (Text "__") <$> parse (T_CodeSpan : xs)
                _                   ->  parse xs
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

parse (T_Text str : T_MaybeLineEM : xs) =
    parse (T_Text (str++"_") : xs)

parse (T_Text str : T_MaybeLineST : xs) =
    parse (T_Text (str++"__") : xs)

parse (T_MaybeLineEM : xs) =
    case (head xs) of
        T_Blanks b  ->  parse ((T_Text "_") : xs)
        _           ->  lookahead T_MaybeLineEM [] xs [T_EM] [(T_Text "_")]

parse (T_MaybeLineST : xs) =
    case (head xs) of
        T_Blanks b  ->  parse ((T_Text "__") : xs)
        _           ->  lookahead T_MaybeLineST [] xs [T_ST] [(T_Text "__")]


-- if yes EM
parse (T_EM : x : xs) =
    if xs /= []
        then
            case x of -- allowed tokens
                T_Text str          ->      addEM (Text str) <$> parse (T_EM : xs)
                T_Blanks b          ->      addEM (Text (replicate b ' ')) <$> parse (T_EM : xs)
                T_Newline           ->      addEM (Text ("\n")) <$> parse (T_EM : xs)
                T_HardLineBreak ch  ->      addEM (EM [HardLineBreak]) <$> parse (T_EM : xs)
                T_MaybeCS n []      ->      let (em, (x:tmp)) = span (/= T_MaybeCS n []) xs -- nested
                                            in addEM (EM []) <$> parse (((T_MaybeCS n []) : em ++ [x]) ++ (T_EM : tmp))
                T_MaybeStarST       ->      addEM (Text "**") <$> parse (T_EM : xs)         -- literal
                T_MaybeLineST       ->      addEM (Text "__") <$> parse (T_EM : xs)         -- literal
                _                   ->      parse xs
        else parse xs

-- if yes ST
parse (T_ST : x : xs) =
    if xs /= []
        then
            case x of -- allowed tokens
                T_Text str          ->      addST (Text str) <$> parse (T_ST : xs)
                T_Blanks b          ->      addST (Text (replicate b ' ')) <$> parse (T_ST : xs)
                T_Newline           ->      addST (Text ("\n")) <$> parse (T_ST : xs)
                T_HardLineBreak ch  ->      addST (ST [HardLineBreak]) <$> parse (T_ST : xs)
                T_MaybeCS n []      ->      let (em, (x:tmp)) = span (/= T_MaybeCS n []) xs -- nested
                                            in addST (ST []) <$> parse (((T_MaybeCS n []) : em ++ [x]) ++ (T_ST : tmp))
                T_MaybeStarEM       ->      addST (Text "*") <$> parse (T_ST : xs)       -- literal
                T_MaybeLineEM       ->      addST (Text "_") <$> parse (T_ST : xs)       -- literal
                _                   ->      parse xs
        else parse xs

--------OTHERS-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-
parse (T_Text str : T_Blanks i : T_Text str2 : xs) =
    let regexLinkStart = mkRegex "\\[[a-zA-Z0-9./:-]*\\]\\("
        regexLinkURI1 = mkRegex "/[a-zA-Z0-9./:-]*"
        --regexLinkURI2 = mkRegex "/<[a-zA-Z0-9./:- ]*\\>)"
        regexLinkTitle = mkRegex "\"[a-zA-Z0-9./:-]*\"\\)"
    in case (matchRegexAll regexLinkStart str) of
        Nothing -> addP (P [(Text str)]) <$> parse xs
        Just (one,two,three,four) -> if one /= [] && (last one) == '!'
            then case matchRegexAll regexLinkURI1 three of
                Nothing -> addP (P [(Text str)]) <$> parse xs
                Just (one1,two2,three3,four4) -> if one1 == [] && three3 == []
                    then case (matchRegexAll regexLinkTitle str2) of
                        Nothing -> addP (P [(Text str)]) <$> parse xs
                        Just (one11,two22,three33,four44) -> if one1 == []
                            then addP (P ([Text (init one)] ++ [ImageTitle (two++two2++[' ']++two22)] ++ [Text three3])) <$> parse xs
                            else addP (P [(Text str)]) <$> parse xs
                    else addP (P [(Text str)]) <$> parse xs
            else case matchRegexAll regexLinkURI1 three of
                Nothing -> addP (P [(Text str)]) <$> parse xs
                Just (one1,two2,three3,four4) -> if one1 == [] && three3 == []
                    then case (matchRegexAll regexLinkTitle str2) of
                        Nothing -> addP (P [(Text str)]) <$> parse xs
                        Just (one11,two22,three33,four44) -> if one1 == []
                            then addP (P ([Text one] ++ [LinkTitle (two++two2++[' ']++two22)] ++ [Text three3])) <$> parse xs
                            else addP (P [(Text str)]) <$> parse xs
                    else addP (P [(Text str)]) <$> parse xs


- Text OR Link without Spaces
parse (T_Text str : xs)  =
    let regexLinkStart = mkRegex "\\[[a-zA-Z0-9./:-]*\\]\\("
        regexLinkURI1 = mkRegex "/[a-zA-Z0-9./:-]*\\)"
    in case (matchRegexAll regexLinkStart str) of
        Nothing -> addP (P [(Text str)]) <$> parse xs
        Just (one,two,three,four) -> if one /= [] && (last one) == '!'
            then case matchRegexAll regexLinkURI1 three of
                Nothing -> addP (P [(Text str)]) <$> parse xs
                Just (one1,two2,three3,four4) -> if one1 == []
                    then addP (P ([Text (init one)] ++ [Image (two++two2)] ++ [Text three3])) <$> parse xs
                    else addP (P [(Text str)]) <$> parse xs
            else case matchRegexAll regexLinkURI1 three of
                Nothing -> addP (P [(Text str)]) <$> parse xs
                Just (one1,two2,three3,four4) -> if one1 == []
                    then addP (P ([Text one] ++ [Link (two++two2)] ++ [Text three3])) <$> parse xs
                    else addP (P [(Text str)]) <$> parse xs
-}

parse (T_Text str : xs) =
    let (line,rest) = span (/= T_Newline) (T_Text str:xs)
        regexLinkTitle = mkRegex "\\[[a-zA-Z0-9./:-_ ]*\\]\\([ ]*/[a-zA-Z0-9./:-_]*[ ]*\"[a-zA-Z0-9./:-_ ]*\"\\)"
        regexLink = mkRegex "\\[[a-zA-Z0-9./:-_ ]*\\]\\([ ]*[/[a-zA-Z0-9./:-_]*]{0,1}[ ]*\\)"
        regexLinkEnd = mkRegex "[ ]*\\)"
        in if length line > 0
            then let test = tokenToString line
                     in case matchRegexAll regexLinkTitle test of
                         Nothing -> case matchRegexAll regexLink test of
                             Nothing -> addP (P [(Text (str))]) <$> parse xs
                             Just (one,two,three,four) ->
                                    let afterLink = returnEnd line
                                        in if length one == 1 && (last one) == '!'
                                            then addP (P ([Image (two)])) <$> parse (afterLink ++ rest)
                                            else if length one == 0
                                                then addP (P [Link (two)]) <$> parse (afterLink ++ rest)
                                                else addP (P [Text (init one)]) <$> parse (T_Text (drop (length (init one)) str) : xs)
                         Just (one1,two2,three3,four4) -> 
                            let afterLink = returnEnd line
                                    in if length one1 == 1 && (last one1) == '!'
                                            then addP (P ([ImageTitle (two2)])) <$> parse (afterLink ++ rest)
                                            else if length one1 == 0
                                                then addP (P [LinkTitle (two2)]) <$> parse (afterLink ++ rest)
                                                else addP (P [Text (init one1)]) <$> parse (T_Text (drop (length (init one1)) str) : xs)
{-                                    if length one1 == 0
                                        then addP (P ([LinkTitle (two2)])) <$> parse (afterLink ++ rest)
                                        else addP (P [Text (one1)]) <$> parse (T_Text (drop (length one1) str) : xs) -}
            else addP (P [Text "\n"]) <$> parse rest

-- parse (T_Text str : T_Newline : T_Text str2 : xs) =
--    parse ((T_Text (str++"\n"++str2)) : xs)

-- Removes Trailing Spaces
parse (T_Blanks i : T_Newline : xs) = parse (T_Newline : xs)

-- Blanks werden hier wieder durch Leerzeichen ersetzt
parse (T_Blanks i : xs)  = addP (P[Text (replicate i ' ')]) <$> parse xs


parse tokens = error $ show tokens


-- Hilfsfunktionen für den Parser

returnEnd :: [Token] -> [Token]
regexEnd = mkRegex "\\)"
returnEnd [] = []
returnEnd (T_Text str : xs) = let (test,rest) = span (/= ')') str
    in case rest of
        [] -> returnEnd xs
        _ -> [T_Text (tail rest)] ++ xs
returnEnd (test :rest) = returnEnd rest


tokenToString :: [Token] -> String
tokenToString [] = ""
tokenToString tmpcontent =
    let (test:xs) = tmpcontent
    in case test of
        T_Newline -> tokenToString xs
        T_H i -> (replicate i '#') ++ tokenToString xs
        T_Blanks i -> [' '] ++ tokenToString xs
        T_Text str -> str ++ tokenToString xs
        T_HorizontalLine h -> (replicate h '*') ++ tokenToString xs
        T_MaybeCS i ast-> ['`'] ++ tokenToString xs
        T_MaybeStarEM -> ['*'] ++ tokenToString xs
        T_MaybeStarST -> "**" ++ tokenToString xs
        T_MaybeLineEM -> ['-'] ++ tokenToString xs
        T_MaybeLineST -> "--" ++ tokenToString xs
        T_End -> "xxx" ++ tokenToString xs
        _ -> "!unknownCharacter!" ++ tokenToString xs

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
                T_End -> let newcontent = init tmpcontent
                             in modifyAst newcontent
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
addP (Text str) (Sequence (CS ast2 : asts)) = addP (P ((Text str) : [CS ast2])) (Sequence asts)
addP (Text str) (Sequence (Text str2 : asts)) = Sequence (P [(Text (str++str2))] : asts)
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
addEM (EM ast1) (Sequence (EM ast2 : asts)) = Sequence (EM (ast1 ++ ast2) : asts)
addEM (EM ast1) (Sequence (ST ast2 : asts)) = addEM (EM (ast1 ++ [ST ast2])) (Sequence asts)
addEM (EM ast1) (Sequence (CS ast2 : asts)) = Sequence (EM ast1 : CS ast2 : asts)
-- addEM (EM ast1) (Sequence (HardLineBreak : asts)) = Sequence (EM ast1 : HardLineBreak : asts)
addEM text@(Text _) (Sequence (EM ast2 : asts)) = Sequence (EM (text : ast2) : asts)
addEM text@(Text _) (Sequence asts) = Sequence (EM [text] : asts)
-- addEM em (Sequence asts) = unP (Sequence (em : asts)) -- unP ?
addEM em ast = error $ show em ++ "\n" ++ show ast

addST :: AST -> AST -> AST
addST (ST ast1) (Sequence (ST ast2 : asts)) = Sequence (ST (ast1 ++ ast2) : asts)
addST (ST ast1) (Sequence (EM ast2 : asts)) = addST (ST (ast1 ++ [EM ast2])) (Sequence asts)
addST (ST ast1) (Sequence (CS ast2 : asts)) = Sequence (ST ast1 : CS ast2 : asts)
addST text@(Text _) (Sequence (ST ast2 : asts)) = Sequence (ST (text : ast2) : asts)
addST text@(Text _) (Sequence asts) = Sequence (ST [text] : asts)
-- addST strng (Sequence asts) = unP (Sequence (strng : asts)) -- unP ?
addST strng ast = error $ show strng ++ "\n" ++ show ast

----------------------------

lookahead :: Token -> [Token] -> [Token] -> [Token] -> [Token] -> Maybe AST
lookahead pivot stack (x:xs) yes no
                -- yes
                | x == pivot   =  case (last stack) of -- if last Token was a Blank
                                        T_Blanks b  ->  parse (no ++ stack ++ [x] ++ xs)
                                        _           ->  case pivot of
                                                            T_MaybeLineEM   ->   case (head xs) of -- inline underline?
                                                                                    T_Text str ->   parse (no ++ stack ++ [x] ++ xs)
                                                                                    _          ->   addP (P []) <$> parse (yes ++ stack ++ [x] ++ xs)
                                                            T_MaybeLineST   ->   case (head xs) of -- inline underline?
                                                                                    T_Text str ->   parse (no ++ stack ++ [x] ++ xs)
                                                                                    _          ->   addP (P []) <$> parse (yes ++ stack ++ [x] ++ xs)
                                                            _               ->   addP (P []) <$> parse (yes ++ stack ++ [x] ++ xs)
                -- next
                | x /= a       =  lookahead pivot (stack++[x]) xs yes no -- where clause
                -- no
                | otherwise    =  parse (no ++ stack ++ [x] ++ xs)
                    -- translate pattern to value
                where a = case x of -- promitted token
                            T_End           ->  T_End
                            T_MaybeStarEM   ->  T_MaybeStarEM -- either nested or not
                            T_MaybeStarST   ->  T_MaybeStarST
                            T_MaybeLineEM   ->  T_MaybeLineEM
                            T_MaybeLineST   ->  T_MaybeLineST
                            _               ->  T_End

escapeMaybe :: Maybe AST -> AST
escapeMaybe (Just ast) = ast
escapeMaybe Nothing = Empty

escapeMaybeList :: [Maybe AST] -> [AST]
escapeMaybeList list = map escapeMaybe list