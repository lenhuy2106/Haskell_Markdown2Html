module Parser ( parse {- nur parse exportieren -} )
    where

import           Control.Applicative ((<$>), (<*>))
import           IR

-- Der Parser versucht aus einer Liste von Token einen AST zu erzeugen
parse :: [Token] -> Maybe AST

-- Die leere Liste ergibt eine leere Sequenz
parse [] = Just $ Sequence []

-- Zwei Zeilenumbrüche hintereinander sind eine leere Zeile, die in eine Sequenz
-- eingefügt wird (TODO: in Zukunft nicht immer, z.B. nicht in einem Codeblock!)
parse (T_Newline:T_Newline:xs) =
        (\(Sequence ast) -> Sequence (Emptyline : ast))
        <$> parse xs

-- einen einzelnen Zeilenumbruch ignorieren wir (TODO: aber nicht mehr bei
-- z.B. Code Blocks!)
parse (T_Newline:xs) =
        addP (Text "\n") <$> parse xs

-- einem Header muss ein Text etc. bis zum Zeilenende folgen.
-- Das ergibt zusammen einen Header im AST, er wird einer Sequenz hinzugefügt.
parse (T_H i : xs) =
    let (content, rest) = span (/= T_Newline) xs
    in case content of
      -- Zwischen den ### und dem Content muss mindestens ein Leerzeichen
      -- stehen
      (T_Blanks _ : content') ->
        (\(Sequence ast) headerAst -> Sequence (H i (unP headerAst) : ast))
         <$> parse rest
         <*> parse content'
      -- kein Leerzeichen == kein Header
      _ -> addP (Text (replicate i '#')) <$> parse xs

-- Text
parse (T_Text str : xs)  = addP (Text str) <$> parse xs

-- Blanks werden hier wieder durch Leerzeichen ersetzt
parse (T_Blanks i : xs)  = addP (Text (replicate i ' ')) <$> parse xs

parse tokens = error $ show tokens

-- Hilfsfunktionen für den Parser

-- Standardmäßig werden Text, Leerzeichen, etc. in einem P gesammelt
-- in einem Header ist aber kein P, daher wird der P-Knoten hier wieder
-- entfernt.
unP :: AST -> AST
unP (Sequence [P asts]) = Sequence asts
unP (Sequence (P ast : asts )) = Sequence (Sequence ast : asts)
unP ast = ast

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
