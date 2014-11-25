module IR where

data Token = T_Newline          -- '\n'
           | T_H Int            -- ein Header mit der Anzahl der Hashes
           | T_Text String      -- Text
           | T_Blanks Int       -- Blanks mit Anzahl
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
           | T_HorizontalLine   -- Blanks mit Anzahl
           | T_EscapeChar       -- Entwerter
           | T_Char Char        -- Einzelner Character
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    deriving (Show, Eq)

-- Abstract Syntax Tree für HTML-Generierung. Daher schon nahe an HTML angelehnt.
data AST = Sequence [AST]   -- eine Sequenz von HTML-Elementen
         | H Int AST        -- eine Überschrift, der Int ist das Level (6 für H6)
                            -- und der AST repräsentiert den Inhalt
         | P [AST]          -- ein Absatz mit dem Inhalt
         | Text String      -- einfach nur Text
         | Emptyline        -- eine leere Zeile
         | HorizontalLine   -- eine Horizontale Trennlinie
    deriving (Eq, Show)
