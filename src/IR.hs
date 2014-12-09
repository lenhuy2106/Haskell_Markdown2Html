module IR where

data Token = T_Newline          -- '\n'
           | T_H Int            -- ein Header mit der Anzahl der Hashes
           | T_Text String      -- Text
           | T_Blanks Int       -- Blanks mit Anzahl
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
           | T_HorizontalLine Int Char  -- Blanks mit Anzahl
           | T_HardLineBreak String   -- Hard Line Break
           | T_IndCodeBlock     -- Eingerückter Code Block
           | T_MaybeCS Int [Token]
           | T_CodeSpan
           | T_MaybeStarEM      -- Emphasis mit tag
           | T_MaybeStarST
           | T_MaybeLineEM
           | T_MaybeLineST
           | T_EM
           | T_ST
           | T_End
    deriving (Show, Eq)

-- Abstract Syntax Tree für HTML-Generierung. Daher schon nahe an HTML angelehnt.
data AST = Sequence [AST]   -- eine Sequenz von HTML-Elementen
         | H Int AST        -- eine Überschrift, der Int ist das Level (6 für H6)
                            -- und der AST repräsentiert den Inhalt
         | P [AST]          -- ein Absatz mit dem Inhalt
         | Text String      -- einfach nur Text
         | Link String -- einfach nur Link
         | LinkTitle String
         | Image String
         | ImageTitle String
         | Emptyline        -- eine leere Zeile
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
         | HorizontalLine   -- eine Horizontale Trennlinie
         | HardLineBreak    -- Hard Line Break
         | CB [AST]         -- Code Block
         | CS [AST]         -- Code Span
         | EM [AST]         -- Emphasis
         | ST [AST]      -- Strong Emphasis
         | Empty
    deriving (Eq, Show)

data LinkPart = String