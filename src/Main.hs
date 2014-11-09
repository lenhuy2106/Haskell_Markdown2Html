module Main where

import           CodeGen
import           IR
import           Parser
import           Scanner

main :: IO ()
main = do
    -- lese den Inhalt der Datei "test.md" als einen kompletten String ein
    input <- readFile "test.md"
    -- versuche den String zu scannen
    let maybeTokens = scan input
    putStrLn "Scanner output\n=============="
    print maybeTokens
    -- the parse
    case maybeTokens of
        Nothing -> putStrLn "scanner failed"
        Just tokens -> do -- der Scanner war erfolgreich
            -- versuche die Tokens zu parsen
            let maybeAst = parse tokens
            putStrLn "\nParser output\n============="
            print maybeAst
            case maybeAst of
                Nothing -> putStrLn "parser failed"
                Just ast -> do -- der Parser war erfolgreich
                    putStrLn "\nGenerated HTML\n=============="
                    -- generiere HTML und gebe es aus
                    let html = generateHTML ast
                    putStrLn html
                    -- geht nur lokal, nicht im FP Haskell Center
                    writeFile "test.html" html
