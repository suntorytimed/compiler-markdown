module Scanner where
import Data.Char (isDigit)

-- MD: Markdown
data MDToken = T_Newline     -- '\n' 
             | T_H Int       -- ein Header mit der Anzahl der Hashes
             | T_Text String -- Text, aber immer nur bis zum Zeilenende, Text über mehrere Zeilen muss vom Parser zusammengesetzt werden
             | T_ULI Int     -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_SLI Int     -- ein geordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_White Int   -- ein Header mit der Anzahl der Hashes
             | T_Num Int     -- eine Zahl mit Anzahl an Stellen
             | T_Dot         -- ein Punkt
             | T_SBracketO   -- eine eckige Klammer geoeffnet
             | T_RBracketO   -- eine runde Klammer geoeffnet
             | T_WBracketO   -- eine geschweifte Klammer geoeffnet
             | T_ABracketO   -- eine spitze Klammer geoeffnet
             | T_SBracketC   -- eine eckige Klammer geschlossen
             | T_RBracketC   -- eine runde Klammer geschlossen
             | T_WBracketC   -- eine geschweifte Klammer geschlossen
             | T_ABracketC   -- eine spitze Klammer geschlossen
             | T_Star Int    -- ein Sternchen
             | T_DDot        -- ein Doppelpunkt
             | T_BSlash      -- ein Backslash
             | T_ExMark      -- ein Ausrufezeichen
    deriving (Show, Eq)

endChars =" \\*:<{[()]}>\n"

scan :: String -> Maybe [MDToken]
-- Rekursionsende
scan ""           = Just []
-- eine Überschrift
scan str@(x:xs)
    -- String aufteilen in Hashes und Rest
    |x == '#' = let (hashes, rest) = span (=='#') str
                -- Anzahl der Hashes ergibt das Level, aber höchstens 6 werden gezählt, der Rest ignoriert
                    level = min (length hashes) 6
    in maybe Nothing (\tokens -> Just (T_H level:tokens))      $ scan rest

    -- String aufteilen in Whitespaces und Rest
    |x == ' ' = let (whitespace, rest) = span (==' ') str
                    -- Anzahl der Whitespaces ist egal, es wird immer nur eine Newline eingefuegt
                    level = (length whitespace)
    in maybe Nothing (\tokens -> Just (T_White level:tokens))     $ scan rest

    |x == '*' = let (star, rest) = span (=='*') str
                    -- Anzahl der Whitespaces ist egal, es wird immer nur eine Newline eingefuegt
                    level = (length star)
    in maybe Nothing (\tokens -> Just (T_Star level:tokens))     $ scan rest

    |isDigit x = let (num, rest) = span isDigit str
                     level = (length num)
    in maybe Nothing (\tokens -> Just (T_Num level:tokens))       $ scan rest

    |x=='\n'   = maybe Nothing (\tokens -> Just (T_Newline:tokens))    $ scan xs
    |x=='-'    = maybe Nothing (\tokens -> Just (T_ULI 0:tokens))   $ scan xs
    |x=='+'    = maybe Nothing (\tokens -> Just (T_ULI 0:tokens))   $ scan xs
    |x=='.'    = maybe Nothing (\tokens -> Just (T_Dot:tokens))    $ scan xs
    |x=='['    = maybe Nothing (\tokens -> Just (T_SBracketO:tokens))    $scan xs
    |x=='('    = maybe Nothing (\tokens -> Just (T_RBracketO:tokens))    $scan xs
    |x=='{'    = maybe Nothing (\tokens -> Just (T_WBracketO:tokens))    $scan xs
    |x=='<'    = maybe Nothing (\tokens -> Just (T_ABracketO:tokens))    $scan xs
    |x==']'    = maybe Nothing (\tokens -> Just (T_SBracketC:tokens))    $scan xs
    |x==')'    = maybe Nothing (\tokens -> Just (T_RBracketC:tokens))    $scan xs
    |x=='}'    = maybe Nothing (\tokens -> Just (T_WBracketC:tokens))    $scan xs
    |x=='>'    = maybe Nothing (\tokens -> Just (T_ABracketC:tokens))    $scan xs
    |x==':'    = maybe Nothing (\tokens -> Just (T_DDot:tokens))    $scan xs
    |x=='\\'   = maybe Nothing (\tokens -> Just (T_BSlash:tokens))    $scan xs
    |x=='!'    = maybe Nothing (\tokens -> Just (T_ExMark:tokens))    $scan xs

scan str          =
    let (restOfLine, restOfStr) = span (`notElem` endChars) str
    in maybe Nothing (\tokens -> Just (T_Text restOfLine:tokens)) $ scan restOfStr



