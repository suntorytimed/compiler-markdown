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
    deriving (Show, Eq)

endChars =" )]\n"

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

    |isDigit x = let (num, rest) = span isDigit str
                     level = (length num)
    in maybe Nothing (\tokens -> Just (T_Num level:tokens))       $ scan rest

    |x=='\n'    = maybe Nothing (\tokens -> Just (T_Newline:tokens))    $ scan xs
    |x=='-'     = maybe Nothing (\tokens -> Just (T_ULI 0:tokens))   $ scan xs
    |x=='+'     = maybe Nothing (\tokens -> Just (T_ULI 0:tokens))   $ scan xs
    |x=='*'     = maybe Nothing (\tokens -> Just (T_ULI 0:tokens))   $ scan xs
    |x=='.'     = maybe Nothing (\tokens -> Just (T_Dot:tokens))    $ scan xs

scan str          =
    let (restOfLine, restOfStr) = span (`notElem` endChars) str
    in maybe Nothing (\tokens -> Just (T_Text restOfLine:tokens)) $ scan restOfStr



