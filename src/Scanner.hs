module Scanner where

-- MD: Markdown
data MDToken = T_Newline     -- '\n' 
             | T_H Int       -- ein Header mit der Anzahl der Hashes
             | T_Text String -- Text, aber immer nur bis zum Zeilenende, Text über mehrere Zeilen muss vom Parser zusammengesetzt werden
             | T_ULI Int     -- ein ungeordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
             | T_SLI Int     -- ein geordnetes Listenelement-Marker mit der (Einrückungs-)Ebene
    deriving (Show, Eq)

scan :: String -> Maybe [MDToken]
-- Rekursionsende
scan ""           = Just []
-- eine Überschrift
scan str@('#':xs) =
        -- String aufteilen in Hashes und Rest
    let (hashes, rest) = span (=='#') str
        -- Anzahl der Hashes ergibt das Level, aber höchstens 6 werden gezählt, der Rest ignoriert
        level = min (length hashes) 6
    in maybe Nothing (\tokens -> Just (T_H level:tokens))      $ scan rest
    
    -- bei 2 oder mehr Leerzeichen wird eine neue Zeile eingefuegt
scan str@(' ':xs) =
        -- String aufteilen in Whitespaces und Rest
    let (whitespace, rest) = span (==' ') str
        -- Anzahl der Whitespaces ist egal, es wird immer nur eine Newline eingefuegt
        level = (length whitespace)
    in maybe Nothing (\tokens -> Just (T_Newline:tokens))     $ scan rest

-- sonst lesen wir einfach den Rest bis zum Zeilenende in ein Text-Token ein
-- Zeilenumbrüche aufheben um im Parser Leerzeilen zu erkennen
scan ('\n':xs)    = maybe Nothing (\tokens -> Just (T_Newline:tokens)) $ scan xs

-- wenn das '-' am Zeilenanfang gelesen wird, ist es Level 0
-- TODO: noch sind wir sicher am Zeilenanfang, aber nicht mehr unbedingt, wenn wir weitere Fälle einbauen (Links etc.)
scan ('-':xs)     = maybe Nothing (\tokens -> Just (T_ULI 0:tokens))    $ scan xs
scan ('+':xs)     = maybe Nothing (\tokens -> Just (T_ULI 0:tokens))    $ scan xs
scan ('*':xs)     = maybe Nothing (\tokens -> Just (T_ULI 0:tokens))    $ scan xs

-- wenn eine Zahl gefolgt von einem Punkt da steht, dann soll eine geordnete Liste erzeugt werden
--scan ('???':xs)     = maybe Nothing (\tokens -> Just (T_SLI 0:tokens))    $ scan xs

scan str          =
    let (restOfLine, restOfStr) = span (/='\n') str
    in maybe Nothing (\tokens -> Just (T_Text restOfLine:tokens)) $ scan restOfStr



