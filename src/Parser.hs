module Parser ( parse {- nur parse exportieren -} )

    where

import           IR
import           Scanner

-- Der Parser versucht aus einer Liste von MDToken einen AST zu erzeugen 
parse :: [MDToken] -> Maybe AST
-- Die leere Liste ergibt eine leere Sequenz
parse [] = Just $ Sequence []
-- Zwei Zeilenumbrüche hintereinander sind eine leere Zeile, die in eine Sequenz eingeführt wird (wirklich immer?)
parse (T_Newline:T_Newline:xs) = maybe Nothing (\(Sequence ast) -> Just $ Sequence (EmptyLine : ast)) $ parse xs
-- ein einzelnes Leerzeichen ignorieren wir (für den Moment?)
parse (T_Newline:xs)           = parse xs
-- einem Header muss ein Text folgen. Das ergibt zusammen einen Header im AST, er wird einer Sequenz hinzugefügt
parse (T_H i : xs) =
    let (content, rest) = span(/=T_Newline) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString -> maybe Nothing (\(Sequence ast) -> Just $ Sequence (H i contentString:ast)) $ parse rest
-- einem listitem-Marker muss auch ein Text folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addLI eingefügt
parse (T_Plus : xs) =
    let (content, rest) = span(/=T_Newline) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString -> maybe Nothing (\ast -> Just $ addULI (LI contentString) ast) $ parse rest
parse (T_Minus : xs) =
    let (content, rest) = span(/=T_Newline) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString -> maybe Nothing (\ast -> Just $ addULI (LI contentString) ast) $ parse rest
-- einem listitem-Marker muss auch ein Text folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addLI eingefügt
parse (T_Num i: T_Dot : xs) =
    let (content, rest) = span(/=T_Newline) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString -> maybe Nothing (\ast -> Just $ addSLI (LI contentString) ast) $ parse rest
-- ein Text am Anfang gehört in einen Absatz. Damit direkt auf einander folgende Texte in einem gemeinsamen
-- Absatz landen, wird die Hilfsfunktion addP genutzt um den Text einzufügen
parse (T_Text str: xs)         = maybe Nothing (\ast -> Just $ addP (P str) ast) $ parse xs
-- Codeblock
parse (T_White i : T_Text str: xs)
    |i==4 =
        let (content, rest) = span(/=T_Newline) xs
            in case parse content of
                Nothing -> Nothing
                Just contentString -> maybe Nothing (\ast -> Just $ addP (C contentString) ast) $ parse rest
-- href
parse (T_SBracketO: xs) =
    let (content, rest) = span(/=T_SBracketC) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString -> maybe Nothing (\ast -> Just $ addP (Id contentString) ast) $ parse rest
-- link
parse (T_RBracketO: xs) =
    let (content, rest) = span(/=T_RBracketC) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString -> maybe Nothing (\ast -> Just $ addP (Link contentString) ast) $ parse rest
-- Square Bracket ignorieren
parse (T_SBracketC : xs) = maybe Nothing (\ast -> Just $ addP (P "") ast) $ parse xs
-- Round Bracket ignorieren
parse (T_RBracketC : xs) = maybe Nothing (\ast -> Just $ addP (P "") ast) $ parse xs
-- Newline nach 2 oder mehr Leerzeichen
parse (T_White i : T_Newline:xs)
    |i>=2   = maybe Nothing (\ast -> Just $ addP (EmptyLine) ast) $ parse xs
-- Leerzeichen parsen
parse (T_White i : xs) = maybe Nothing (\ast -> Just $ addP (P " ") ast) $ parse xs
-- Ein Punkt als Satzzeichen
parse (T_Dot : xs) = maybe Nothing (\ast -> Just $ addP (P ".") ast) $ parse xs
-- Eine Zahl
parse (T_Num i : xs) = maybe Nothing (\ast -> Just $ addP (P i) ast) $ parse xs
-- ein Backslash kann Zeichen escapen
parse (T_BSlash:x: xs)
    |x == T_BSlash = maybe Nothing (\ast -> Just $ addP (P "\\") ast) $ parse xs    -- ein Backslash
    |x == T_SBracketO = maybe Nothing (\ast -> Just $ addP (P "[") ast) $ parse xs  -- eine eckige Klammer geoeffnet
    |x == T_RBracketO = maybe Nothing (\ast -> Just $ addP (P "(") ast) $ parse xs  -- eine runde Klammer geoeffnet
    |x == T_WBracketO = maybe Nothing (\ast -> Just $ addP (P "{") ast) $ parse xs  -- eine geschweifte Klammer geoeffnet
    |x == T_ABracketO = maybe Nothing (\ast -> Just $ addP (P "<") ast) $ parse xs  -- eine spitze Klammer geoeffnet
    |x == T_SBracketC = maybe Nothing (\ast -> Just $ addP (P "]") ast) $ parse xs  -- eine eckige Klammer geschlossen
    |x == T_RBracketC = maybe Nothing (\ast -> Just $ addP (P ")") ast) $ parse xs  -- eine runde Klammer geschlossen
    |x == T_WBracketC = maybe Nothing (\ast -> Just $ addP (P "}") ast) $ parse xs  -- eine geschweifte Klammer geschlossen
    |x == T_ABracketC = maybe Nothing (\ast -> Just $ addP (P ">") ast) $ parse xs  -- eine spitze Klammer geschlossen
    |x == T_Star 1 = maybe Nothing (\ast -> Just $ addP (P "*") ast) $ parse xs   -- ein Sternchen
    |x == T_H 1 = maybe Nothing (\ast -> Just $ addP (P "#") ast) $ parse xs   -- ein Hash
    |x == T_Dot = maybe Nothing (\ast -> Just $ addP (P ".") ast) $ parse xs   -- ein Punkt
    |x == T_ExMark = maybe Nothing (\ast -> Just $ addP (P "!") ast) $ parse xs     -- ein Ausrufezeichen
    |x == T_BQuote 1 = maybe Nothing (\ast -> Just $ addP (P "`") ast) $ parse xs -- ein Backquote`
    |x == T_Plus = maybe Nothing (\ast -> Just $ addP (P "+") ast) $ parse xs -- ein Plus
    |x == T_Minus = maybe Nothing (\ast -> Just $ addP (P "-") ast) $ parse xs -- ein Minus
-- Der gesamte Rest wird für den Moment ignoriert. Achtung: Der Parser schlägt, in der momentanen Implementierung, nie fehl.
-- Das kann in der Endfassung natürlich nicht so bleiben!
-- parse ts = error $ show ts
parse _ = Just $ Sequence []


-- Hilfsfunktionen für den Parser

-- Einfügen eines Listenelements in eine ungeordnete Liste
addULI :: AST -> AST -> AST
-- Wenn wir ein Listenelement einfügen wollen und im Rest schon eine UL haben, fügen wir das Element in die UL ein
addULI li (Sequence (UL lis : ast)) = Sequence (UL (li:lis) : ast)
-- Andernfalls erzeugen wir eine neue UL.
addULI li (Sequence ast) = Sequence (UL [li] : ast)

-- Mehrere aufeinander folgende Texte werden zu einem Absatz zusammengefügt.
addP :: AST -> AST -> AST
-- Wenn wir zwei Absätze hintereinander finden, fassen wir diese zusammen 
addP (P str1) (Sequence (P str2 : ast)) = Sequence (P (str1 ++ str2) : ast)
-- Andernfalls bleibt der Absatz alleine
addP p (Sequence ast) = Sequence (p : ast)

-- Einfügen eines Listenelements in eine sortierte Liste
addSLI :: AST -> AST -> AST
-- Wenn wir ein Listenelement einfügen wollen und im Rest schon eine SL haben, fügen wir das Element in die SL ein
addSLI li (Sequence (SL lis : ast)) = Sequence (SL (li:lis) : ast)
-- Andernfalls erzeugen wir eine neue SL.
addSLI li (Sequence ast) = Sequence (SL [li] : ast)
