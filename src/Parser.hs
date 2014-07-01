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
            Just contentString -> fmap (addULI (LI contentString)) $ parse rest
parse (T_Minus : xs) =
    let (content, rest) = span(/=T_Newline) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString -> fmap (addULI (LI contentString)) $ parse rest
-- einem listitem-Marker muss auch ein Text folgen. Das gibt zusammen ein Listitem im AST.
-- es wird mit der Hilfsfunktion addLI eingefügt
parse (T_Num i: T_Dot : xs) =
    let (content, rest) = span(/=T_Newline) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString -> fmap (addSLI (LI contentString)) $ parse rest
-- ein Text am Anfang gehört in einen Absatz. Damit direkt auf einander folgende Texte in einem gemeinsamen
-- Absatz landen, wird die Hilfsfunktion addP genutzt um den Text einzufügen
parse (T_Text str: xs)         = fmap (addP (P str)) $ parse xs
-- Codeblock
parse (T_White i : T_Text str: xs)
    |i==4 =
        let (content, rest) = span(/=T_Newline) xs
            in case parse content of
                Nothing -> Nothing
                Just contentString -> fmap (addP (C str contentString)) $ parse rest
-- Fettdruck
parse (T_Star i : T_Text str: xs) =
    let (content, rest) = span(/=T_Star i) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString ->fmap (addP (Bold str contentString)) $ parse rest
parse (T_Star i : xs) = fmap (addP (P "")) $ parse xs
-- href
parse (T_SBracketO: xs) =
    let (content, rest) = span(/=T_SBracketC) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString -> fmap (addID (Id contentString)) $ parse rest
-- link
parse (T_RBracketO: xs) =
    let (content, rest) = span(/=T_RBracketC) xs
        in case parse content of
            Nothing -> Nothing
            Just contentString -> fmap (addLink (Link contentString)) $ parse rest
-- ein automatischer Link wird eingefügt
parse (T_ABracketO : xs) =
    let (content, rest) = span(/=T_ABracketC) xs
        in case parse content of
            Nothing -> Nothing
            -- eine addLink-Funktion mit zwei Paraemetern ist besser
            Just contentString -> fmap (addLi (Id contentString, Link contentString)) $ parse rest
-- Square Bracket ignorieren
parse (T_SBracketC : xs) = fmap (addP (P "")) $ parse xs
-- Round Bracket ignorieren
parse (T_RBracketC : xs) = fmap (addP (P "")) $ parse xs
-- Spitze Klammer ignorieren
parse (T_ABracketC : xs) = fmap (addP (P "")) $ parse xs
-- Newline nach 2 oder mehr Leerzeichen
parse (T_White i : T_Newline:xs)
    |i>=2   = fmap (addP EmptyLine) $ parse xs
-- Leerzeichen parsen
parse (T_White i : xs) = fmap (addP (P " ")) $ parse xs
-- Ein Punkt als Satzzeichen
parse (T_Dot : xs) = fmap (addP (P ".")) $ parse xs
-- Ein Doppelpunkt als Satzzeichen
parse (T_DDot : xs) = fmap (addP (P ":")) $ parse xs
-- Eine Zahl
parse (T_Num i : xs) = fmap (addP (P i)) $ parse xs
-- ein Backslash kann Zeichen escapen
parse (T_BSlash:x: xs)
    |x == T_BSlash = fmap (addP (P "\\")) $ parse xs    -- ein Backslash
    |x == T_SBracketO = fmap (addP (P "[")) $ parse xs  -- eine eckige Klammer geoeffnet
    |x == T_RBracketO = fmap (addP (P "(")) $ parse xs  -- eine runde Klammer geoeffnet
    |x == T_WBracketO = fmap (addP (P "{")) $ parse xs  -- eine geschweifte Klammer geoeffnet
    |x == T_ABracketO = fmap (addP (P "<")) $ parse xs  -- eine spitze Klammer geoeffnet
    |x == T_SBracketC = fmap (addP (P "]")) $ parse xs  -- eine eckige Klammer geschlossen
    |x == T_RBracketC = fmap (addP (P ")")) $ parse xs  -- eine runde Klammer geschlossen
    |x == T_WBracketC = fmap (addP (P "}")) $ parse xs  -- eine geschweifte Klammer geschlossen
    |x == T_ABracketC = fmap (addP (P ">")) $ parse xs  -- eine spitze Klammer geschlossen
    |x == T_Star 1 = fmap (addP (P "*")) $ parse xs   -- ein Sternchen
    |x == T_H 1 = fmap (addP (P "#")) $ parse xs   -- ein Hash
    |x == T_Dot = fmap (addP (P ".")) $ parse xs   -- ein Punkt
    |x == T_ExMark = fmap (addP (P "!")) $ parse xs     -- ein Ausrufezeichen
    |x == T_BQuote 1 = fmap (addP (P "`")) $ parse xs -- ein Backquote`
    |x == T_Plus = fmap (addP (P "+")) $ parse xs -- ein Plus
    |x == T_Minus = fmap (addP (P "-")) $ parse xs -- ein Minus

-- Der gesamte Rest wird für den Moment ignoriert. Achtung: Der Parser schlägt, in der momentanen Implementierung, nie fehl.
-- Das kann in der Endfassung natürlich nicht so bleiben!
parse ts = error $ show ts
-- parse _ = Just $ Sequence []


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

-- Einfügen eines normalen Links erfordert sowohl addID als auch addLink
addLink :: AST -> AST -> AST
addLink (Link link) (Sequence ast) = Sequence (Link link : ast)
addID :: AST -> AST -> AST
addID (Id id) (Sequence ast) = Sequence (Id id : ast)

-- Einfügen eines automatischen Links
addLi :: (AST,AST) -> AST -> AST
-- Wir erzeugen einen neuen Link.
addLi (Id id,Link link) (Sequence ast) = Sequence (Link link: Id id : ast)