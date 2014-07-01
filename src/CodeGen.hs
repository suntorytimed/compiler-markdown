-- Modul zum Generieren von HTML-Code der als String repräsentiert wird aus einem AST.
module CodeGen where

import IR

-- HTML generieren
-- zuerst das äußere Gerüst
generateHTML :: AST -> String
generateHTML ast = "<html>\n<head></head>\n<body>\n" ++ generateHTML' ast ++ "</body>\n</html>"

-- dann Elemente für jeden AST-Knoten
generateHTML' :: AST -> String
-- eine Sequenz
generateHTML' (Sequence (a:as)) = generateHTML' a ++ generateHTML' (Sequence as)
-- sortierte Liste
generateHTML' (SL (a:as)) = "<ol>\n" ++ generateHTML' a ++ generateHTML' (Sequence as) ++ "</ol>\n"
-- eine Überschrift
generateHTML' (H i ast) = "<h" ++ show i ++ ">" ++ generateHTML' ast ++ "</h" ++ show i ++ ">\n"
-- eine ungeordnete Liste
generateHTML' (UL lis) = "<ul>\n" ++ concatMap generateHTML' lis ++ "</ul>\n"
-- Listenelemente
generateHTML' (LI ast) = "<li>" ++ generateHTML' ast ++ "</li>\n"
-- ein Absatz
generateHTML' (P str)  = str
-- Code
generateHTML' (C str ast) = "<code>" ++ str ++ generateHTML' ast ++ "</code>\n"
-- Fettdruck
generateHTML' (Bold str ast) = "<b>" ++ str ++ generateHTML' ast ++ "</b>\n"
-- Punkt
generateHTML' (DOT) = "."
-- Doppelpunkt
generateHTML' (DDot) = ":"
-- EmptyLine
generateHTML' (EmptyLine) = "<br></br>\n"
-- Link-ID
generateHTML' (Id ast) = generateHTML' ast ++ "</a> "
-- Link
generateHTML' (Link ast) = "<a href=\"" ++ generateHTML' ast ++ "\">"
-- alles andere (?) wird für den Moment ignoriert
generateHTML' (Sequence []) = ""
generateHTML' ast = error $ show ast
