module IR where

-- Abstract Syntax Tree für HTML-Generierung. Daher schon nahe an HTML angelehnt.
data AST = Sequence [AST] -- eine Sequenz von HTML-Elementen
         | H Int AST      -- eine Überschrift, de Int ist das Level (6 für H6) und der String der Text
         | UL [AST]       -- eine ungeordnete Liste, in der Liste müssen dann die Listenelemente stehen
         | LI AST         -- ein Listenelement mit dem Inhalt
         | P String       -- ein Absatz mit dem Inhalt
         | EmptyLine      -- eine leere Zeile
         | C AST          -- ein Codeblock
         | SL [AST]       -- eine sortierte Liste
         | DOT            -- ein Punkt
         | Id AST         -- eine Link ID
         | Link AST       -- der Link
         | DDot           -- ein Doppelpunkt
    deriving (Show)