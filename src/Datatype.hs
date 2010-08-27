module Datatype where
import Network.URI

data Wikicreole = Wikicreole [Wikielem]
        deriving Show

data Wikielem = WParagraph    [Inline]
              | WNoWikiBlock  String
              | WHead Int     [Inline]
              | WList         [Item]
              | WHLine
        deriving Show

data Item = IContent Int Char [Inline]
          | IWList            [Item]
        deriving Show

data Inline = Italic                    [Inline]
            | Bold                      [Inline]
            | Link  String [Inline]
            | Image String [Inline]
            | NoWikiInline String
            | SimpleString String
            | Break
        deriving Show

instance Eq (Item) where
    (IContent n1 c1 _) == (IContent n2 c2 _) = n1 == n2 && c1 == c2
    (IContent _  _  _) == _                  = False

