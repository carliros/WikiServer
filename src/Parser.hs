module Parser where
import Datatype
import UU.Parsing
import UU.Parsing.CharParser
import UU.Scanner.Position
import Network.URI
import System
import System.Cmd
import Data.Char
import Data.List

-- parsing wikicreole
pWikicreole =   Wikicreole <$ pStuff <*> pList1 pWikielem <* pStuff

pWikielem =  WParagraph        <$> pListSep_ng pSeparator pInline <* pEndLine <* pSepParagraph  -- que hayga almenos un elemento
         <|> WHLine            <$  pKey "---"                     <* pEndLine <* pSepParagraph
         <|> WList . transform <$> pList1 pItem                   <* pEndLine <* pSepParagraph
         <|> (fhead WHead)     <$> pList1 (pSym '=') <* pSpaces <*> 
                                   pListSep_ng pSeparator pInline <* pEndLine <* pSepParagraph
         <|> WNoWikiBlock      <$  pKey "{{{" <*> pAll <*
                                   pKey "}}}" <* pEndLine <* pSepParagraph
    where fhead c li lits = c (length li) lits

pItem =  procList '*' <$> pList1 (pSym '*') <* pSpaces <*> pList1Sep_ng pSpaces pInline <* pEndLine
     <|> procList '#' <$> pList1 (pSym '#') <* pSpaces <*> pList1Sep_ng pSpaces pInline <* pEndLine
    where procList c la li = IContent (length la) c li

pInline =  Italic       <$  pKey "//" <* pSpaces <*> pListSep_ng pSeparator pInline <* pSpaces  <* pKey "//"
       <|> Bold         <$  pKey "**" <* pSpaces <*> pListSep_ng pSeparator pInline <* pSpaces  <* pKey "**"
       <|> NoWikiInline <$  pKey "{{{" <*> pAllInline <* pKey "}}}"
       <|> SimpleString <$> pString
       <|> Break        <$  pKey "\\\\"
       <|> flink1       <$  pKey "[[" <* pSpaces <*> pLink <* pSpaces <* pKey "]]"
       <|> flink2       <$  pKey "[[" <* pSpaces <*> pLink <* pSpaces <*
                            pKey "|"  <* pSpaces <*> pListSep_ng pSeparator pInline <* pSpaces <* pKey "]]"
       <|> fimg1        <$  pKey "{{" <* pSpaces <*> pLinkImg <* pSpaces <* pKey "}}"
       <|> fimg2        <$  pKey "{{" <* pSpaces <*> pLinkImg <* pSpaces <*
                            pKey "|"  <* pSpaces <*> pListSep_ng pSeparator pInline <* pSpaces <* pKey "}}"
    where flink1 lk    = Link lk [SimpleString lk]
          flink2 lk li = Link lk li
          fimg1 lk     = Image lk [SimpleString lk]
          fimg2 lk li  = Image lk li

-- auxiliar functions
transform ls = let tp = getTipo $ head ls
               in foldr (fun2 tp) [] (group ls)
    where getTipo (IContent _ c _) = c
          fun2 tp []                        rest = rest
          fun2 tp l@((IContent n c itm):xs) rest 
                | n > 1            = IWList l : rest
                | tp /= c          = IWList l : rest
                | otherwise        = l ++ rest

-- scanner stuff
pKey [s]     = (:[]) <$> (pSym s) -- <*  pStuff
pKey (s:ss)  = (:)   <$> (pSym s) <*> pKey ss
pKey []      = usererror "Scanner: You cannot have empty reserved words!"

pSeparator =          pSpaces
          <|> "\n" <$ pSpaces <* pSym '\n' <* pSpaces

pStuff  = pList (pAnySym " \t\r\n")

pSpaces = pList (pAnySym " \t\r")

pString    = pList1 ('a'<..>'z' <|> 'A'<..>'Z' <|> '0'<..>'9' <|> pAnySym "\'\".,:;_-()!")
pAll       = pList1 ('a'<..>'z' <|> 'A'<..>'Z' <|> '0'<..>'9' <|> pAnySym " /\\\'\"()&%$.·:,;@|!=?¿_-*#\t\r\n")
pAllInline = pList1 ('a'<..>'z' <|> 'A'<..>'Z' <|> '0'<..>'9' <|> pAnySym " /\\\'\"()&%$.·:,;@|!=?¿_-*#\t\r")

pLink = verificar <$> pList1 ('a'<..>'z' <|> 'A'<..>'Z' <|> '0'<..>'9' <|> pAnySym ":/?%#._-~@=&")
    where verificar lk = case parseURI lk of
                            Nothing  -> case lk of
                                            "google"     -> "http://www.google.com"
                                            "wikicreole" -> "http://www.wikicreole.org"
                                            "hotmail"    -> "http://www.hotmail.com"
                                            "gmail"      -> "http://www.gmail.com"
                                            _            -> "unknowlink"
                            Just uri -> lk

pLinkImg = verificar <$> pList1 ('a'<..>'z' <|> 'A'<..>'Z' <|> '0'<..>'9' <|> pAnySym ":/?%#._-~@=&")
    where verificar lk = case parseURI lk of
                            Nothing  -> "unknowlinkimage"
                            Just uri -> lk
pEndLine = pSpaces <* pSym '\n'
pSepParagraph = (pSpaces <* pList1 (pSym '\n') <* pSpaces) `opt` []





-- debug
debug p inp = do res <- parseStringIO showMessage p inp
                 putStrLn $ show res

-- other stuff (just for the parser)
exec = do
    args <- getArgs
    let nfile    =  (!!) args 0
    res <- parseFile showMessage pWikicreole nfile
    putStrLn $ show res
    return ()

showMessage (Msg expecting (Pos ln cl fn) action)  
    =  let pos = case action of 
                    Insert _ -> "before line:" ++ show ln ++ ", column:" ++ show cl
                    Delete t -> "at " ++ show t ++ "in position line:" ++ show ln ++ ", column:" ++ show cl
       in "\n?? Error      : " ++ pos ++
          "\n?? Expecting  : " ++ show expecting ++
          "\n?? Repaired by: " ++ show action ++ "\n" 

