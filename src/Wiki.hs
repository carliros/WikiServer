module Main where

import Data.Maybe (fromJust)
import Text.Html
import Network.Shed.Httpd
import Network.URI
import Data.List
import UU.Parsing.CharParser
import System.Cmd
import System
import Datatype
import Parser

-- | Convert an inline element to html

inlineToHtml :: Inline -> Html
inlineToHtml inl = case inl of
                    SimpleString str -> stringToHtml str
                    Italic litem     -> italics << listInlineToHtml (intersperse (SimpleString " ") litem)
                    Bold   litem     -> bold    << listInlineToHtml (intersperse (SimpleString " ") litem)
                    Break            -> br
                    NoWikiInline str -> pre     << stringToHtml str
                    Link lk litem    -> anchor << listInlineToHtml (intersperse (SimpleString " ") litem) ! [href lk]
                    Image lk litem   -> image ! [src lk]    -- need to fix, no suport alt attribute

-- | Convert a list inline elements to html

listInlineToHtml :: [Inline] -> Html
listInlineToHtml = foldr ((+++) . inlineToHtml) noHtml

-- | Convert block type elements into html

blockToHtml :: Wikielem -> Html
blockToHtml bl = case bl of
                    WHead n litem    -> getHead n << listInlineToHtml (intersperse (SimpleString " ") litem)
                    WParagraph litem -> paragraph << listInlineToHtml (intersperse (SimpleString " ") litem)
                    WNoWikiBlock str -> pre << stringToHtml str
                    WHLine           -> hr
                    WList litem      -> getList litem << map generateElem litem
    where getHead n = case n of
                      1 -> h1
                      2 -> h2
                      3 -> h3
                      4 -> h4
                      5 -> h5
                      _ -> h6
          getList (IContent _ '*' _ : xs) = ulist
          getList (IContent _ '#' _ : xs) = olist
          getList (IWList litem     : xs) = ulist
          generateElem (IContent _ _ litem) = li << listInlineToHtml (intersperse (SimpleString " ") litem)
          generateElem (IWList litem)       = getList litem << map generateElem litem

-- | Convert a wikicreole datatype into html

pageToHtml :: Wikicreole -> Html
pageToHtml (Wikicreole le) = foldr ((+++).blockToHtml) noHtml le

-- | Convert a wiki-formatted string to a renderable HTML string.

wikify :: String -> IO String
wikify str = do page <- parseStringIO showMessage pWikicreole str
                return $ renderHtml $ (body <<) $ pageToHtml page

--------------------------------------------------------------------------------
-- server http

initMyServer :: (Request -> IO Response) -> IO Server
initMyServer = initServer myPort

myPort :: Int
myPort = 7734

myDir = "./www/"

contentText, contentHtml :: (String, String)
contentText = contentType "text/plain"
contentHtml = contentType "text/html"

responseOK, responseNotFound :: Int
responseOK       = 200
responseNotFound = 404

methodGET, methodPOST :: String
methodGET  = "GET"
methodPOST = "POST"

--------------------------------------------------------------------------------
-- | Methods of answering to the http server

fileServer :: Request -> IO Response
fileServer (Request met uri headers body) = 
    case parseUri (span (/='.') (show uri)) of
        TText nm  -> do rb <- getFile $ tail nm
                        case rb of
                            Just body -> return $ Response responseOK [contentText] body
                            Nothing   -> return $ Response responseNotFound [contentText] "Something is wrong."
        THtml nm  -> do rb <- getFile $ tail nm
                        case rb of
                            Just body -> return $ Response responseOK [contentHtml] body
                            Nothing   -> return $ Response responseNotFound [contentText] "Something is wrong."
        TWiki nm  -> do rb <- getFile $ tail nm
                        case rb of
                            Just body -> do content <- wikify body
                                            return $ Response responseOK [contentHtml] content
                            Nothing   -> return $ Response responseNotFound [contentText] "Something is wrong." 
        TWelc     ->    return $ Response responseOK [contentText] "Welcome to the httpd-shed server."
        TError nm ->    return $ Response responseNotFound [contentText] "Something is wrong."

getFile nm = catch 
                (do body <- readFile $ myDir ++ nm
                    return $ Just body)
                handler
    where handler er = return Nothing

data TipeFile = TText  String
              | THtml  String
              | TWiki  String
              | TWelc  
              | TError String
                deriving Show

parseUri (nm, tp) = case tp of
                      ".txt"  -> TText $ nm++tp
                      ".html" -> THtml $ nm++tp
                      ".wiki" -> TWiki $ nm++tp
                      _       -> if nm == "/"
                                  then TWelc
                                  else TError nm

--------------------------------------------------------------------------------

-- other stuff
main :: IO()
main = do putStrLn "Welcome to the wiki server.\nListening on port 7734"
          initMyServer fileServer
          return ()
