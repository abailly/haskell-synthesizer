{-# LANGUAGE OverloadedStrings #-}
    
import Network.Miku
import Hack2.Handler.SnapServer
import Text.Html   
import qualified Data.ByteString.Char8 as B

main = run . miku $ do
  get "/" (html $
           B.pack (prettyHtml $
                   thehtml 
                   (body $ concatHtml 
                    [h2 $ stringToHtml "Enter score",
                     (form $ concatHtml [
                         textarea noHtml,
                         br,
                         reset "cancel" "Cancel",
                         submit "submit" "Submit"]
                     ) ! [strAttr "action" "/synthesize",
                          strAttr "method" "GET"]
                    ])))
