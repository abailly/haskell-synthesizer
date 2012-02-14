{-# LANGUAGE OverloadedStrings, PackageImports #-}
    
import Network.Miku(miku,html,get,post)
import Network.Miku.Utils(update)
import Hack2.Contrib.Request(input_bytestring)
import Hack2.Contrib.Response(set_content_type,set_body_bytestring)
import Hack2.Handler.SnapServer(run)
import Hack2.Contrib.Request(params)
import "monads-tf" Control.Monad.Reader(ask)
import "monads-tf" Control.Monad.Trans(liftIO)
import Text.Html   
import qualified Data.ByteString.Char8 as B
import SoundIO
import Music

main = run . miku $ do
  get "/" (html $
           B.pack (prettyHtml $
                   thehtml 
                   (body $ concatHtml 
                    [h2 $ stringToHtml "Enter score",
                     (form $ concatHtml [
                         (tag "Tempo" $ stringToHtml "Tempo") ! [strAttr "for" "score" ],
                         input ! [strAttr "name" "tempo"], 
                         br,
                         textarea noHtml ! [strAttr "name" "score"],
                         br,
                         reset "cancel" "Cancel",
                         submit "submit" "Submit"]
                     ) ! [strAttr "action" "/synthesize",
                          strAttr "method" "GET"]
                    ])))
  get "/synthesize" $ do 
    env <- ask 
    let tempo = maybe 140 (read.B.unpack) (lookup "tempo" $ params env)
    let score = maybe [] (read.B.unpack) (lookup "score" $ params env)
    let wav = B.concat $ map (prepareSound.interpret tempo.note) score
    update $ set_content_type "application/octet-stream"
    update $ set_body_bytestring wav
