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
                         textarea noHtml ! [strAttr "name" "score"],
                         br,
                         reset "cancel" "Cancel",
                         submit "submit" "Submit"]
                     ) ! [strAttr "action" "/synthesize",
                          strAttr "method" "GET"]
                    ])))
  get "/synthesize" $ do 
    env <- ask 
    let tempo = maybe 80 (read.B.unpack) (lookup "tempo" $ params env)
    let score = maybe [] (read.B.unpack) (lookup "score" $ params env)
    liftIO $ B.putStrLn $ B.pack $ "score: " ++ show score
    let wav = B.concat $ map (prepareSound.interpret tempo.note) score
    liftIO $ input_bytestring env >>= B.putStrLn 
    update $ set_content_type "application/octet-stream"
    update $ set_body_bytestring wav
