{-# LANGUAGE OverloadedStrings, PackageImports, QuasiQuotes #-}
    
import Network.Miku(miku,html,get,post)
import Network.Miku.Utils(update)
import Hack2.Contrib.Request(input_bytestring)
import Hack2.Contrib.Response(set_content_type,set_body_bytestring)
import Hack2.Handler.SnapServer(run)
import Hack2.Contrib.Request(params)
import "monads-tf" Control.Monad.Reader(ask)
import "monads-tf" Control.Monad.Trans(liftIO)
import Blaze.ByteString.Builder
import Text.Blaze.Html5 hiding (map, html)
import Text.Blaze.Html5.Attributes hiding (form,label)
import Text.Blaze.Renderer.Utf8 (renderHtmlBuilder)
import qualified Data.ByteString.Char8 as B
import SoundIO
import Music hiding (value)

                   
mainPage :: Html
mainPage = docTypeHtml $
           body $ do
             h2 $ "Enter score"
             form ! action "/synthesize" ! method "GET" $ do       
               label ! for "tempo" $ text "Tempo"
               input ! name "tempo" 
               br
               textarea ! name "score" $ text "enter your score"
               br
               input ! type_ "reset" ! value "Cancel"
               input ! type_ "submit" ! value "Submit"
 
main = run . miku $ do
  get "/" (html $ toByteString $ renderHtmlBuilder mainPage)
  get "/synthesize" $ do 
    env <- ask 
    let tempo = maybe 140 (read.B.unpack) (lookup "tempo" $ params env)
    let score = maybe [] (read.B.unpack) (lookup "score" $ params env)
    let wav = B.concat $ map (prepareSound.interpret tempo.note) score
    update $ set_content_type "application/octet-stream"
    update $ set_body_bytestring wav
