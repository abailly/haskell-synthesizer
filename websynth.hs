-- | A module for sample web server providing wave generation from 
-- a score in Readable form.

-- We use the OverloadedStrings extension in order to benefit from more  
-- efficient string implementation provided by Data.ByteString.Char8 module.
-- By default in Haskell, a String is a list of Char which is extremely 
-- inefficient as a Char is a full Unicode 32-bit number.

{-# LANGUAGE OverloadedStrings, PackageImports #-}
    
-- miku is one of many recent web frameworks that try to offer the same 
-- ease of use than what's available in more dynamic languages like python
-- or ruby.
import Network.Miku(miku,html,get,post)
import Network.Miku.Utils(update)
import Hack2.Contrib.Request(input_bytestring)
import Hack2.Contrib.Response(set_content_type,set_body_bytestring)
import Hack2.Handler.SnapServer(run)
import Hack2.Contrib.Request(params)
import "monads-tf" Control.Monad.Reader(ask)
import "monads-tf" Control.Monad.Trans(liftIO)

-- Templating stuff. Blaze provides a rich set of combinators (functions) to
-- build objects representing HTML structure in a typed and efficient way.
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
  -- miku provides a dedicated monad for expressing routing rules based on standard
  -- http queries structure. Each operation in the monad is a rule that gets matched
  -- by incoming request in order and returns a value.
  get "/" (html $ toByteString $ renderHtmlBuilder mainPage)
  get "/synthesize" $ do 
    env <- ask 
    let tempo = param env "tempo" 140
    let score = param env "score" []
    let wav = B.concat $ map (prepareSound.interpret tempo.note) score
    update $ set_content_type "application/octet-stream"
    update $ set_body_bytestring wav
    where
      param env name def =  maybe def (read.B.unpack) (lookup name $ params env)
