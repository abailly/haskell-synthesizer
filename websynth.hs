{-# LANGUAGE OverloadedStrings #-}
    
import Network.Miku
import Hack2.Handler.SnapServer
    
main = run . miku $ get "/" (text "miku power")
