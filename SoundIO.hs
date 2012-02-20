{-# LANGUAGE PackageImports, ViewPatterns #-}
module SoundIO where
import Music
import Sound
import qualified Data.ByteString.Char8 as B
import System.Process(createProcess, shell, CreateProcess(..), StdStream(..))
import System.IO(hSetBuffering, hSetBinaryMode, BufferMode(..))
import "monads-tf" Control.Monad.State(State(..),get,put)
import qualified Data.Map as Map

type Store = Map.Map String String

prepareSound = B.pack.map toEnum.scale (0,255)
outputSound  = B.putStr. prepareSound
note (p,o,d) = Note p o d

command :: String -> State Store Bool
command (words -> ["load",name,file]) = do
  store <- get 
  let store' = Map.insert name file store 
  put store' 
  return True
  
-- use external program 'aplay' to generate sound 
playSound :: (Playable a) => [a] -> IO ()
playSound sounds = do 
  let procDef = (shell $ "aplay -r " ++ (show samplingRate)) { std_in = CreatePipe }
  (Just hin, _ ,_, _ ) <- createProcess procDef
  hSetBuffering hin NoBuffering
  hSetBinaryMode hin True
  B.hPutStr hin $ B.concat $ map (prepareSound.interpret 160) sounds
