{-# LANGUAGE PackageImports, ViewPatterns #-}
module SoundIO where
import Music
import Sound
import qualified Data.ByteString.Char8 as B
import System.Process(createProcess, shell, CreateProcess(..), StdStream(..))
import System.IO(hSetBuffering, hSetBinaryMode, hPutStrLn, hGetLine, stdin, hFlush, stdout, BufferMode(..))
import "monads-tf" Control.Monad.State(State(..),get,put,runState)
import qualified Data.Map as Map

type Store = Map.Map String String

emptyStore = Map.empty

data CommandResult = Loaded 
                   | Play String
                   | Error String
                   deriving (Eq,Show)
                            
prepareSound = B.pack.map toEnum.scale (0,255)
outputSound  = B.putStr. prepareSound
note (p,o,d) = Note p o d

command :: String -> State Store CommandResult
command (words -> ["load",name,file]) = do
  store <- get 
  let store' = Map.insert name file store 
  put store' 
  return Loaded
command (words -> ["play",name]) = do
  store <- get 
  return $ maybe 
    (Error $ "score " ++ name ++" does not exist") 
    Play 
    (Map.lookup name store)

command c = return $ Error $ "'" ++ c ++ "' is not a valid command"
  
prompt = do putStr "> "
            hFlush stdout

commandLoop :: Store -> IO ()
commandLoop s = do
  cmd <- hGetLine stdin 
  let (result, s') = runState (command cmd) s 
  eval result
  prompt 
  commandLoop s'
  where
    eval Loaded      = hPutStrLn stdout "loaded" 
    eval (Play file) = do
      scoreData <- readFile file
      playSound $ (map note.read) scoreData
    
-- use external program 'aplay' to generate sound 
playSound :: (Playable a) => [a] -> IO ()
playSound sounds = do 
  let procDef = (shell $ "aplay -r " ++ (show samplingRate)) { std_in = CreatePipe }
  (Just hin, _ ,_, _ ) <- createProcess procDef
  hSetBuffering hin NoBuffering
  hSetBinaryMode hin True
  B.hPutStr hin $ B.concat $ map (prepareSound.interpret 160) sounds
