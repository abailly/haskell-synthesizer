{-# LANGUAGE PackageImports, ViewPatterns #-}
module SoundIO where
import Music
import Sound
-- imports can be qualified to use a shortcut prefix. This allows disambiguating 
-- functions occuring in several modules. 
-- Note that by default we import every symbol from a module when it is used 
-- unqualified. Qualifying it implies that no symbol is imported by default.
import qualified Data.ByteString.Char8 as B
-- We can explicitly restrict the set of symbols imported from a module
import System.Process(createProcess, shell, 
                      -- import a type and all its constructors. 
                      CreateProcess(..), 
                      StdStream(..))
import System.IO(hSetBuffering, hSetBinaryMode, hPutStrLn, hGetLine, stdin, hFlush, stdout, BufferMode(..))
-- Package-qualified imports resolves ambiguity when two modules export the same symbol.
-- This is generally a bad idea and a sign that the build environment is somehow broken 
-- or in bad shape.
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


-- | A command that uses the "dreaded" State monad to maintain a "state".
-- A function operates "in" a monad M when its return type is M X for some X.
-- The State type definition is something similar to the following:
-- @
-- newtype State s a = State { runState :: s -> (a, s) }
-- @
-- In other words, it encapsulates a function that takes a state value @s@ and
-- returns a value @a@ together with a possibly updated state @s@. Sequencing 
-- operations within the State monad hides the details of passing the state @s@
-- from one function to another hence propagating "mutations" down the chain.
command :: String -> State Store CommandResult

-- here we use a View pattern (a language extension) as a pattern in the clause:
-- The function @words@ is called with the actual argument to command as as its
-- argument, and its result is matched against the pattern on
-- the right of the @->@ symbol.
command (words -> ["load",name,file]) = do
  -- @get@ is a monadic operation in the State monad that exposes the current value of
  -- the state @s@. The arrow @<-@ is part of the special monad notation (just like the 
  -- @do@ keyword.
  store <- get 
  let store' = Map.insert name file store 
  -- @put@ modifies the current state using its argument
  put store' 
  -- @return@ is the standard Monad function for producing a result in the Monad. Its 
  -- name is purposefully confusing with the @return@ of imperative languages but the 
  -- semantic is very different.
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

-- | Command-loop is another monadic computation, this time in the IO monad.
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
    
-- | Use external program 'aplay' to generate sound.
-- Haskell has a rich set of functions to interact with external processes and 
-- system, which are rather simple to use. 
playSound :: (Playable a) => [a] -> IO ()
playSound sounds = do 
  let procDef = (shell $ "aplay -r " ++ (show samplingRate)) { std_in = CreatePipe }
  (Just hin, _ ,_, _ ) <- createProcess procDef
  hSetBuffering hin NoBuffering
  hSetBinaryMode hin True
  B.hPutStr hin $ B.concat $ map (prepareSound.interpret 160) sounds
