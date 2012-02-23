{- Command-line interface. 

By convention program entry point is the symbol @main@ defined in the 
module @Main@. Implicitly an unnamed module is Main.
-}
import System.Environment (getArgs)
import Sound
import Music
import SoundIO

{-| 
We define various Constant Applicative Forms that can be used to 
to generate music. Variables in Haskell are immutable and referentially
transparent: One can always replace the variable with the expression
it denotes. This implies that CAF and local variables value is memoized
upon its first evaluation.
-}
simplenote p = Note p 4 Crotchet

cMajor = Chord (map note [(C,4,Crotchet), 
                          (E,4,Crotchet), 
                          (G,4,Crotchet)]) Crotchet

cMinor = Chord (map note [(C,4,Crotchet), 
                          (Ds,4,Crotchet), 
                          (G,4,Crotchet)]) Crotchet

-- main = do
--   outputSound $ (interpret 60 cMajor) ++ (interpret 60 cMinor)
  
{-|
Main symbol has type @IO ()@: It is a computation in the IO monad with
no interesting result.
-}
main = do
  prompt
  commandLoop emptyStore

{-|
This version of @main@ demonstrates extraction of command-line arguments: We simply
invoke @getArgs@ which outputs a list of strings and we pattern match against the 
expected number of arguments. In real-life situation, we would want to use a dedicated
utility such as @System.Console.GetOpt@ for handling arguments.
-}
-- main = do
--   [frequency,volume,duration] <- getArgs
--   let f = read frequency :: Int
--   let d = read duration :: Double
--   let a = read volume :: Double
--   outputSound $ computeSound f d a
   
