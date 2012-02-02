import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Sound
import Music

prepareSound = B.pack.map fromIntegral.scale (0,255)
outputSound  = B.putStr. prepareSound

note p = Note p 4 Crotchet

main = do
  B.putStr $ B.concat $ map (prepareSound.interpret 120.note) [C,D,E,C,D,D,E,F,F,E,E,C,D,E,C,D,D,E,F,G,C]

-- main = do
--   [frequency,volume,duration] <- getArgs
--   let f = read frequency :: Int
--   let d = read duration :: Double
--   let a = read volume :: Double
--   outputSound $ computeSound f d a
   
