import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Sound

outputSound = B.putStr.B.pack.map fromIntegral.scale (0,255)
   
main = do
  [frequency,volume,duration] <- getArgs
  let f = read frequency :: Int
  let d = read duration :: Int
  let a = read volume :: Double
  outputSound $ computeSound f d a
   
