import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Sound

main = do
  [frequency,volume,duration] <- getArgs
  let f = read frequency :: Int
  let d = read duration :: Int
  let a = read volume :: Double
  B.putStr $ (B.pack . (map fromIntegral) . scale (0,255) . slice d . amplitude a) $ wave f
