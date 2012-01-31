import System.Environment (getArgs)
import Sound

main = do
  [frequency,duration] <- getArgs
  let f = read frequency :: Int
  let d = read duration :: Int
  putStrLn $ show (map show $ scale (0,255) $ slice d samplingRate $ wave f samplingRate)
