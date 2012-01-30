{-# LANGUAGE NoMonomorphismRestriction #-}
import qualified Data.ByteString.Lazy as BS
import System.Environment (getArgs)
import System.Random

samplingRate = 44100

-- | The codomain for the samples
codomain :: (Int, Int)
codomain = (-128,128)

-- | Simple waveform datatype
data Waveform = Sine | Square
    deriving (Eq, Show, Read)

-- | Normalized half duty cycle square function.
-- Dom(square) = [0,1], Im(square) = {-1,1}
square :: Float -> Float
square x
    | x < 0.5   = -1.0
    | otherwise =  1.0


-- | Normalizes an interval [0,n] to [0,1]
normalizeDom :: Int -> [Float]
normalizeDom n = [(fromIntegral i) / n_ | i <- [0..n]]
    where n_ = fromIntegral n


-- | Transforms [0,n] into [0,2*pi]
zeroTo2Pi :: Int -> [Float]
zeroTo2Pi n = map (* (2*pi)) (normalizeDom n)


-- | Scale a normal interval (between -1 and 1) back to a given codomain
scale :: (Int, Int) -> [Float] -> [Int]
scale (low, high) normal = [round (low_ + d*x) | x <- zeroTo2]
    where zeroTo2 = map (+ 1) normal
          d = (high_ - low_) / 2
          low_ = fromIntegral low
          high_ = fromIntegral high

-- | Get a slice of an infinite signal with n seconds
slice :: Float -> BS.ByteString -> BS.ByteString
slice time = BS.take (truncate (fromIntegral samplingRate * time))


-- | Makes a continuous (infinite) signal for a given frequency
signal freq = cycleAndPack (singlePeriod freq)
    where
        singlePeriod 0 = silence
        singlePeriod f = period (truncate $ fromIntegral samplingRate / f) Sine


-- | Converts a [Int] to a Lazy ByteString and then cycles it
cycleAndPack :: [Int] -> BS.ByteString
cycleAndPack = BS.cycle . BS.pack . (map fromIntegral) 


-- | Synthesizes one period
period :: Int -> Waveform -> [Int]
period n Square = scale codomain $ map square (normalizeDom n)
period n Sine = scale codomain $ map sin (zeroTo2Pi n)


-- | Synthesizes one small silence, one sample :)
silence :: [Int]
silence = [0]

ks freq b g noise = s
  where s = 
          take (samplingRate `div` freq) noise
--          (map ( / (2*pi)) $ zeroTo2Pi (samplingRate `div` freq))
          ++ drum b s (randomRs (0.0, 1.0) g)
--          ++ loPass s

drum b (x:y:xs) (r:rs) | r > b     = 0.5*(x+y): drum b (y:xs) rs
                       | otherwise = -0.5*(x+y): drum b (y:xs) rs
                           
loPass (x:y:xs) = 0.5*(x+y):loPass (y:xs)

toByte = map (fromIntegral . floor . (255 *) . (\x -> (x+1.0) / 2))

toByteString = BS.pack . toByte

main = do
  [b,p] <- getArgs
  g <- getStdGen
  g' <- newStdGen
  BS.putStr $ toByteString $ ks (read p :: Int) (read b :: Double) g' $ randomRs (-1.0 :: Double, 1.0) g
--  BS.putStr $ BS.concat $ map (slice 0.2 . signal) [ 220 / (8 - i)  + 220 | i <- [1..7] ] 

