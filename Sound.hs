{-# LANGUAGE NoMonomorphismRestriction #-}
module Sound where

type Wave = [Double]

-- this a CAF: Constant Applicative Form
samplingRate = 44000

-- a sinusoidal wave between -1 and +1 
wave frequency  = 
  let n = samplingRate `div` frequency
  in map (sin . (* (2 * pi))) [ fromIntegral i / fromIntegral n | i <- [0 .. n]]

(°) :: Wave -> Wave -> Wave
w ° w' = zipWith avg w1 w2
  where 
    avg a b = (a + b) /2
    w1 = w ++ w1
    w2 = w' ++ w2

duplicate :: Int -> [a] -> [a]
duplicate 0 l = l
duplicate n l = l ++ duplicate (n-1) l

amplitude ratio | ratio > 0 && ratio < 1 = map (*ratio)
                | otherwise              = id
                                           
slice seconds wave = 
  take (truncate $ seconds * samplingRate) repeatWave
  where
    repeatWave = wave ++ repeatWave

-- |Scale a list of doubles between -1 and 1 to an integer interval
scale :: (Int,Int) -> [Double] -> [Int]
scale (min,max) (x:xs) = truncate (((x + 1) / 2)  * fromIntegral (max - min)) + min : scale (min,max) xs
scale _         []     = []  

computeSound frequency duration volume = 
  slice duration $ amplitude volume $ wave frequency
