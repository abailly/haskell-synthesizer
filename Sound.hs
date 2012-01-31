module Sound where

-- this a CAF: Constant Applicative Form
samplingRate = 44000 :: Int

-- a sinusoidal wave between -1 and +1 
wave frequency  = 
  let n = samplingRate `div` frequency
  in map (sin . (* (2 * pi))) [ fromIntegral i / fromIntegral n | i <- [0 .. n]]

amplitude ratio | ratio > 0 && ratio < 1 = map (*ratio)
                | otherwise              = id
                                           
slice seconds wave = 
  take (samplingRate * seconds) repeatWave
  where
    repeatWave = wave ++ repeatWave

scale :: (Int,Int) -> [Double] -> [Int]
scale (min,max) (x:xs) = truncate (((x + 1) / 2)  * fromIntegral (max - min)) + min : scale (min,max) xs
scale _         []     = []  

computeSound frequency duration volume = 
  slice duration $ amplitude volume $ wave frequency
