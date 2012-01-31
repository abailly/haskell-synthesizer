module Sound where

-- this a CAF: Constant Applicative Form
samplingRate = 44000 :: Int

wave frequency samplingRate = 
  let n = samplingRate `div` frequency
  in map (sin . (* (2 * pi))) [ fromIntegral i / fromIntegral n | i <- [0 .. n]]

slice seconds samplingRate wave = 
  take (samplingRate * seconds) repeatWave
  where
    repeatWave = wave ++ repeatWave

scale :: (Int,Int) -> [Double] -> [Int]
scale (min,max) (x:xs) = truncate (x * fromIntegral (max - min)) + min : scale (min,max) xs
