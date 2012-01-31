module Sound where

-- this a CAF: Constant Applicative Form
samplingRate = 44000

wave frequency samplingRate = 
  let n = samplingRate `div` frequency
  in map (sin . (* (2 * pi))) [ fromIntegral i / fromIntegral n | i <- [0 .. n]]

slice seconds samplingRate wave = 
  take (samplingRate * seconds) repeatWave
  where
    repeatWave = wave ++ repeatWave
