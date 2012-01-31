module Sound where

wave frequency samplingRate = 
  let n = samplingRate `div` frequency
  in map (* (2 * pi)) [ fromIntegral i / fromIntegral n | i <- [0 .. n]]

slice seconds samplingRate wave = 
  take (samplingRate * seconds) repeatWave
  where
    repeatWave = wave ++ repeatWave
