module Sound where

wave :: Integer -> Integer -> [Double]
wave frequency samplingRate = 
  let n = samplingRate `div` frequency
  in map (* (2 * pi)) [ fromIntegral i / fromIntegral n | i <- [0 .. n]]
