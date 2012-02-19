module Sound where

wave frequency samplingRate = 
  let n = samplingRate / frequency
  in map (* (2 * pi)) [ i / n | i <- [0 .. n]]
