module Sound where

wave frequency samplingRate = 
  let n = samplingRate / frequency
  in map (* (2 * pi)) [ i / n | i <- [0 .. n]]

-- -- | Normalizes an interval [0,n] to [0,1]
-- normalizeDom :: Int -> [Float]
-- normalizeDom n = [(fromIntegral i) / n_ | i <- [0..n]]
--     where n_ = fromIntegral n


-- -- | Transforms [0,n] into [0,2*pi]
-- zeroTo2Pi :: Int -> [Float]
-- zeroTo2Pi n = map (* (2*pi)) (normalizeDom n)


-- -- | Synthesizes one period
-- period :: Int -> Waveform -> [Int]
-- period n Square = scale codomain $ map square (normalizeDom n)
-- period n Sine = scale codomain $ map sin (zeroTo2Pi n)
