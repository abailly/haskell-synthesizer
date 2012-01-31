import Test.HUnit

-- first test on sound 
convertAFrequencyToAWave = 
  take 3 (wave frequency samplingRate) ~?= [0.0, 0.1, 0.2]
