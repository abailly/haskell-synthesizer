import Test.HUnit
import Sound

-- first test on sound 
convertAFrequencyToAWave = 
  take 3 (wave frequency samplingRate) ~?= [0.0,6.283185307179587e-2,0.12566370614359174]
  where
    frequency    = 440
    samplingRate = 44000
