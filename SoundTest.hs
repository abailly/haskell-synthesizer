import Test.HUnit
import Sound

-- first test on sound 
convert_a_frequency_to_a_wave = 
  take 3 (wave frequency samplingRate) ~?= [0.0,6.283185307179587e-2,0.12566370614359174]
  where
    frequency    = 440
    samplingRate = 44000

slice_a_wave_for_a_given_number_of_seconds = 
  length (slice seconds samplingRate aWave) ~?=  88000
  where
    seconds = 2 
    samplingRate = 44000
    aWave   = wave 440 samplingRate

tests = [ convert_a_frequency_to_a_wave, 
          slice_a_wave_for_a_given_number_of_seconds]
        
runAllTests = runTestTT $ TestList tests
