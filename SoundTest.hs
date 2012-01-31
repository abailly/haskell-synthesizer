import Test.HUnit
import Sound

-- first test on sound 
convert_a_frequency_to_a_wave = 
  take 3 (wave frequency samplingRate) ~?= [0.0,6.279051952931337e-2,0.12533323356430426]
  where
    frequency    = 440

slice_a_wave_for_a_given_number_of_seconds = 
  length (slice seconds samplingRate aWave) ~?=  88000
  where
    seconds = 2 
    aWave   = wave 440 samplingRate

scale_wave_to_a_single_byte_value =
  take 3 (scale (0,255) wave) ~?= [0,1,2]
  where
    aWave = wave 440 samplingRate

tests = [ convert_a_frequency_to_a_wave, 
          slice_a_wave_for_a_given_number_of_seconds]
        
runAllTests = runTestTT $ TestList tests
