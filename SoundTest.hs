import Test.HUnit
import Test.QuickCheck
import Sound
import Music

amplitude_multiply_wave_samples d = 
  amplitude d wave' == map (*d) wave'
  where 
    wave' = wave 440
  
-- first test on sound 
convert_a_frequency_to_a_wave = 
  take 3 (wave frequency) ~?= [0.0,6.279051952931337e-2,0.12533323356430426]
  where
    frequency    = 440

slice_a_wave_for_a_given_number_of_seconds = 
  length (slice seconds aWave) ~?=  88000
  where
    seconds = 2 
    aWave   = wave 440

scale_wave_to_a_single_byte_value =
  take 3 (scale (0,255) aWave) ~?= [127,135,143]
  where
    aWave = wave 440

convert_note_to_signal = TestList [
  length (interpret allegro a4crotchet) ~?= (60 * samplingRate `div` 80),
  length (interpret allegro a4minim) ~?= 2 * (60 * samplingRate `div` 80),
  length (interpret largo a4crotchet) ~?= 2 * (60 * samplingRate `div` 80),
  take 3 (interpret largo c4crotchet) ~?= take 3 (wave 261)
  ]
  where
    c4crotchet = Note C 4 Crotchet
    a4crotchet = Note A 4 Crotchet
    a4minim    = Note A 4 Minim
    
tests = [ convert_a_frequency_to_a_wave, 
          slice_a_wave_for_a_given_number_of_seconds,
          scale_wave_to_a_single_byte_value,
          convert_note_to_signal]
        
runAllTests = runTestTT $ TestList tests
