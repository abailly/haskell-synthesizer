import System.Environment (getArgs)
import Sound
import Music
import SoundIO

simplenote p = Note p 4 Crotchet

harrypotter = [(B,4,Crotchet), 
               (E,5,Pointed Crotchet), 
               (G,5,Quaver), 
               (Fs,5,Crotchet), 
               (E,5,Minim), 
               (B,5,Crotchet), 
               (A,5,Minim), 
               (Fs,5,Minim),
               (E,5,Pointed Crotchet), 
               (G,5,Quaver), 
               (Fs,5,Crotchet), 
               (Ds,5,Minim), 
               (F,5,Crotchet), 
               (B,4,Semibreve)]
              
cMajor = Chord (map note [(C,4,Crotchet), 
                          (E,4,Crotchet), 
                          (G,4,Crotchet)]) Crotchet

cMinor = Chord (map note [(C,4,Crotchet), 
                          (Ds,4,Crotchet), 
                          (G,4,Crotchet)]) Crotchet

-- main = do
--   outputSound $ (interpret 60 cMajor) ++ (interpret 60 cMinor)
  
main = do
  playSound (map note harrypotter)

-- main = do
--   [frequency,volume,duration] <- getArgs
--   let f = read frequency :: Int
--   let d = read duration :: Double
--   let a = read volume :: Double
--   outputSound $ computeSound f d a
   
