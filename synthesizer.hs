import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Sound
import Music

prepareSound = B.pack.map fromIntegral.scale (0,255)
outputSound  = B.putStr. prepareSound

simplenote p = Note p 4 Crotchet

note (p,o,d) = Note p o d

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
              
cMajor = Chord (map note [(C,4,Crotchet), (E,4,Crotchet), (G,4,Crotchet)])

main = do
  outputSound $ playChord 160 cMajor
  
-- main = do
--   B.putStr $ B.concat $ map (prepareSound.interpret 160.note) harrypotter

-- main = do
--   [frequency,volume,duration] <- getArgs
--   let f = read frequency :: Int
--   let d = read duration :: Double
--   let a = read volume :: Double
--   outputSound $ computeSound f d a
   
