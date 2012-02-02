module Music where
import Sound

type Octave = Int
type Tempo = Int

data Pitch = A
          deriving (Eq,Ord,Show,Read)

data Duration = Black |
                White
              deriving(Eq,Ord,Show,Read)
                      
data Note = Note Pitch Octave Duration
          deriving (Eq,Ord,Show,Read)

-- tempi in bpm
allegro = 80 :: Int

interpret :: Tempo -> Note -> Wave
interpret tempo (Note p o Black) = slice (60.0 / fromIntegral tempo) $ wave 440
interpret tempo (Note p o White) = slice (2 * 60.0 / fromIntegral tempo) $ wave 440

