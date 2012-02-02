module Music where
import Sound

type Octave = Int
type Tempo = Int

-- twelve half-tones form a chromatic scale
data Pitch = C | Cs |
             D | Ds | 
             E | 
             F | Fs |
             G | Gs | 
             A | As |
             B
          deriving (Eq,Ord,Show,Read,Enum)

-- see http://en.wikipedia.org/wiki/Note for formula
frequency p = truncate $ 2 ** (fromIntegral (fromEnum p - fromEnum A) / 12) * 440

-- note values (in british notation)
data Duration =  Semiquaver |
                 Quaver     |
                 Crotchet   | 
                 Minim      | 
                 Semibreve  |
                 Breve
              deriving(Eq,Ord,Show,Read)
                      
data Note = Note Pitch Octave Duration
          deriving (Eq,Ord,Show,Read)

-- tempi in bpm
allegro = 80 :: Int
largo   = 40 :: Int
                
interpret :: Tempo -> Note -> Wave
interpret tempo (Note p o Crotchet) = slice (60.0 / fromIntegral tempo) $ wave (frequency p)
interpret tempo (Note p o Minim) = slice (2 * 60.0 / fromIntegral tempo) $ wave (frequency p)


