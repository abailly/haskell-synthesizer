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

-- note values (in british notation)
data Duration =  Semiquaver |
                 Quaver     |
                 Crotchet   | 
                 Minim      | 
                 Semibreve  |
                 Breve
              deriving (Eq,Ord,Show,Read)

data Note = Note Pitch Octave Duration
          deriving (Eq,Ord,Show,Read)

-- tempi in bpm
allegro = 80 :: Int
largo   = 40 :: Int

-- see http://en.wikipedia.org/wiki/Note for formula
frequency p = truncate $ 2 ** (fromIntegral (fromEnum p - fromEnum A) / 12) * 440

value Semiquaver = 1/4 
value Quaver     = 1/2 
value Crotchet   = 1
value Minim      = 2
value Semibreve  = 4 
value Breve      = 8

interpret :: Tempo -> Note -> Wave
interpret tempo (Note p o d) = slice t $  wave f
  where
    t = value d * 60.0 / fromIntegral tempo
    f = truncate (fromIntegral (frequency p) * (2 ** fromIntegral (o - 4)))
