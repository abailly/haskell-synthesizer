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
data Duration =  Pointed Duration |
                 Semiquaver |
                 Quaver     |
                 Crotchet   | 
                 Minim      | 
                 Semibreve  |
                 Breve
              deriving (Eq,Ord,Show,Read)

data Note = Note { 
  pitch    :: Pitch, 
  octave   :: Octave, 
  duration :: Duration
  } deriving (Eq,Ord,Show,Read)

data Chord = Chord [Note] Duration
           deriving (Eq,Ord,Show,Read)

-- tempi in bpm
allegro = 80 :: Int
largo   = 40 :: Int

-- see http://en.wikipedia.org/wiki/Note for formula
frequency p = truncate $ 2 ** (fromIntegral (fromEnum p - fromEnum A) / 12) * 440

value Semiquaver  = 1/4 
value Quaver      = 1/2 
value Crotchet    = 1
value Minim       = 2
value Semibreve   = 4 
value Breve       = 8
value (Pointed d) = value d * 1.5

chord :: Note -> Note -> Chord
chord n n' = Chord [n,n'] (max (duration n) (duration n'))

playChord :: Tempo -> Chord -> Wave
playChord tempo (Chord ns d) = slice (durationInSeconds tempo d) $ foldl1 (°) (map (interpret tempo) ns)

interpret :: Tempo -> Note -> Wave
interpret tempo (Note p o d) = slice t $  wave f
  where
    t = durationInSeconds tempo d
    f = truncate (fromIntegral (frequency p) * (2 ** fromIntegral (o - 4)))

durationInSeconds tempo d = value d * 60.0 / fromIntegral tempo
