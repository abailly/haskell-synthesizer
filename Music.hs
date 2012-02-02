module Music where
import Sound

type Octave = Int

data Pitch = A
          deriving (Eq,Ord,Show,Read)

data Note = Note Pitch Octave
          deriving (Eq,Ord,Show,Read)

interpret :: Note -> Wave
interpret _ = wave 440

