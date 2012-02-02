module Music where
import Sound


data Note = A
          deriving (Eq,Ord,Show,Read)

interpret :: Note -> Wave
interpret _ = wave 440
