module SoundIO where
import Music
import Sound
import qualified Data.ByteString.Char8 as B

prepareSound = B.pack.map toEnum.scale (0,255)
outputSound  = B.putStr. prepareSound
note (p,o,d) = Note p o d

