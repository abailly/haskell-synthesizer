# Playing music on Linux


[Getting started with MIDI](http://www.lesbell.com.au/Home.nsf/b8ec57204f60dfcb4a2568c60014ed0f/c4b39482154feb03ca256f8100150ad9?OpenDocument) was my primary source: It has detailed
instructions on how to get MIDI played on a Linux box. 

Install a midi file player. This actually came out-of-the-box in my
linux distro (Xubuntu 11.10):
   
~~~~
$ sudo apt-get install aplaymidi
~~~~

Install a software MIDI synthesizer (I don't have and don't know how
to use a hardware synthesizer). this will install both a command-line
and a Qt-based synthesizer called =qsynth=.

~~~~
$ sudo apt-get install fluidsynth
~~~~

Install [[http://jackaudio.org/][Jackaudio daemon]] software, seems useful to route output of
MIDI synthesizer to actual hardware soundcard and abstract away from
the gory details of sound playing on linux and other platforms. There
is a an accompanying QT-based controller:

~~~~
$ sudo apt-get install jackd qjacktctl
~~~~

Optionally, install [[http://www.rosegardenmusic.com/][rosegarden]], a sequencer for creating, recording
and playing soundtracks and music. 

# Creating music with Haskell

 - Got the idea reading [this blog entry](http://joaopizani.hopto.org/en/2012/01/haskell-synth/)
 - This of course reminded me of Paul Hudak's [Haskell School of Expression](http://www.cs.yale.edu/homes/hudak/SOE/) which is a great book to learn Haskell and contains a
   section on music based on Haskore

## Making Noise

First, we want to be able to make some sound. What is sound? Sound is
simply a stream of binary data that is sent to some audio device
sampling a sound wave. So we need a sampling rate (in Hz) which is the
number of samples per second, here a CD quality rate:

> samplingRate = 44100 

Now, let's make a sound. We will use the
[Karplus-Strong](http://en.wikipedia.org/wiki/Karplus%E2%80%93Strong_string_synthesis)
algorithm to synthesize a sound mimicking a string (eg. a guitar): 

> ks freq noise = s
>  where s = 
>          take (samplingRate `div` freq) noise 

The algorithm works as a feedback loop that takes some *white
noise* (eg. random noise) during an initial period then feed it to
some filter. 

>          ++ loPass s

Here the filter is a so-called low-pass filter that averages two
successive samples, that produces a plucked-string sound:

> loPass (x:y:xs) = 0.5*(x+y):loPass (y:xs)
>

We can also use another filter that shall produced a drum sound, using
some random stream to select the averaging coefficient:

> drum (x:y:xs) (r:rs) | r < 0.5   = 0.5*(x+y): drum (y:xs) 
>                      | otherwise = -0.5*(x+y): drum (y:xs) 


> toByteString = BS.pack . map (fromIntegral . floor . (255 *))
>
> main = do
>  g <- getStdGen
>  BS.putStr $ slice 2 $ toByteString $ ks 440 $ randomRs (0.0 :: Double, 1.0) g



## Music Theory Crash Course


A single note is a pitch, an octave and a duration which is a fraction of the basic *tempo*:

> data Note = Note Pitch Octave Duration 
>
> type Octave = Int
> 
> type Duration = Int
> 
> data Pitch = C | C♯ | 
>              D♭ | D | D♯ | 
>              E♭ | E | E♯ |
>              F♭ | F | F♯ |
>              G♭ | G | G♯ |
>              A♭ | A | A♯ |
>              B♭ | B | B♯ |
>              C♭ 
>            deriving (Eq, Show, Read)

To make sound from a note, we need to derive a sequence of bytes representing the soundwave, encoded in RAW format to send to our sound player.
