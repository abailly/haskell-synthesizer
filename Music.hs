module Music where
import Sound

type Octave = Int
type Tempo = Int

-- | @data@ keyword introduces a new concrete data-type and its set of 
-- constructors. Here we define a simple enumerated type consisting in
-- the twelve notes of the chromatic scale. Note that the lexer/parser 
-- of Haskell imposes the following restrictions on identifiers:
--   
--  * Types and Constructors identifiers must start with an upper-case letter
--  * Functions and variables identifiers must start with a lower-case letter
--  * Types and Constructor operators must start with a colon (:)
data Pitch = C | Cs |
             D | Ds | 
             E | 
             F | Fs |
             G | Gs | 
             A | As |
             B
             -- a deriving clause provides automatic derivation of the functions 
             -- defined in the corresponding type-class. This derivation is by 
             -- default restricted to a predefined set of standard type classes 
             -- provided in the Prelude (or the compiler) but it can be extended
             -- using some language extensions. 
          deriving (Eq,Ord,Show,Read,Enum)

-- | Note values (in british notation).
-- Here we use a recursive constructor for @Pointed Duration@. Technically this
-- kind of objects are called *products* as they represent the cartesian product 
-- of the possible values of the composed types (sums are simply the enumeration
-- of all the constructors).
data Duration =  Pointed Duration |
                 Semiquaver |
                 Quaver     |
                 Crotchet   | 
                 Minim      | 
                 Semibreve  |
                 Breve
              deriving (Eq,Ord,Show,Read)


-- | A Note defined using records-notation.
-- A Haskell record is similar to a product-type but provides syntactic sugar to
-- introduce accessors for components (eg. attributes, fields) of the type. Within patterns 
-- one can use the product notation or explicitly match against named fields of the record.
-- For example, the following fragment:
-- @
-- f Note { pitch = p } = ...
-- @
-- binds the @pitch@ value of a parameter of type Node to p, ignoring other fields.
data Note = Note { 
  pitch    :: Pitch, 
  octave   :: Octave, 
  duration :: Duration
  } deriving (Eq,Ord,Show,Read)

-- | A chord could be alternatively defined as another form of Note, using a different
-- constructor. 
data Chord = Chord [Note] Duration
           deriving (Eq,Ord,Show,Read)

-- | A type-class declaration.
-- Type-classes can be thought of as both:
-- 
--   1. An interface (in the Java sense) defining some related functions over a specific
--      type,
--
--   2. A constraint over the possible types during type inference: Using a function 
--      defined in a type-class effectively restricts the possible types occuring as 
--      arguments or return values of this function to some member of type-class which
--      has consequences for callers.
--
-- Here we use the "simple" Haskell 98 syntax with a single variable.
class Playable p where
  interpret :: Tempo -> p -> Wave
  
-- tempi in bpm
allegro = 80 :: Int
largo   = 40 :: Int

-- see http://en.wikipedia.org/wiki/Note for formula
frequency p = truncate $ 2 ** (fromIntegral (fromEnum p - fromEnum A) / 12) * 440

-- |Define value function as an enumerated case over possible instances of Duration.
-- Note the *tabular* structure of this definition.
value Semiquaver  = 1/4 
value Quaver      = 1/2 
value Crotchet    = 1
value Minim       = 2
value Semibreve   = 4 
value Breve       = 8
value (Pointed d) = value d * 1.5

chord :: Note -> Note -> Chord
chord n n' = Chord [n,n'] (max (duration n) (duration n'))

-- |A possible instance of Playable for Chord type.
instance Playable Chord where
  interpret tempo (Chord ns d) = slice (durationInSeconds tempo d) $ foldl1 (°) (map (interpret tempo) ns)

instance Playable Note where                                                                             
  interpret tempo (Note p o d) = slice t $  wave f
    where
      t = durationInSeconds tempo d
      f = truncate (fromIntegral (frequency p) * (2 ** fromIntegral (o - 4)))

durationInSeconds tempo d = value d * 60.0 / fromIntegral tempo
