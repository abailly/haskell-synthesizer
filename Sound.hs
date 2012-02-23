-- One of many possible language extensions
-- Here is the error we get from the compiler when we remove this clause:
--
-- Sound.hs:74:9:
--     Ambiguous type variable `a0' in the constraints:
--       (RealFrac a0) arising from a use of `truncate' at Sound.hs:74:9-16
--       (Num a0) arising from a use of `*' at Sound.hs:74:28
--       (Integral a0) arising from a use of `fromIntegral'
--                     at Sound.hs:36:25-36
--       (Enum a0) arising from the arithmetic sequence `0 .. n'
--                 at Sound.hs:36:47-54
--     Possible cause: the monomorphism restriction applied to the following:
--       computeSound :: forall a.
--                       (Ord a, Floating a) =>
--                       a0 -> a0 -> a -> [a]
--         (bound at Sound.hs:86:1)
--       slice :: forall a. a0 -> [a] -> [a] (bound at Sound.hs:73:1)
--       wave :: forall b. Floating b => a0 -> [b] (bound at Sound.hs:21:1)
--       samplingRate :: a0 (bound at Sound.hs:17:1)
--     Probable fix: give these definition(s) an explicit type signature
--                   or use -XNoMonomorphismRestriction
--     In the expression: truncate
--     In the first argument of `take', namely
--       `(truncate $ seconds * samplingRate)'
--     In the expression:
--       take (truncate $ seconds * samplingRate) repeatWave
--
-- Monomorphism restriction's definition is rather complex (see Haskell Report 4.5.5 for details)
-- but its meaning is quite simple: When a type variable is free within a type expression it
-- cannot be generalized (eg. implicitly universally quantified) even if this would be perfectly legal. Here we do not
-- have an explicit type for @samplingRate@ hence it's type is a variable which occurs free in the reported 
-- context, hence it cannot be generalized and becomes "ambiguous" when used in two places with two different
-- possible types. Another way to  fix the problem would be to declare explicitly a type for sampling rate:
--
-- samplingRate :: (Num a) => a
--

{-# LANGUAGE NoMonomorphismRestriction #-}

{-| A module for low-level sound generation routines. 

By default modules export all the symbols they define.
-}
module Sound where

-- | A simple type alias. 
-- Type aliases are stricly syntactic: The symbol is replaced by its definition before
-- typechecking. Common aliases are 
-- @@
-- type String = [Char]
-- @@
type Wave = [Double]

-- | This a CAF: Constant Applicative Form.
samplingRate = 44000

-- | A function producing a sinusoidal sampling of a given frequency
-- depending on the sampling rate.
wave frequency  = 
  -- @let@ keyword introduces variable definition scope. For all practical purpose
  -- a let scope can contain any definition that would be valid at the toplevel but 
  -- of course the symbols are visible only within the scope of the let environment.
  -- Note that mutually recursive definitions are perfectly legal in a let environment,
  -- just like it is legal at the top-level.
  --
  -- Note here the use of a symbol in infix notation using backquotes. All binary function 
  -- symbols may be used as operators using backquotes. Conversely, all operators may be 
  -- used in prefix form using parens.
  let n = samplingRate `div` frequency
  in map 
     (sin . (* (2 * pi)))  -- a common idiom in Haskell: Using "function pipelines" in so-called 
                           -- point-free notation. The use of a partially applied operators @(* x)@
                           -- is called a _section_.
     [ fromIntegral i / fromIntegral n | i <- [0 .. n]]  -- a list comprehension, similar to a for-loop in Scala


-- |Defining an operator is identical to declaring a function.
-- An operator is any (valid) symbol which does not start with an alphabetic letter. 
-- Note the use of an explicit type declaration which is never mandatory excepts in 
-- situation where there is an ambiguity (for example due to conflicting type-classes existing
-- in scope)
(°) :: Wave -> Wave -> Wave
-- Operator's definition can use infix notation.       
w ° w' = zipWith avg w1 w2
  -- a @where@ clause is similar to a let-environment excepts its scope is the preceding expression.
  where 
    avg a b = (a + b) /2
    w1 = w ++ w1
    w2 = w' ++ w2

-- |A typical definition of a recursive function using pattern-matching.
-- There is a very close match with inductive proofs in mathematics and practically this
-- form is quite useful to prove properties or derive more efficient forms from an existing
-- definition. However the order of clauses is important when they may overlap as this gets translated into
-- a sequence of possibly nested cases. The compiler can however issue warnings when it 
-- it detects overlapping clauses (which could be the case here as 0 is a special case of n).
duplicate :: Int -> [a] -> [a]
-- define the behaviour for base case which usually stops the recursion
duplicate 0 l = l
-- recursively call the function, "reducing" step-by-step its scope until
-- it reaches the base case. 
duplicate n l = l ++ duplicate (n-1) l

-- | This function's definition uses a *guard* to select cases depending on 
-- the value of an expression, not only the structure of the parameters. All 
-- the variables defined in patterns are in scope in the guard.
amplitude ratio | ratio > 0 && ratio < 1 = map (*ratio)
                  -- otherwise is simply an alias for True. 
                | otherwise              = id
                                           
slice seconds wave = 
  take (truncate $ seconds * samplingRate) repeatWave
  where
    -- We take advantage of laziness to define concisely and efficiently a repeating structure 
    -- This expression and each subexpression will be evaluated only if its value is actually
    -- needed at runtime to make progress in the computation (of the top-level main function). 
    repeatWave = wave ++ repeatWave

-- |Scale a list of doubles between -1 and 1 to an integer interval
scale :: (Int,Int) -> [Double] -> [Int]
scale (min,max) (x:xs) = truncate (((x + 1) / 2)  * fromIntegral (max - min)) + min : scale (min,max) xs
scale _         []     = []  

computeSound frequency duration volume = 
  slice duration $ amplitude volume $ wave frequency

