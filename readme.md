A toy synthesizer program to demonstrate various Haskell language features. this is 
more a receptacle for exposing pedagogically Haskell's concept and features than a
"Real-World" program, so use with care and feel free to hack it.

To build (only on a recent linux, I think):

    > cabal install
    
To run the web application:

    > websynth
 
Then point your browser at =http://localhost:4000/= 

To run the command-line application:

    > synthesizer
    > > load hp hp
    > > play hp
    
This will load a file named =hp= containing the first few notes of Harry Potter's theme then
play the tune. Note this requires the program aplay to be installed.
    
