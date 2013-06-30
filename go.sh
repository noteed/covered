#! /bin/sh
cabal clean
rm -rf main main.o main.hi main.tix .hpc Main.hs.html
ghc --make -fhpc main.hs || exit 1
cabal configure && cabal build || exit 1
./main
dist/build/covered/covered markup main
