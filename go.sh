#! /bin/sh
rm -rf main main.o main.hi main.tix .hpc bin/covered bin/covered.o bin/covered.hi
rm -rf alternatives.html definitions.html expressions.html index.html
rm -rf Main.hs*.html
ghc --make -fhpc main.hs || exit 1
ghc --make -Wall bin/covered.hs || exit 1
./main
bin/covered main
