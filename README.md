# Covered - Syntax and Haskell Program Coverage highlighting

bin/covered.hs combines the hpc-markup tool (utils/hpc/HpcMarkup.hs in
the GHC source tree) and hscolour (the HTML part).

It can be used on the main.hs example program to produce a
Main.hscovered.html file with both syntax highlighting and coverage
highlighting.

(The regular Main.hs.html and an additional pure hscolour
Main.hscolour.html files are also produced.)

Note that hpc-markup is BSD-licensed but hscolour is GPL.
