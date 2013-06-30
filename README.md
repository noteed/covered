# Covered - Syntax and Haskell Program Coverage highlighting

bin/covered.hs combines the hpc-markup tool (utils/hpc/HpcMarkup.hs in the GHC
source tree) and hscolour (the HTML part).

It can be used on the main.hs example program to produce a Main.hs.html file
with both syntax highlighting and coverage highlighting.

Note that hpc-markup is BSD-licensed but hscolour is GPL.
