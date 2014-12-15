# Covered - Syntax and Haskell Program Coverage highlighting

bin/covered.hs combines the hpc-markup tool (utils/hpc/HpcMarkup.hs in the GHC
source tree) and hscolour (the HTML part).

It can be used on the main.hs example program to produce a Main.hs.html file
with both syntax highlighting and coverage highlighting.

Note that hpc-markup is BSD-licensed but hscolour is GPL.

## Docker images

To build Covered, I am using a Docker image. A Dockerfile is provided in this
repository. The image depends on the [Reesd stack
image](https://github.com/noteed/reesd-stack) and can be built with

    > docker build -t images.reesd.com/reesd/stack-hscolour images/stack-hscolour

(Rename the image and the `FROM` instruction as appropriate.)
