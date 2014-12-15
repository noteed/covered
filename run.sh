#! /usr/bin/env bash

docker run \
  -v `pwd`:/source \
  -t -i images.reesd.com/reesd/stack-hscolour \
  sh -c 'cd /source ; ./go.sh'
