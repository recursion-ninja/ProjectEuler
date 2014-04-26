#!/bin/bash

# Save only:
#  - This file
#  - Problem directories
#  - Haskell source files (.hs) [in problem directories]
#  - Makefiles                  [in problem directories]
#  - Input files (.data)        [in problem directories]
# Delete all other files & directories in CWD and problem directories

shopt -s extglob # enable the pattern matching

rm -frv !(`basename $0`|*p[0-9][0-9][0-9][0-9]) *p[0-9][0-9][0-9][0-9]/!(*.c|makefile|*.data)
