language = hs
dir      = $(shell basename $(CURDIR))
number   = $(shell echo $(dir) | cut -d'p' -f 2)
optimize = -O2
linking  = --make
warnings = -w -W -Wall -Werror -fwarn-duplicate-exports -fwarn-implicit-prelude -fwarn-incomplete-patterns -fwarn-missing-signatures -fwarn-overlapping-patterns -fwarn-tabs -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches -fwarn-unused-do-bind
name     = ans-$(number)
source   = s$(number).$(language)

all: $(name)

$(name): $(source)
	ghc $< -o $@ $(optimize) $(linking) $(warnings)
clean:
	rm *.o *.hi
