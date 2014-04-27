language = hs
dir      = $(shell basename $(CURDIR))
number   = $(shell echo $(dir) | cut -d'p' -f 2)
optimize = -O2
linking  = --make
warnings = -Wall -W -w
name     = ans-$(number)
source   = s$(number).$(language)

all: $(name)

$(name): $(source)
	ghc $< -o $@ $(optimize) $(linking) $(warnings)
clean:
	rm *.o *.hi
