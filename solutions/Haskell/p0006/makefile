language = hs
number   = 0006
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
