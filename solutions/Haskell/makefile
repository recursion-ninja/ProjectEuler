NUM      = 1
number   = $(shell printf "%04d" $(NUM))
language = hs
optimize = -O2
linking  = --make
main-mod = ProjectEuler/Problem_$(number)
main-is  = -main-is ProjectEuler.Problem_$(number).main
warnings = -w -W -Wall -Werror
sanity   = -fwarn-duplicate-exports -fwarn-incomplete-patterns -fwarn-missing-signatures -fwarn-overlapping-patterns -fwarn-tabs -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches -fwarn-unused-do-bind
name     = ans-$(number)
source   = $(main-mod).$(language)

all: $(name)

$(name): $(source)
	ghc $< -o $@ $(optimize) $(linking) $(main-is) $(warnings) $(sanity)
clean:
	rm -fv $(main-mod).o $(main-mod).hi $(main-mod)/*.o $(main-mod)/*.hi
