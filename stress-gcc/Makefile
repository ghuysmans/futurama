CFLAGS+=-O4

all: dance.c part2

dance.c: ../day16.txt
	dune exec ../bin/compiler.exe <$< >$@

clean:
	rm -f dance.c part1 part2
