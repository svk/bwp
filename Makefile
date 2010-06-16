all: bwp

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *.exe

bwp: Wavestream WavWrite
	ghc --make $@ -Wall

Wavestream:
	ghc --make $@ -Wall

WavWrite:
	ghc --make $@ -Wall
