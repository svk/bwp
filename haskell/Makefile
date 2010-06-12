all: bwp

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *.exe

bwp: Wavestream WavWrite
	ghc --make $@

Wavestream:
	ghc --make $@

WavWrite:
	ghc --make $@
