all: Parsing Wavestream

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *.exe

Sss: Wavestream WavWrite
	ghc --make $@

Wavestream:
	ghc --make $@

WavWrite:
	ghc --make $@
