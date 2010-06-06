CC=g++
CPPFLAGS=-g

clean:
	rm -f *.o

TestWave: TestWave.o Wave.o
TestWaveFile: TestWaveFile.o Wave.o WaveFileExporter.o
