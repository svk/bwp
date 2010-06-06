CC=g++
CPPFLAGS=-g

all: TestWave TestWaveFile

clean:
	rm -f *.o

TestWave: TestWave.o Wave.o
	$(CC) -o $@ $^

TestWaveFile: TestWaveFile.o Wave.o WaveFileExporter.o
	$(CC) -o $@ $^
