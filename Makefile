CC=g++
CPPFLAGS=-g

all: TestWave TestWaveFile

clean:
	rm -f *.o
	rm TestWave.exe
	rm TestWaveFile.exe

TestWave: TestWave.o Wave.o
	$(CC) -o $@ $^

TestWaveFile: TestWaveFile.o Wave.o WaveFileExporter.o
	$(CC) -o $@ $^
