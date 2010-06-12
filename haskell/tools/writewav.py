import sys, struct

i, o = sys.stdin, sys.stdout

samplerate = 44100
channels = 2

samples = [ float(line.split()[1]) for line in i ]

o.write( struct.pack( "<4si4s4sihhiihh4si",
                      "RIFF",
                      4 + 24 + 8 + len(samples) * 2 * channels,
                      "WAVE",
                      "fmt ",
                      16,
                      1,
                      channels,
                      samplerate,
                      2 * channels * samplerate,
                      2 * channels,
                      16,
                      "data",
                      len( samples ) * 2 * channels ) )
for sample in samples:
    sample = min( 1.0, max( -1.0, sample ))
    sample = int( 32767 * sample + 0.5 )
    for j in range(channels):
        o.write( struct.pack( "<h", sample ) )

