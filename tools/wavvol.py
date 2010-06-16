import sys, wave, struct

def sixteen_bit_sample(a):
    return struct.unpack( "<h", a )[0]

def vol( window ):
    return max( map( abs, window ) )


wavname = sys.argv[1]
windowsize = int(sys.argv[2])

f = wave.open( wavname, "rb" )

samplerate = f.getframerate()
channels = f.getnchannels()
assert channels == 1
frames = f.getnframes()
assert f.getsampwidth() == 2

print "mydata:["
dt = 1/float(samplerate)

maxv, minv = -2**32, 2**32
window = []
first = True
for i in range(windowsize):
    window.append( sixteen_bit_sample ( f.readframes(1) ) )
    frames -= 1
while frames > 0:
    v = vol( window )
    window.pop(0)
    window.append( sixteen_bit_sample ( f.readframes(1) ) )
    frames -= 1
    print "\t%s(%lf,%lf)" % ("," if not first else "", dt, v)
    first = False
    maxv = max(maxv,v)
    minv = min(minv,v)
f.close()

print "]; #max = %lf, min = %lf"  % (maxv, minv)
print "mydatatest: sine{freq=440} * (linear_interpolation{initial=0,data=mydata} * %lf);" % (1./float(maxv))
