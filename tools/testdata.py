import math

print "mydata: ["

for i in range(44100):
    print "\t(%lf,%lf)," % ((1/44100.), math.sin(0.001 * i))


print "(0.0,0.0)];"
print "myoutput: linear_interpolation{initial=0,data=mydata};"
