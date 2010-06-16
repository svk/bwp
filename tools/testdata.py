import math

print "mydata: ["

for i in range(44100):
    print math.sin(0.001 * i), ","

print "];"
print "myoutput: linear_interpolation{initial=0,data=mydata);"
