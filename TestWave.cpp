#include "Wave.h"

#include <cstdio>

int main(int argc, char *argv[]) {
    SineWave sine ( 1.0 );
    SquareWave square ( 1.0 );
    SawtoothWave saw (1.0 );

    const double dt = 0.001;
    double t = 0;
    for(int i=0;i<3000;i++) {
        t += 0.001;
        printf( "%lf\t%lf\t%lf\t%lf\n", t, sine.advance(dt), square.advance(dt), saw.advance(dt) );
    }
    return 0;
}
