#include "Wave.h"

#include <cstdio>

int main(int argc, char *argv[]) {
    SineWave sine ( 10.0 );
    PiecewiseLinearClosedSupport envelope;
    envelope.add( 0.1, 0.8 ).add( 0.2, 1.0 ).add( 0.5, 1.0 ).add( 0.3, 0.0 );

    SineEnvelope vibrato ( 14, 0.9, 0.1 );

    const double dt = 0.001;
    double t = 0;
    for(int i=0;i<3000;i++) {
        t += 0.001;
        printf( "%lf\t%lf\t%lf\t%lf\n", t, sine.advance(dt), envelope.advance(dt), vibrato.advance(dt) );
    }
    return 0;
}
