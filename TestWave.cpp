#include "Wave.h"

#include <cstdio>

int main(int argc, char *argv[]) {
    SineWave sine ( 10.0 );
    PiecewiseLinearClosedSupport envelope;
    envelope.add( 0.1, 0.8 ).add( 0.2, 1.0 ).add( 0.5, 1.0 ).add( 0.3, 0.0 );

    SineEnvelope vibrato ( 14, 0.9, 0.1 );

    ProductWave envelope_prime = ProductWave().add( envelope ).add( vibrato );

    SoundWave sound ( sine, envelope_prime );

    TranslatedWave alpha ( sound, 0.0 );
    TranslatedWave beta ( sound, 0.3 );
    TranslatedWave gamma ( sound, 2.0 );
    SumWave allwaves;
    allwaves.add( alpha ).add( beta ).add( gamma );

    for(double t = 0; t < 3.0; t += 0.001 ) {
        printf( "%lf\t%lf\t%lf\n", t, allwaves.sample(t), beta.sample( t ) );
    }
    return 0;
}
