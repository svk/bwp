#include "Wave.h"
#include "WaveFileExporter.h"

#include <cstdio>

int main(int argc, char *argv[]) {
    PiecewiseLinearClosedSupport envelope;
    envelope.add( 0.05, 0.8 ).add( 0.1, 0.6 ).add( 0.1, 0.6 ).add( 0.15, 0.1 ).add( 0.15, 0.0 );

    SineEnvelope vibrato ( 10.0, 0.8, 0.2 );
    ProductWave envelope_prime;
    envelope_prime.add( envelope ).add( vibrato );


    SineWave sine440 ( 440.0 );
    SoundWave sound440 ( sine440, envelope_prime );

    SineWave sine660 ( 660.0 );
    SoundWave sound660 ( sine660, envelope_prime );

    TranslatedWave beta ( sound440, 0.3 );
    TranslatedWave gamma ( sound660, 0.6 );


    ExponentialFadeoutEnvelope fadeout ( 2.0 );
    CutoffEnvelope cutoff ( 0.0, 3.0 );
    ProductWave fadeout_sound;
    fadeout_sound.add( cutoff ).add( fadeout ).add( vibrato ).add( sine440 );
    TranslatedWave alpha ( fadeout_sound, 1.5 );
    
    SumWave allwaves;
    allwaves.add( beta );
    allwaves.add( gamma );
    allwaves.add( alpha );

    WaveFileExporter exporter = WaveFileExporter().setRate(44100)
                                                  .setFakeStereo()
                                                  .setNormalize()
                                                  .setOutputFilename( "TestWaveFile-output.wav" )
                                                  .setSpan( 0.0, 6.0 )
                                                  .setInput( allwaves );
    exporter.write();

    exporter.close();

    return 0;
}

