#include "Wave.h"
#include "WaveFileExporter.h"

#include <cstdio>

int main(int argc, char *argv[]) {
    ProductWave* tone440 = new ProductWave();
    (*tone440).add( new CutoffEnvelope( 0.0, 3.0 ) )
              .add( new ExponentialFadeoutEnvelope( 2.0 ) )
              .add( new SineEnvelope( 10.0, 0.8, 0.2 ) )
              .add( new SquareWave( 440.0 ) );


    WaveFileExporter exporter = WaveFileExporter().setRate(44100)
                                                  .setFakeStereo()
                                                  .setNormalize()
                                                  .setOutputFilename( "TestWaveFile-output.wav" )
                                                  .setSpan( 0.0, 6.0 )
                                                  .setInput( tone440 );
    exporter.write();
    exporter.close();

    delete tone440;

    return 0;
}

