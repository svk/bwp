#include "Wave.h"
#include "WaveFileExporter.h"

#include <cstdio>

int main(int argc, char *argv[]) {
    ProductWave* tone440 = new ProductWave();
    (*tone440).add( new CutoffEnvelope( 0.0, 3.0 ) )
              .add( new ExponentialFadeoutEnvelope( 2.0 ) )
              .add( new DynamicSineWave( new SineEnvelope( 10, 440.0, 10.0 ) ) );


    WaveFileExporter exporter = WaveFileExporter().setRate(44100)
                                                  .setFakeStereo()
                                                  .setNormalize()
                                                  .setOutputFilename( "TestWaveFile-output.wav" )
                                                  .setSpanUntilNil( 0.0 )
                                                  .setInput( tone440 );
    exporter.write();
    exporter.close();

    delete tone440;

    return 0;
}

