#include "WaveFileExporter.h"

#include <stdexcept>
#include <limits>

#include <stdint.h>
#include <cstdio>
#include <cerrno>
#include <cmath>

WaveFileExporter::WaveFileExporter() :
    sampleRate( -1 ),
    fakeStereo( false ),
    filename ( "" ),
    file ( 0 ),
    adoptOutput ( false ),
    t0 ( 0 ),
    t1 ( 1 ),
    closed ( false ),
    input ( 0 )
{
}

void WaveFileExporter::close() {
    if( !closed ) {
        if( file && adoptOutput ) {
            fclose( file );
        }
        closed = true;
    }
}

WaveFileExporter::~WaveFileExporter() {
}

WaveFileExporter& WaveFileExporter::setRate(int rate_) {
    sampleRate = rate_;
    return *this;
}

WaveFileExporter& WaveFileExporter::setNormalize(bool state_) {
    normalize = state_;
    return *this;
}

WaveFileExporter& WaveFileExporter::setFakeStereo(bool state_) {
    fakeStereo = state_;
    return *this;
}

WaveFileExporter& WaveFileExporter::setOutput(FILE *file_) {
    file = file_;
    return *this;
}

WaveFileExporter& WaveFileExporter::setOutputFilename(const std::string& filename_) {
    filename = filename_;
    file = fopen( filename.c_str(), "wb" );
    if( !file ) {
        throw std::runtime_error( "unable to write to file: " + filename );
    }
    adoptOutput = true;
    return *this;
}

WaveFileExporter& WaveFileExporter::setAdoptOutput(bool state_ ) {
    adoptOutput = state_;
    return *this;
}

WaveFileExporter& WaveFileExporter::setSpan(double t0_, double t1_) {
    t0 = t0_;
    t1 = t1_;
    return *this;
}

WaveFileExporter& WaveFileExporter::setInput( WaveStream* wave_ ) {
    input = wave_;
    return *this;
}

void WaveFileExporter::write() {
    /* Not implemented yet! */
    const int fmtHeaderSize = 24;
    const int noSamples = (int)(0.5 + (t1-t0)*sampleRate);

    uint32_t data;

    uint16_t sampleWidth = 16;
    uint16_t blockSize = 2;
    uint16_t channels = fakeStereo ? 2 : 1;
    uint32_t chunkSize = 4 + 24 + 8 + noSamples * blockSize;

    writeString( "RIFF" );
    writeInt32( chunkSize );
    writeString( "WAVE" );

    writeString( "fmt " );
    writeInt32( 16 );
    writeInt16( 1 );
    writeInt16( channels );
    writeInt32( sampleRate );
    writeInt32( blockSize * sampleRate );
    writeInt16( blockSize * channels );
    writeInt16( sampleWidth );

    writeString( "data" );
    writeInt32( noSamples * blockSize * channels );

    const double uintAmp = (std::numeric_limits<uint16_t>::max() >> 1);
    const double dt = 1.0 / (double) sampleRate;
    double t = t0;
    int i = 0;
    double max_abs = 0.0;
    std::vector<double> samples;
    while( i < noSamples ) {
        double x = input->advance( dt );
        samples.push_back( x );
        if( fabs(x) > max_abs ) {
            max_abs = fabs(x);
        }
        i++;
    }
    for(std::vector<double>::iterator i = samples.begin(); i != samples.end(); i++) {
        double x = *i;
        if( normalize ) {
            x /= max_abs;
        } else {
            if( x > 1.0 ) x = 1.0;
            if( x < -1.0 ) x = -1.0;
        }
        int16_t sample = static_cast<uint16_t>( uintAmp * x + 0.5);
        for(int j=0;j<channels;j++) {
            writeInt16( sample );
        }
    }
}

void WaveFileExporter::writeString(const std::string& s) {
    int rv = fwrite( s.data(), s.length(), 1, file );
    if( rv != 1 ) {
        fprintf(stderr, "errno = %d", errno );
        throw std::runtime_error( "write error (writeString)" );
    }
}

void WaveFileExporter::writeInt32(uint32_t data) {
#ifdef SWAP_BYTES
    uint8_t *t = reinterpret_cast<uint8_t*>( &data );
    t[0] ^= t[3];
    t[3] ^= t[0];
    t[0] ^= t[3];
    t[1] ^= t[2];
    t[2] ^= t[1];
    t[1] ^= t[2];
#endif
    int rv = fwrite( &data, 4, 1, file );
    if( rv != 1 ) {
        throw std::runtime_error( "write error (writeInt32)" );
    }
}

void WaveFileExporter::writeInt16(uint16_t data) {
#ifdef SWAP_BYTES
    uint8_t *t = reinterpret_cast<uint8_t*>( &data );
    t[0] ^= t[1];
    t[1] ^= t[0];
    t[0] ^= t[1];
#endif
    int rv = fwrite( &data, 2, 1, file );
    if( rv != 1 ) {
        throw std::runtime_error( "write error (writeInt16)" );
    }


}



