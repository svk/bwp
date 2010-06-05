#include "Wave.h"

#include <cmath>
#include <cstdlib>

SineEnvelope::SineEnvelope( double frequency_,
                            double baseline_,
                            double amplitude_,
                            double phase_) :
    frequency ( frequency_ ),
    baseline ( baseline_ ),
    amplitude ( amplitude_ ),
    phase ( phase_ )
{
}

SineEnvelope::~SineEnvelope() {
}

double SineEnvelope::sample(double t) const {
    const double x = sin( 2 * M_PI * (frequency * t + phase) );
    return x * amplitude + baseline;
}

SineWave::SineWave( double frequency_,
                    double phase_ ) :
    frequency ( frequency_ ),
    phase ( phase_ )
{
}

SineWave::~SineWave() {
}

double SineWave::sample(double t) const {
    return sin( 2 * M_PI * (frequency * t + phase) );
}

SoundWave::SoundWave( const AudioFunction& amp_,
                      const AudioFunction& sig_) :
    amplitude ( amp_ ),
    wave ( sig_ )
{
}

SoundWave::~SoundWave() {
}

double SoundWave::sample(double t) const {
    double a = amplitude.sample( t );
    if( a != 0.0 ) {
        return a * wave.sample( t );
    }
}

PiecewiseLinearClosedSupport::~PiecewiseLinearClosedSupport() {
}

PiecewiseLinearClosedSupport::PiecewiseLinearClosedSupport( double t0_ ) :
    t0 ( t0_ ),
    t1 ( t0_ ),
    points ()
{
}

PiecewiseLinearClosedSupport& PiecewiseLinearClosedSupport::add( double duration, double value ) {
    t1 += duration;
    points.push_back( std::pair<double,double>( t1, value ) );
    return *this;
}

double PiecewiseLinearClosedSupport::sample(double t) const {
    if( t < t0 || t >  t1 ) return 0.0;
    double a = t0;
    double av = 0.0;
    for(std::vector<std::pair<double,double> >::const_iterator i = points.begin(); i != points.end(); i++) {
        double b = i->first;
        double bv = i->second;
        if( t < b ) {
            return (bv-av)/(b-a)*(t-a)+ av;
        }
        av = bv;
        a = b;
    }
    return 0.0;
}

TranslatedWave::TranslatedWave( const AudioFunction& wave_, double dt_ ) :
    wave ( wave_ ),
    dt ( dt_ )
{
}

double TranslatedWave::sample(double t) const {
    return wave.sample(t - dt);
}

SumWave::SumWave() :
    waves ()
{
}

SumWave& SumWave::add(const AudioFunction& wave) {
    waves.push_back( &wave );
    return *this;
}

double SumWave::sample( double t ) const {
    double s = 0.0;
    for(std::vector<AudioFunction const*>::const_iterator i = waves.begin(); i != waves.end(); i++) {
        s += (*i)->sample( t );
    }
    return s;
}

ProductWave::ProductWave() :
    waves ()
{
}

ProductWave& ProductWave::add(const AudioFunction& wave) {
    waves.push_back( &wave );
    return *this;
}

double ProductWave::sample( double t ) const {
    double s = 1.0;
    for(std::vector<AudioFunction const*>::const_iterator i = waves.begin(); i != waves.end(); i++) {
        double x = (*i)->sample( t );
        if( x == 0.0 ) return 0.0;
        s *= x;
    }
    return s;
}
