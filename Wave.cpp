#include "Wave.h"

#include <cmath>
#include <cstdlib>
#include <cassert>

SineEnvelope::SineEnvelope( double frequency_,
                            double baseline_,
                            double amplitude_,
                            double phase_) :
    frequency ( frequency_ ),
    baseline ( baseline_ ),
    amplitude ( amplitude_ ),
    phase ( phase_ ),
    t ( 0 )
{
}

SineEnvelope::~SineEnvelope() {
}

double SineEnvelope::advance(double dt) {
    t += dt;

    const double x = sin( 2 * M_PI * (frequency * t + phase) );
    return x * amplitude + baseline;
}

SineWave::SineWave( double frequency_,
                    double phase_ ) :
    frequency ( frequency_ ),
    phase ( phase_ ),
    t ( 0 )
{
}

SineWave::~SineWave() {
}

double SineWave::advance(double dt) {
    t += dt;
    return sin( 2 * M_PI * (frequency * t + phase) );
}

PiecewiseLinearClosedSupport::~PiecewiseLinearClosedSupport() {
}

PiecewiseLinearClosedSupport::PiecewiseLinearClosedSupport( double t0_ ) :
    t0 ( t0_ ),
    t1 ( t0_ ),
    t ( 0 ),
    points ()
{
}

PiecewiseLinearClosedSupport& PiecewiseLinearClosedSupport::add( double duration, double value ) {
    t1 += duration;
    points.push_back( std::pair<double,double>( t1, value ) );
    return *this;
}

double PiecewiseLinearClosedSupport::advance(double dt) {
    t += dt;

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

ExponentialFadeoutEnvelope::ExponentialFadeoutEnvelope(double alpha_) :
    alpha ( alpha_ ),
    t ( 0 )
{
}

double ExponentialFadeoutEnvelope::advance(double dt) {
    t += dt;

    if( t < 0.0 ) {
        return 1.0;
    }
    return exp( - alpha * t );
}

CutoffEnvelope::CutoffEnvelope( double t0_, double t1_ ) :
    t0 ( t0_ ),
    t1 ( t1_ ),
    t ( 0 )
{
}

double CutoffEnvelope::advance(double dt) {
    t += dt;

    if( t < t0 || t > t1 ) return 0.0;
    return 1.0;
}

DelayedWave::DelayedWave( WaveStream* wave_, double delay_ ) :
    wave ( wave_ ),
    delay ( delay_ )
{
}

DelayedWave::~DelayedWave() {
    delete wave;
}

double DelayedWave::advance(double dt) {
    if( delay > dt ) {
        delay -= dt;
        return 0.0;
    }
    dt -= delay;
    delay = 0.0;
    return wave->advance( dt );
}

SumWave::SumWave() {
}

SumWave::~SumWave() {
    for(std::vector<WaveStream*>::iterator i = waves.begin(); i != waves.end(); i++) {
        delete *i;
    }
}

SumWave& SumWave::add(WaveStream *wave) {
    waves.push_back( wave );
    return *this;
}

double SumWave::advance(double dt) {
    double rv = 0.0;
    for(std::vector<WaveStream*>::iterator i = waves.begin(); i != waves.end(); i++) {
        rv += (*i)->advance( dt );
    }
    return rv;
}

ProductWave::ProductWave() {
}

ProductWave::~ProductWave() {
    for(std::vector<WaveStream*>::iterator i = waves.begin(); i != waves.end(); i++) {
        delete *i;
    }
}

ProductWave& ProductWave::add(WaveStream *wave) {
    waves.push_back( wave );
    return *this;
}

double ProductWave::advance(double dt) {
    double rv = 1.0;
    for(std::vector<WaveStream*>::iterator i = waves.begin(); i != waves.end(); i++) {
        rv *= (*i)->advance( dt );
    }
    return rv;
}

SawtoothWave::SawtoothWave( double frequency_, double phase_ ) :
    wavelength ( 1.0 / frequency_ ),
    phase ( (phase_+0.25) * wavelength )
{
    advance(0);
}

double SawtoothWave::advance(double dt) {
    const double halfwl = wavelength / 2.0;
    phase += dt;
    while(phase > wavelength) {
        phase -= wavelength;
    }
    /* Descend, then ascend. (Note that we offset phase by .25
     * to resemble a sine wave.) */
    if( phase <= halfwl ) {
        /* Descending phase. */
        double t = phase / halfwl;
        return 2.0 * t - 1.0;
    } else {
        double t = (phase-halfwl) / halfwl;
        return (-2.0) * t + 1.0;
    }
}

SquareWave::SquareWave(double frequency_, double phase_) :
    wavelength( 1.0 / frequency_ ),
    phase( phase * wavelength )
{
}

double SquareWave::advance(double dt) {
    const double halfwl = wavelength / 2.0;
    phase += dt;
    while(phase > wavelength) {
        phase -= wavelength;
    }
    return (phase < halfwl) ? 1.0 : -1.0;
}

bool PiecewiseLinearClosedSupport::nil(void) const {
    return t > t1;
}

bool DelayedWave::nil(void) const {
    return wave->nil();
}

bool SumWave::nil(void) const {
    for(std::vector<WaveStream*>::const_iterator i = waves.begin(); i != waves.end(); i++) {
        if( !(*i)->nil() ) return false;
    }
    return true;
}

bool ProductWave::nil(void) const {
    for(std::vector<WaveStream*>::const_iterator i = waves.begin(); i != waves.end(); i++) {
        if( (*i)->nil() ) return true;
    }
    return false;
}

bool CutoffEnvelope::nil(void) const {
    return t > t1;
}




