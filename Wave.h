#ifndef H_WAVE
#define H_WAVE

#include <vector>

class AudioFunction {
    /* A frequency-defining wave is generally a function R -> [-1,1].
     * An envelope is a function R -> [0,1].
     * These can be multiplied together to get a sound with a controlled volume.
     * Waves can also be used in more creative ways, e.g. a wave that controls
     *   the "immediate frequency" of another (pitch bending).
     */
    public:
        virtual ~AudioFunction() {}

        virtual double sample(double) const = 0;
};

class SineEnvelope : public AudioFunction {
    public:
        SineEnvelope( double frequency,
                      double baseline,
                      double amplitude,
                      double phase = 0 );
        ~SineEnvelope();

        double sample(double) const;

    private:
        double frequency, baseline, amplitude, phase;
};

class SineWave : public AudioFunction {
    public:
        SineWave( double frequency,
                  double phase = 0 ); // phase is [0,1], 0.5 gets you a cosine wave
        ~SineWave();
        double sample(double) const;

    private:
        double frequency, phase;
        /* sin(ax) has wavelength 2pi / a, freq a / 2pi */
};

class PiecewiseLinearClosedSupport : public AudioFunction {
    public:
        PiecewiseLinearClosedSupport( double = 0 );

        PiecewiseLinearClosedSupport& add( double, double );
        ~PiecewiseLinearClosedSupport();

        double sample(double) const;

    private:
        double t0, t1;
        std::vector< std::pair<double,double> > points;
};

class SoundWave : public AudioFunction {
    public:
        SoundWave( const AudioFunction&, const AudioFunction& );
        ~SoundWave();

        double sample(double t) const;
    private:
        const AudioFunction& amplitude;
        const AudioFunction& wave;
};

class TranslatedWave : public AudioFunction {
    public:
        TranslatedWave( const AudioFunction&, double );
        double sample(double t) const;
    private:
        const AudioFunction& wave;
        double dt;
};

class SumWave : public AudioFunction {
    public:
        SumWave();
        SumWave& add(const AudioFunction&);
    
        double sample(double t) const;
    private:
        std::vector<const AudioFunction *> waves;
};

class ProductWave : public AudioFunction {
    public:
        ProductWave();
        ProductWave& add(const AudioFunction&);
    
        double sample(double t) const;
    private:
        std::vector<const AudioFunction *> waves;
};

class CutoffEnvelope : public AudioFunction {
    public:
        CutoffEnvelope(double, double);

        double sample(double t) const;

    private:
        double t0, t1;
};


class ExponentialFadeoutEnvelope : public AudioFunction {
    public:
        ExponentialFadeoutEnvelope(double);

        double sample(double t) const;

    private:
        double alpha;
};

#endif
