#ifndef H_WAVE
#define H_WAVE

#include <vector>

class WaveStream {
    /* A wave stream replaces the concept of an audio function;
     *  crucially it is not an entire function. It cannot be
     *  evaluated at any point; it is only possible to advance
     *  the wave. The stream may or may not be deterministic!
     *  It is not possible to rewind.
     *
     * Note that deterministic streams are probably more
     * interesting for producing neat sounds.
     *
     * Any function can be expressed as a wave stream: simply
     * include an additional variable; state. It should be easier
     * to do some things (like change the frequency of a wave
     * dynamically).
     */
    public:
        virtual ~WaveStream() {}

        virtual double advance(double) = 0;
};

class SineEnvelope : public WaveStream {
    public:
        SineEnvelope( double frequency,
                      double baseline,
                      double amplitude,
                      double phase = 0 );
        ~SineEnvelope();

        double advance(double);

    private:
        const double frequency, baseline, amplitude, phase;
        double t;
};

class SineWave : public WaveStream {
    public:
        SineWave( double frequency,
                  double phase = 0 ); // phase is [0,1], 0.5 gets you a cosine wave
        ~SineWave();
        double advance(double);

    private:
        const double frequency, phase;
        double t;
};

class PiecewiseLinearClosedSupport : public WaveStream {
    public:
        PiecewiseLinearClosedSupport( double = 0 );

        PiecewiseLinearClosedSupport& add( double, double );
        ~PiecewiseLinearClosedSupport();

        double advance(double);

    private:
        double t0, t1;
        std::vector< std::pair<double,double> > points;

        double t;
};

class DelayedWave : public WaveStream {
    public:
        DelayedWave( WaveStream*, double );
        ~DelayedWave();

        double advance(double t);
    private:
        WaveStream *wave;
        double delay;
};

class SumWave : public WaveStream {
    public:
        SumWave();
        ~SumWave();

        SumWave& add(WaveStream*);
    
        double advance(double t);
    private:
        std::vector<WaveStream*> waves;
};

class ProductWave : public WaveStream {
    public:
        ProductWave();
        ~ProductWave();

        ProductWave& add(WaveStream*);
    
        double advance(double t);
    private:
        std::vector<WaveStream*> waves;
};

class CutoffEnvelope : public WaveStream {
    public:
        CutoffEnvelope(double, double);

        double advance(double);

    private:
        const double t0, t1;

        double t;
};


class ExponentialFadeoutEnvelope : public WaveStream {
    public:
        ExponentialFadeoutEnvelope(double);

        double advance(double);

    private:
        const double alpha;
        double t;
};

#endif
