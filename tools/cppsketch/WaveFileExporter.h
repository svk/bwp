#ifndef H_WAVEFILEEXPORTER
#define H_WAVEFILEEXPORTER

#include <string>

#include "Wave.h"

class WaveFileExporter {
    public:
        WaveFileExporter();
        ~WaveFileExporter();

        WaveFileExporter& setRate(int);
        WaveFileExporter& setFakeStereo(bool = true);
        WaveFileExporter& setNormalize(bool = true);
        WaveFileExporter& setOutput(FILE*);
        WaveFileExporter& setOutputFilename(const std::string&);
        WaveFileExporter& setAdoptOutput(bool = true);
        WaveFileExporter& setSpan(double, double);
        WaveFileExporter& setSpanUntilNil(double);
        WaveFileExporter& setInput( WaveStream* );

        void write();
        void close();

    private:
        int sampleRate;
        bool fakeStereo;
        bool normalize;
        bool untilNil;
        std::string filename;
        FILE * file;
        bool adoptOutput;
        double t0, t1;

        bool closed;
        WaveStream *input;

        void writeString(const std::string&);
        void writeInt32(uint32_t);
        void writeInt16(uint16_t);
};


#endif
