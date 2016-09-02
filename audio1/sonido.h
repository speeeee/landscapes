#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

#include <sndfile.h>

#ifndef	M_PI
#define	M_PI 3.14159265358979323846264338
#endif

#define	SAMPLE_RATE 44100

typedef struct { const char *name; sf_count_t len; } Sound;
typedef struct { int *dat; sf_count_t l; } Samp;
typedef struct { Samp *d; sf_count_t len; } SampArr;
typedef Samp (*wsnd)(Samp, va_list);

Samp concat(SampArr); Samp concat_pad(SampArr, int);

Sound snd(const char *name, sf_count_t len);

Samp pitch(Samp buf, double ratio);
Samp pitchn(Samp buf, va_list vl);

SampArr sc_edo(Samp key, double ratio, int notes);
Samp sc_edo_c(Samp key, va_list vl);

Samp para(Samp a, Samp b, int pos);
Samp para_(Samp a, Samp b, int pos);
Samp paran(Samp buf, va_list vl);

Samp sample(Samp s, int a, int b);
Samp samplen(Samp buf, va_list vl);

Samp loop(Samp a, int amt);
Samp loopn(Samp buf, va_list vl);

/* == Functions for transforming SampArr ========== */

Samp concat(SampArr a);
Samp concat_pad(SampArr a, int pad);

/* == Main functions and IO - Base ==== */

// fix SAMPLE_COUNT
// purposefully specific to "key.wav".
/*int *init_key(sf_count_t sc);*/

void play(SNDFILE *out, sf_count_t c, Samp s);

// stat = SFM_READ or SFM_WRITE
SNDFILE *sf_init_1(const char *name, sf_count_t len, int stat);

//void with_sound(const char *dest, Sound src, int chan, wsnd f, ...);
