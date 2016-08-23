#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sndfile.h>

#ifndef	M_PI
#define	M_PI 3.14159265358979323846264338
#endif

#define	SAMPLE_RATE 44100
#define	SAMPLE_COUNT (SAMPLE_RATE)

// low-pass filter, just block if above (N)Hz

typedef void (*FTy)(void *);

void s_freev(void *); void ss_freev(void *);
void none(void *a);

typedef struct { sf_count_t l; int *s; } Samp;
typedef struct { int n; Samp *dat; } SampArr;
typedef struct Meas { Samp sa; struct Meas *next; } Meas;
typedef struct { Meas *s; int fpm; } SampLst;

typedef struct Stk { void *pt; FTy typ; int *rc; struct Stk *prev; } Stk;

Stk *push(void *y,int *rc,FTy typ,Stk *x);
Stk *pushr(void *y,FTy typ,Stk *x);
Stk *pop(Stk *x);

//void dup(Stk *x)
Stk *sget(int q,Stk *x);
Stk *sgets(Stk *x);

Stk *lift(Stk *q,Stk *x);

Stk *pushs(Samp e,Stk *x);
Stk *pushsa(SampArr e,Stk *x);
Stk *pushsl(SampLst e,Stk *x);
Stk *pushi(int e,Stk *x);
Stk *pushf(float e,Stk *x);

void ss_free(SampArr s);

void ss_freev(void *s);
void s_freev(void *s);

/* == Sample manipulation ========== */

// sqrt(octave_interval,tones/octave) => sqrt(2,12) = 1 semitone interval
// this function is fairly naive.
Samp pitch(Samp buf, float ratio);
Stk *pitchs(Stk *x);

SampArr sc_edo(Samp key, float ratio, int notes);
Stk *sc_edos(Stk *x);

Samp pad(Samp a,sf_count_t nl);

SampArr pad_s(SampArr a, sf_count_t nl);

Samp reverse(Samp a);
Stk *reverses(Stk *x);

//`:> ([A-Za-z0-9_]+) ([A-Za-z0-9_]+)``$$0 *$$1 = *($$0 *)~#REF->pt;\n`DEF

  //if (sfinfo.channels == 1) {
  //  for (k = 0 ; k < SAMPLE_COUNT ; k++) {
  //    buffer [k] = AMPLITUDE * sin (FREQ * 2 * k * M_PI); } }
Samp amp(Samp a, float perc);
Stk *amps(Stk *x);

Samp sample(Samp s, sf_count_t a, sf_count_t b);
Stk *samples(Stk *x);

Samp loop(Samp a, sf_count_t amt);
Stk *loops(Stk *x);

/* == Granular ====================== */

Samp lpf(Samp b);
Stk *lpfs(Stk *x);

/* == Rhythm & Compisition ========== */

// Sample b MUST not be larger than Sample a.
Samp para(Samp a, Samp b, int pos);
Stk *paras(Stk *x);

//Samp para_(Samp a, Samp b, int pos);

// Sample b MUST not be larger than Sample a.
Samp beat(Samp a, Samp b, sf_count_t dur);
Stk *beats(Stk *x);

/* == Main functions and IO ========== */

// purposefully specific to "key.wav".
int *init_key(sf_count_t sc);

void play(SNDFILE *out, sf_count_t c, Samp s);

void st_free(Stk *x);
