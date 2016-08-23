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

// mechanical
// slight vibrato

typedef void (*FTy)(void *);

void s_freev(void *); void ss_freev(void *);
void none(void *a) { return; }

int sign(int a) { return (a>0) - (a<0); }

FTy typs[4] = { none, s_freev, ss_freev, free /* to be changed: SampLst */ };

typedef struct { sf_count_t l; int *s; } Samp;
typedef struct { int n; Samp *dat; } SampArr;
typedef struct Meas { Samp sa; struct Meas *next; } Meas;
typedef struct { Meas *s; int fpm; } SampLst;

typedef struct Stk { void *pt; FTy typ; int *rc; struct Stk *prev; } Stk;

Stk *push(void *y,int *rc,FTy typ,Stk *x) { Stk *e = malloc(sizeof(Stk));
  e->pt = y; e->rc = rc; e->prev = NULL; e->typ = typ;
  if(x) { e->prev = x; } x = e; return x; }
Stk *pushr(void *y,FTy typ,Stk *x) { int *rc = malloc(sizeof(int)); *rc = 1;
  return push(y,rc,typ,x); }
Stk *pop(Stk *x) { Stk *q = x->prev;
  if(--(*(x->rc))<1) { (x->typ)(x->pt); free(x->pt); free(x->rc); } x = q; return x; }
//void dup(Stk *x) { push(x->y,x->rc,x); }
Stk *sget(int q,Stk *x) { Stk *e = x; for(int i=0;i<q;i++) { e = e->prev; }
  *e->rc = *e->rc+1; return push(e->pt,e->rc,e->typ,x); }
Stk *sgets(Stk *x) { return sget(*(int *)x->pt,pop(x)); }

Stk *lift(Stk *q, Stk *x) { *q->rc = *q->rc+1; return push(q->pt,q->rc,q->typ,x); }

Stk *pushs(Samp e,Stk *x) { Samp *f = malloc(sizeof(Samp));
  *f = e; return pushr((void *)f,typs[1],x); }
Stk *pushsa(SampArr e,Stk *x) { SampArr *f = malloc(sizeof(SampArr));
  *f = e; return pushr((void *)f,typs[2],x); }
Stk *pushsl(SampLst e,Stk *x) { SampLst *f = malloc(sizeof(SampLst));
  *f = e; return pushr((void *)f,typs[3],x); }
Stk *pushi(int e,Stk *x) { int *f = malloc(sizeof(int));
  *f = e; return pushr((void *)f,typs[0],x); }
Stk *pushf(float e,Stk *x) { float *f = malloc(sizeof(float));
  *f = e; return pushr((void *)f,typs[0],x); }

void ss_free(SampArr s) { for(int i=0;i<s.n;i++) { free(s.dat[i].s); } free(s.dat); }
void s_free(Samp s) { free(s.s); }

void ss_freev(void *s) { ss_free(*(SampArr *)s); }
void s_freev(void *s) { s_free(*(Samp *)s); }

/* == Sample manipulation - Base ===== */

// sqrt(octave_interval,tones/octave) => sqrt(2,12) = 1 semitone interval
// this function is fairly naive.
Samp pitch(Samp buf, float ratio) { Samp buf2; buf2.l = ceil((float)buf.l/ratio);
  buf2.s = malloc(buf2.l*sizeof(int)); float e = 0;
  for(int i=0;i<buf2.l;i++) { buf2.s[i] = buf.s[(int)e]; e+=ratio; } return buf2; }
Stk *pitchs(Stk *x) { Samp s = pitch(*(Samp *)x->prev->pt,*(float *)x->pt);
  return pushs(s,pop(pop(x))); } // reversed

SampArr sc_edo(Samp key, float ratio, int notes) {
  SampArr s; s.dat = malloc(notes*sizeof(Samp)); s.n = notes;
  for(int i=0;i<notes;i++) { s.dat[i] = pitch(key,pow(pow(ratio,1.0/notes),i)); }
  return s; }
Stk *sc_edos(Stk *x) { SampArr s = sc_edo(*(Samp *)x->prev->prev->pt
                                         ,*(float *)x->prev->pt,*(int *)x->pt);
  return pushsa(s,pop(pop(pop(x)))); } // reversed

Samp pad(Samp a,sf_count_t nl) { Samp b; b.l = nl;
  b.s = malloc(nl*sizeof(int));
  for(int i=0;i<nl;i++) { if(i<b.l) { b.s[i] = a.s[i]; } else { b.s[i] = 0; } } return b; }

SampArr pad_s(SampArr a, sf_count_t nl) { SampArr an; an.n = a.n;
  an.dat = malloc(a.n*sizeof(Samp));
  for(int i=0;i<a.n;i++) { pad(a.dat[i],nl); } return an; }

Samp reverse(Samp a) { Samp b; b.l = a.l; b.s = malloc(b.l*sizeof(int));
  for(int i=0;i<b.l;i++) { b.s[i] = a.s[a.l-i-1]; } return b; }
Stk *reverses(Stk *x) { Samp s = reverse(*(Samp *)x->pt);
  return pushs(s,pop(x)); } // reversed

  //if (sfinfo.channels == 1) {
  //  for (k = 0 ; k < SAMPLE_COUNT ; k++) {
  //    buffer [k] = AMPLITUDE * sin (FREQ * 2 * k * M_PI); } }
Samp amp(Samp a, float perc) { Samp b; b.l = a.l; b.s = malloc(b.l*sizeof(int));
  memcpy(b.s,a.s,b.l*sizeof(int));
  for(int i=0;i<a.l;i++) { b.s[i] -= (int)b.s[i]*perc; } return b; }
Stk *amps(Stk *x) { Samp b = amp(*(Samp *)x->prev->pt,*(float *)x->pt);
  return pushs(b,pop(pop(x))); } // reversed

Samp sample(Samp s, sf_count_t a, sf_count_t b) { Samp c; c.l = b-a;
  c.s = malloc(c.l*sizeof(int)); for(int i=a;i<b;i++) { c.s[i] = s.s[i]; }
  return c; }
Stk *samples(Stk *x) { Samp b = sample(*(Samp *)x->prev->prev->pt
                                      ,*(int *)x->prev->pt
                                      ,*(int *)x->pt);
  return pushs(b,pop(pop(pop(x)))); } // reversed

Samp loop(Samp a, sf_count_t amt) { Samp c; c.l = a.l*amt;
  c.s = malloc(c.l*sizeof(int)); for(int i=0;i<c.l;i++) { c.s[i] = a.s[i%a.l]; }
  return c; }
Stk *loops(Stk *x) { Samp b = loop(*(Samp *)x->prev->pt,*(int *)x->pt);
  return pushs(b,pop(pop(x))); } // reversed

/* == Manipulation of individiual frequencies == */

//filteredValue = oldValue + (newValue - oldValue) / (smoothing / timeSinceLastUpdate)
// test of low-pass filter
Samp lpf(Samp a, float s) { Samp b; b.l = a.l; b.s = malloc(b.l*sizeof(int));
  b.s[0] = a.s[0]; for(int i=1;i<b.l;i++) { 
    b.s[i] = b.s[i-1] + (int)((a.s[i]-b.s[i-1])/s);
             /*a.s[i];*/ } return b; }
Stk *lpfs(Stk *x) { Samp b = lpf(*(Samp *)x->prev->pt,*(float *)x->pt);
  return pushs(b,pop(pop(x))); }

/* == Rhythm & Compisition - Base ==== */

// Sample b MUST not be larger than Sample a.
Samp para(Samp a, Samp b, int pos) { Samp c; c.s = malloc(a.l*sizeof(int));
  memcpy(c.s,a.s,a.l*sizeof(int)); c.l = a.l; 
  for(int i=pos;i<b.l+pos&&i<c.l;i++) { c.s[i] = c.s[i]+b.s[i-pos]; }
  return c; }
Stk *paras(Stk *x) { Samp s = para(*(Samp *)x->prev->prev->pt,*(Samp *)x->prev->pt
                                  ,*(int *)x->pt);
  return pushs(s,pop(pop(pop(x)))); } // reversed
//Samp para_(Samp a, Samp b, int pos) { Samp c = para(a,b,pos);
//  s_free(a); s_free(b); return c; }

// Sample b MUST not be larger than Sample a.
Samp beat(Samp a, Samp b, sf_count_t dur, sf_count_t disp) { Samp e = a;
  for(int i=disp;i<a.l;i+=dur) { e = para(e,b,i); } return e; }
Stk *beats(Stk *x) { Samp s = beat(*(Samp *)x->prev->prev->prev->pt
                                  ,*(Samp *)x->prev->prev->pt
                                  ,*(int *)x->prev->pt
                                  ,*(int *)x->pt);
  return pushs(s,pop(pop(pop(pop(x))))); }

/* == Main functions and IO - Base ==== */

// fix SAMPLE_COUNT
// purposefully specific to "key.wav".
int *init_key(sf_count_t sc) { int *buffer = malloc(SAMPLE_COUNT*sizeof(int));
  SF_INFO sfin; memset(&sfin, 0, sizeof(sfin)); SNDFILE *in;
  sfin.samplerate = SAMPLE_RATE;
  sfin.frames	= sc;
  sfin.channels = 1;
  sfin.format	= (SF_FORMAT_WAV | SF_FORMAT_PCM_16);
  if(!(in = sf_open("key3.wav", SFM_READ, &sfin))) { printf("no"); free(buffer); return 1; }
  sf_read_int(in,buffer,sc); sf_close(in); return buffer; }

void play(SNDFILE *out, sf_count_t c, Samp s) {
  if(sf_write_int(out,s.s,c*s.l) != c*s.l) { puts(sf_strerror(out)); } }
void play_s(SNDFILE *out, sf_count_t c, SampArr s) {
  for(int i=0;i<s.n;i++) { play(out,c,s.dat[i]); } }

void st_free(Stk *x) { if(x) { st_free(x->prev);
  if(x->pt) { free(x->pt); } if(x->rc) { free(x->rc); } free(x); } }

// from here on, the following functions mostly build on the previous material.
/* == Sample manipulation ======== */

// not fully functional.
Samp para_(Samp a, Samp b, int pos) { Samp c; c.l = b.l+pos>a.l?b.l+pos:a.l;
  c.s = malloc(c.l*sizeof(int)); memcpy(c.s,a.s,a.l*sizeof(int));
  for(int i=pos;i<b.l+pos;i++) { c.s[i] = c.s[i]+b.s[i-pos]; } return c; }

/*int main(int argc, char **argv) { SNDFILE *out;
  SF_INFO sfout; int k;
  SF_INFO sfin; SNDFILE *in;

  Stk *stk = NULL;

  //if (!(buffer = malloc (2 * SAMPLE_COUNT * sizeof (int)))) {
  //  printf ("Malloc failed.\n"); exit (0); }

  memset(&sfout, 0, sizeof(sfout));

  sfout.samplerate = SAMPLE_RATE;
  sfout.frames = SAMPLE_COUNT*12;
  sfout.channels = 1;
  sfout.format = (SF_FORMAT_WAV | SF_FORMAT_PCM_16);

  int *buffer = init_key(SAMPLE_COUNT/2); Samp s = { SAMPLE_COUNT/2, buffer };

  int *buf2 = calloc(12*SAMPLE_COUNT,sizeof(int));
  Samp mus = (Samp) { SAMPLE_COUNT*12, buf2 };

  // sequences of 12-EDO, 24-EDO, and 31-EDO.
  // tempo rises as pitch rises since the notes are not padded.
  SampArr a = sc_edo(s,2,12);
  //SampArr f = sc_edo(s,2,24);
  //SampArr d = sc_edo(s,2,31);

  Samp d = loop(sample(a.dat[5],0,500),88);
  Samp e = loop(sample(pitch(a.dat[5],0.5),0,1000),44);

  Samp b = beat(mus,d,176400,0);
  Samp c = beat(b,e,176400,77175);

  //stk = pitchs(reverses(sget(1,pushf(0.125,pushs(s,pushi(44100,stk))))));
  //stk = beats(sget(2,pushs(s,pushi(44100,stk))));

  //stk = pitchs(reverses(pushs(s,pushf(0.125,stk))));
  //stk = amps(sget(1,pushf(0.5,pushs(s,stk))));

  if (!(out = sf_open ("sine.wav", SFM_WRITE, &sfout))) {
    printf ("Error : Not able to open output file.\n");
    free(buffer); return 1; }

  play(out,sfout.channels,c); 
  //play(out,sfout.channels,pitch(e.dat[5],0.5));

  //play_s(out,sfout.channels,e); play_s(out,sfout.channels,f);
  //play_s(out,sfout.channels,d);

  sf_close(out); st_free(stk);;
  return 0; }*/
