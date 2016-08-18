#include	<stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sndfile.h>

#ifndef	M_PI
#define	M_PI 3.14159265358979323846264338
#endif

#define	SAMPLE_RATE 44100
#define	SAMPLE_COUNT (SAMPLE_RATE)

typedef struct { sf_count_t l; int *s; } Samp;
typedef struct { int n; Samp *dat; } SampArr;
typedef struct Meas { Samp sa; struct Meas *next; } Meas;
typedef struct { Meas *s; int fpm; } SampLst;

typedef struct Stk { void *pt; int *rc; struct Stk *prev; } Stk;

void push(void *y,int *rc,Stk *x) { Stk *e = malloc(sizeof(Stk));
  e->pt = y; e->rc = malloc(sizeof(int)); e->rc = rc; 
  if(x) { e->prev = x; } x = e; }
void pushr(void *y,Stk *x) { int *rc = malloc(sizeof(int)); push(y,rc,x); }
void pop(Stk *x) { Stk *q = x->prev;
  if(--(*(x->rc))<1) { free(x->pt); free(x->rc); } x = q; }
//void dup(Stk *x) { push(x->y,x->rc,x); }
void sget(int q,Stk *x) { Stk *e = x; for(int i=0;i<q;i++) { e = e->prev; }
  push(e->pt,e->rc,x); }

void pushs(Samp e,Stk *x) { Samp *f = malloc(sizeof(Samp));
  *f = e; pushr((void *)f,x); }
void pushsa(SampArr e,Stk *x) { SampArr *f = malloc(sizeof(SampArr));
  *f = e; pushr((void *)f,x); }
void pushsl(SampLst e,Stk *x) { SampLst *f = malloc(sizeof(SampLst));
  *f = e; pushr((void *)f,x); }

void ss_free(SampArr s) { for(int i=0;i<s.n;i++) { free(s.dat[i].s); } free(s.dat); }
void s_free(Samp s) { free(s.s); }

// sqrt(octave_interval,tones/octave) => sqrt(2,12) = 1 semitone interval
// this function is fairly naive.
Samp pitch(Samp buf, float ratio) { Samp buf2; buf2.l = ceil((float)buf.l/ratio);
  buf2.s = malloc(buf2.l*sizeof(int)); float e = 0;
  for(int i=0;i<buf2.l;i++) { buf2.s[i] = buf.s[(int)e]; e+=ratio; } return buf2; }

SampArr sc_edo(Samp key, float ratio, int notes) {
  SampArr s; s.dat = malloc(notes*sizeof(Samp)); s.n = notes;
  for(int i=0;i<notes;i++) { s.dat[i] = pitch(key,pow(pow(ratio,1.0/notes),i)); }
  return s; }

Samp pad(Samp a,sf_count_t nl) { Samp b; b.l = nl;
  b.s = malloc(nl*sizeof(int));
  for(int i=0;i<nl;i++) { if(i<b.l) { b.s[i] = a.s[i]; } else { b.s[i] = 0; } } return b; }

SampArr pad_s(SampArr a, sf_count_t nl) { SampArr an; an.n = a.n;
  an.dat = malloc(a.n*sizeof(Samp));
  for(int i=0;i<a.n;i++) { pad(a.dat[i],nl); } return an; }

Samp reverse(Samp a) { Samp b; b.l = a.l; b.s = malloc(b.l*sizeof(int));
  for(int i=0;i<b.l;i++) { b.s[i] = a.s[a.l-i-1]; } return b; }

// Sample b MUST not be larger than Sample a.
Samp para(Samp a, Samp b, int pos) { Samp c; c.s = malloc(a.l*sizeof(int));
  memcpy(c.s,a.s,a.l*sizeof(int)); c.l = a.l; 
  for(int i=pos;i<b.l+pos;i++) { c.s[i] = c.s[i]+b.s[i-pos]; }
  return c; }
Samp para_(Samp a, Samp b, int pos) { Samp c = para(a,b,pos);
  s_free(a); s_free(b); return c; }

// Sample b MUST not be larger than Sample a.
Samp beat(Samp a, Samp b, sf_count_t dur) { Samp e = a;
  for(int i=0;i<a.l;i+=dur) { e = para(e,b,i); } return e; }
  
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

int main(int argc, char **argv) { SNDFILE *out;
  SF_INFO sfout; int k;
  SF_INFO sfin; SNDFILE *in;

  //if (!(buffer = malloc (2 * SAMPLE_COUNT * sizeof (int)))) {
  //  printf ("Malloc failed.\n"); exit (0); }

  memset(&sfout, 0, sizeof(sfout));

  sfout.samplerate = SAMPLE_RATE;
  sfout.frames = SAMPLE_COUNT*4;
  sfout.channels = 1;
  sfout.format = (SF_FORMAT_WAV | SF_FORMAT_PCM_16);

  int *buffer = init_key(SAMPLE_COUNT/2); Samp s = { SAMPLE_COUNT/2, buffer };

  // sequences of 12-EDO, 24-EDO, and 31-EDO.
  // tempo rises as pitch rises since the notes are not padded.
  SampArr e = sc_edo(s,2,12);
  SampArr f = sc_edo(s,2,24);
  SampArr d = sc_edo(s,2,31);
  //SampArr e = pad_s(de,44100);
  //Samp q = para(e.dat[0],e.dat[11],0);
  //Samp p = para(q,e.dat[7],0);
  Samp q = reverse(s);
  Samp p = pitch(q,0.125);
  Samp c = beat(p,s,44100);

  if (!(out = sf_open ("sine.wav", SFM_WRITE, &sfout))) {
    printf ("Error : Not able to open output file.\n");
    free(buffer); return 1; }

  play(out,sfout.channels,c); free(c.s);

  //play_s(out,sfout.channels,e); play_s(out,sfout.channels,f);
  //play_s(out,sfout.channels,d);

  //if (sfinfo.channels == 1) {
  //  for (k = 0 ; k < SAMPLE_COUNT ; k++) {
  //    buffer [k] = AMPLITUDE * sin (FREQ * 2 * k * M_PI); } }

  sf_close(out);
  free(buffer); free(q.s); free(p.s); //ss_free(e); ss_free(f); ss_free(d);
  return 0; }
