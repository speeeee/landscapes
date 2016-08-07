#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<math.h>

#include	<sndfile.h>

#ifndef		M_PI
#define		M_PI		3.14159265358979323846264338
#endif

#define		SAMPLE_RATE			44100
#define		SAMPLE_COUNT		(SAMPLE_RATE * 4)	/* 4 seconds */
#define		AMPLITUDE			(1.0 * 0x7F000000)
#define		FREQ			(440.0 / SAMPLE_RATE)

#define OUT  0
#define END  1
#define DEF  2
#define DATA 3
#define CALL 4
#define PACK 5

#define FUN 0
#define DAT 1

// data won't actively be loaded, but instead put on the table.

// 'f' is either data or function pointer.  Can deduce from table.
typedef struct Stk { int8_t op; int32_t f; int32_t *p; int asz; struct *Stk p; } Stk;
typedef struct { Stk *s; int sz; } SInf;
typedef struct { uint64_t loc; uint32_t sz; } Data;

typedef struct { SInf f; Data d; uintint8_t type; } Fun;

SInf stk;

void push(int8_t op, int32_t f) { Stk *x = malloc(sizeof(Stk));
  x->op = op; x->f = f; x->p = NULL; x->asz = 0; x->p = NULL;
  if(stk.s) { x->p = stk.s; } stk.s = x; }
void compose(int8_t op, int32_t f) { push(op,f); }

void read_f(SNDFILE *f, FILE *in, int8_t fun) { int c;
  while((c = fgetc(c,in))!=END) {
    if(c==DEF) { SInf x = (SInf) { SInf.s, SInf.sz }; new_f(x); }
    else if(c==DATA) { uint64_t l; uint32_t sz; fread(&l,sizeof(uint64_t),1,in);
      fread(&sz,sizeof(uint32_t),1,in); new_d(l,sz); }
    else { compose(c,-1); } } }

int main (int argc, char **argv) { SNDFILE *file;
  SF_INFO sfinfo; int k; int *buffer;

  if (!(buffer = malloc (2 * SAMPLE_COUNT * sizeof (int)))) {
    printf ("Malloc failed.\n"); exit (0); }

  memset (&sfinfo, 0, sizeof (sfinfo)) ;

  sfinfo.samplerate = SAMPLE_RATE;
  sfinfo.frames	= SAMPLE_COUNT;
  sfinfo.channels = 1;
  sfinfo.format	= (SF_FORMAT_WAV | SF_FORMAT_PCM_24);

  if (!(file = sf_open ("sine.wav", SFM_WRITE, &sfinfo))) {
    printf ("Error : Not able to open output file.\n");
    free (buffer); return 1; }

  /*if (sfinfo.channels == 1) {
    for (k = 0 ; k < SAMPLE_COUNT ; k++) {
      buffer [k] = AMPLITUDE * sin (FREQ * 2 * k * M_PI); } }

  if (sf_write_int (file, buffer, sfinfo.channels * SAMPLE_COUNT) != 
      sfinfo.channels * SAMPLE_COUNT) { puts (sf_strerror (file)); }*/

  FILE *f = fopen(argv[1],"rb"); read_f(file,argv[1],-1); fclose(f);

  sf_close (file);
  free (buffer);
  return 0; }
