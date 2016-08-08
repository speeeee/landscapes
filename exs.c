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
#define FUN  3
#define DATA 4
#define CALL 5
#define PACK 6
#define PUSH32 7
#define PUSH64 8
#define FEND 9

#define FUN 0
#define DAT 1
#define PRI 2

// data won't actively be loaded, but instead put on the table.

// 'f' is either data or function pointer.  Can deduce from table.
typedef struct Stk { Data d; int type; struct *Stk p; } Stk;
typedef struct { Stk *s; int sz; } SInf;
typedef struct { long loc; uint32_t sz; } Data;

typedef struct { SInf f; uint8_t type; } Fun;

SInf stk; Fun *funs; int fsz = 0;

void push(Data d, int t) { Stk *x = malloc(sizeof(Stk)); x->p = NULL;
  if(stk.s) { x->p = stk.s; } stk.s = x; }
void compose(int8_t op, int32_t f) { push(op,f); }

Data data(long l, uint32_t sz) { return (Data) { l, sz }; }

void fun_f(int32_t, uint32_t, FILE *);
void prim_f(int8_t,  uint32_t, FILE *);

eval(FILE *in, SNDFILE *f);

void call_f(int32_t f, int32_t d, FILE *in, SNDFILE *f) {
  // map (a . b . c) d
  Fun e = funs[f]; Data d = funs[d].d;
  int sto = ftell(in); fseek(d.loc,SEEK_CUR,in);
  fun_f(e,d.sz,in); fseek(sto,SEEK_CUR,in); }
void fun_f(Fun e, int32_t sz, FILE *in) { Stk *fe = e.f;
  for(int i=0;i<sz;i++) { eval(fe,sz,in); } }

void read_f(SNDFILE *f, FILE *in, int8_t fun) { int c;
  while((c = fgetc(c,in))!=END) { switch(c) {
    case DEF: new_f(); break; case PUSH32: push(data(ftell(in),4),DAT); break;
    case PUSH64: push(data(ftell(in),8),DAT); break;
    case DATA: { uint32_t s; fread(&s,sizeof(uint32_t),1,in);
                 push(data(ftell(in),s),DAT); }
    case CALL: eval(in,f); break;
    default: push(data(0,0),PRI); } }

int main(int argc, char **argv) { SNDFILE *file;
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
