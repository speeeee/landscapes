#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sndfile.h>

#ifndef	M_PI
#define	M_PI 3.14159265358979323846264338
#endif

#define	SAMPLE_RATE 44100
typedef Samp (*wsnd)(Samp, int, ...);
typedef struct { const char *name, sf_count_t len } Sound;
typedef struct { int *dat; sf_count_t l; } Samp;

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
  if(sf_write_int(out,s.dat,c*s.l) != c*s.l) { puts(sf_strerror(out)); } }
void play_s(SNDFILE *out, sf_count_t c, SampArr s) {
  for(int i=0;i<s.n;i++) { play(out,c,s.dat[i]); } }

// stat = SFM_READ or SFM_WRITE
SNDFILE *sf_init_1(const char *name, sf_count_t len, int stat) { SF_INFO res;
  memset(&res, 0, sizeof(res)); res.samplerate = SAMPLE_RATE;
  res.frames = len; res.channels = 1;
  res.format = (SF_FORMAT_WAV | SF_FORMAT_PCM_16);
  return sf_open(name, stat, &res); }

void with_sound(Sound dest, Sound src, wsnd f, int arglen, ...) {
  SNDFILE *d = sf_init_1(dest.name, dest.len, SFM_WRITE);
  SNDFILE *s = sf_init_1(src.name, src.len, SFM_READ);
  Samp sd; sd.dat = calloc(src.len,sizeof(int));
  sd = (Samp) { sf_read_int(src,sd.dat,src.len), src.len };
  va_list vl; va_start(vl,arglen);
  Samp dd = f(sd,arglen,vl); play(d,dd.l,dd.dat); }

int main(int argc, char **argv) { SNDFILE *out;
  SF_INFO sfout; int k;
  SF_INFO sfin; SNDFILE *in;

  //if (!(buffer = malloc (2 * SAMPLE_COUNT * sizeof (int)))) {
  //  printf ("Malloc failed.\n"); exit (0); }

  memset(&sfout, 0, sizeof(sfout));

  sfout.samplerate = SAMPLE_RATE;
  sfout.frames = SAMPLE_COUNT*12;
  sfout.channels = 1;
  sfout.format = (SF_FORMAT_WAV | SF_FORMAT_PCM_16);

  int *buffer = init_key(SAMPLE_COUNT/2); Samp s = { SAMPLE_COUNT/2, buffer };

  play(out,sfout.channels,c); 

  sf_close(out);
  return 0; }
