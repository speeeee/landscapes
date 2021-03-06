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

Sound snd(const char *name, sf_count_t len) { return (Sound) { name, len }; }

Samp pitch(Samp buf, double ratio) { Samp buf2; buf2.l = ceil((double)buf.l/ratio);
  buf2.dat = malloc(buf2.l*sizeof(int)); double e = 0;
  for(int i=0;i<buf2.l;i++) { buf2.dat[i] = buf.dat[(int)e]; e+=ratio; } return buf2; }
Samp pitchn(Samp buf, va_list vl) { double ratio = va_arg(vl,double);
  return pitch(buf,ratio); }

SampArr sc_edo(Samp key, double ratio, int notes) {
  SampArr s; s.d = malloc(notes*sizeof(Samp)); s.len = notes;
  for(int i=0;i<notes;i++) { s.d[i] = pitch(key,pow(pow(ratio,1.0/notes),i)); }
  return s; }
Samp sc_edo_c(Samp key, va_list vl) {
  double r = va_arg(vl,double); int n = va_arg(vl,int);
  SampArr c = sc_edo(key,r,n); Samp e = concat(c);
  for(int i=0;i<c.len;i++) { free(c.d[i].dat); } free(c.d); free(key.dat); return e; }

Samp para(Samp a, Samp b, int pos) { Samp c; c.dat = malloc(a.l*sizeof(int));
  memcpy(c.dat,a.dat,a.l*sizeof(int)); c.l = a.l; 
  for(int i=pos;i<b.l+pos&&i<c.l;i++) { c.dat[i] = c.dat[i]+b.dat[i-pos]; }
  return c; }
Samp para_(Samp a, Samp b, int pos) { Samp c = para(a,b,pos);
  free(a.dat); return c; }
Samp paran(Samp buf, va_list vl) { Samp b = va_arg(vl,Samp); int pos = va_arg(vl,int);
  Samp c = para_(buf,b,pos); free(b.dat); return c; }

Samp sample(Samp s, int a, int b) { Samp c; c.l = b-a;
  c.dat = malloc(c.l*sizeof(int)); for(int i=a;i<b;i++) { c.dat[i] = s.dat[i]; }
  return c; }
Samp samplen(Samp buf, va_list vl) { int a = va_arg(vl,int); int b = va_arg(vl,int);
  Samp c = sample(buf,a,b); free(buf.dat); return c; }

Samp loop(Samp a, int amt) { Samp c; c.l = a.l*amt;
  c.dat = malloc(c.l*sizeof(int)); for(int i=0;i<c.l;i++) { c.dat[i] = a.dat[i%a.l]; }
  return c; }
Samp loopn(Samp buf, va_list vl) { int a = va_arg(vl,int);
  Samp c = loop(buf,a); free(buf.dat); return c; }

/* == Functions for transforming SampArr ========== */

Samp concat(SampArr a) { Samp b; b.l = 0; for(int i=0;i<a.len;i++) { b.l += a.d[i].l; }
  b.dat = malloc(b.l*sizeof(int));
  for(int i=0,e=0;i<b.l&&e<a.len;i+=a.d[e].l,e++) { b = para_(b,a.d[e],i); }
  return b; }
Samp concat_pad(SampArr a, int pad) { Samp b; b.l = a.len*pad;
  b.dat = malloc(b.l*sizeof(int));
  for(int e=0;e<a.len;e++) { b = para_(b,a.d[e],e*pad); } return b; }

/* == Main functions and IO - Base ==== */

// fix SAMPLE_COUNT
// purposefully specific to "key.wav".
/*int *init_key(sf_count_t sc) { int *buffer = malloc(SAMPLE_COUNT*sizeof(int));
  SF_INFO sfin; memset(&sfin, 0, sizeof(sfin)); SNDFILE *in;
  sfin.samplerate = SAMPLE_RATE;
  sfin.frames	= sc;
  sfin.channels = 1;
  sfin.format	= (SF_FORMAT_WAV | SF_FORMAT_PCM_16);
  if(!(in = sf_open("key3.wav", SFM_READ, &sfin))) { printf("no"); free(buffer); return 1; }
  sf_read_int(in,buffer,sc); sf_close(in); return buffer; }*/

void play(SNDFILE *out, sf_count_t c, Samp s) {
  if(sf_write_int(out,s.dat,c*s.l) != c*s.l) { puts(sf_strerror(out)); } }
//void play_s(SNDFILE *out, sf_count_t c, SampArr s) {
//  for(int i=0;i<s.n;i++) { play(out,c,s.dat[i]); } }

// stat = SFM_READ or SFM_WRITE
SNDFILE *sf_init_1(const char *name, sf_count_t len, int stat) { SF_INFO res;
  memset(&res, 0, sizeof(res)); res.samplerate = SAMPLE_RATE;
  res.frames = len; res.channels = 1;
  res.format = (SF_FORMAT_WAV | SF_FORMAT_PCM_16);
  return sf_open(name, stat, &res); }

void with_sound(const char *dest, Sound src, int chan, wsnd f, ...) {
  SNDFILE *s = sf_init_1(src.name, src.len, SFM_READ);
  Samp sd; sd.l = src.len; sd.dat = malloc(src.len*sizeof(int));
  sf_read_int(s,sd.dat,src.len); sf_close(s);

  va_list vl; va_start(vl,f);
  Samp dd = f(sd,vl); va_end(vl); 
  SNDFILE *d = sf_init_1(dest, dd.l, SFM_WRITE);
  play(d,chan,dd); sf_close(d); free(dd.dat); }

/*int main(int argc, char **argv) { SNDFILE *out;

  //int *buffer = init_key(SAMPLE_COUNT/2); Samp s = { SAMPLE_COUNT/2, buffer };

  //play(out,sfout.channels,c);
  // reminder: compiler must know that literal is of the type of the arg if var_arg.
  with_sound("keyB.wav",snd("key3.wav",22050),1,sc_edo_c,2.0,12);
  return 0; }*/
