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
typedef struct { int n; Samp *dat; } SampSeq;

// sqrt(octave_interval,tones/octave) => sqrt(2,12) = 1 semitone interval
// this function is fairly naive.
Samp pitch(Samp buf, float ratio) { Samp buf2; buf2.l = ceil((float)buf.l/ratio);
  buf2.s = malloc(buf2.l*sizeof(int)); float e = 0;
  for(int i=0;i<buf2.l;i++) { buf2.s[i] = buf.s[(int)e]; e+=ratio; } return buf2; }

SampSeq sc_edo(Samp key, float ratio, int notes) {
  SampSeq s; s.dat = malloc(notes*sizeof(Samp)); s.n = notes;
  for(int i=0;i<notes;i++) { s.dat[i] = pitch(key,pow(pow(ratio,1.0/notes),i)); }
  return s; }

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
void play_s(SNDFILE *out, sf_count_t c, SampSeq s) {
  for(int i=0;i<s.n;i++) { play(out,c,s.dat[i]); } }

void ss_free(SampSeq s) { for(int i=0;i<s.n;i++) { free(s.dat[i].s); } free(s.dat); }

int main(int argc, char **argv) { SNDFILE *out;
  SF_INFO sfout; int k;
  SF_INFO sfin; SNDFILE *in;

  //if (!(buffer = malloc (2 * SAMPLE_COUNT * sizeof (int)))) {
  //  printf ("Malloc failed.\n"); exit (0); }

  memset(&sfout, 0, sizeof(sfout));

  sfout.samplerate = SAMPLE_RATE;
  sfout.frames = SAMPLE_COUNT*34;
  sfout.channels = 1;
  sfout.format = (SF_FORMAT_WAV | SF_FORMAT_PCM_16);

  int *buffer = init_key(SAMPLE_COUNT/2); Samp s = { SAMPLE_COUNT/2, buffer };

  // sequences of 12-EDO, 24-EDO, and 31-EDO.
  // tempo rises as pitch rises since the notes are not padded.
  SampSeq e = sc_edo(s,2,12);
  SampSeq f = sc_edo(s,2,24);
  SampSeq d = sc_edo(s,2,31);

  if (!(out = sf_open ("sine.wav", SFM_WRITE, &sfout))) {
    printf ("Error : Not able to open output file.\n");
    free(buffer); return 1; }

  play_s(out,sfout.channels,e); play_s(out,sfout.channels,f);
  play_s(out,sfout.channels,d);

  //if (sfinfo.channels == 1) {
  //  for (k = 0 ; k < SAMPLE_COUNT ; k++) {
  //    buffer [k] = AMPLITUDE * sin (FREQ * 2 * k * M_PI); } }

  sf_close(out);
  free(buffer); ss_free(e); ss_free(f); ss_free(d);
  return 0; }
