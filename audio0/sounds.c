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
#define		FREQ			(440.0 / SAMPLE_RATE)}

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

  if (sfinfo.channels == 1) {
    for (k = 0 ; k < SAMPLE_COUNT ; k++) {
      buffer [k] = AMPLITUDE * sin (FREQ * 2 * k * M_PI); } }

  if (sf_write_int (file, buffer, sfinfo.channels * SAMPLE_COUNT) != 
      sfinfo.channels * SAMPLE_COUNT) { puts (sf_strerror (file)); }


  sf_close (file);
  free (buffer);
  return 0; }
