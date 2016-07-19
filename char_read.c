#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#include <math.h>

typedef struct { int8_t r; int8_t g; int8_t b; } col;
typedef struct { col *dat; int w; int h; }       img;

// load BMP
img read_img(FILE *f) { int8_t header[54];
  if(fread(header,54,1,f)!=54) { printf("error: could not read file\n"); exit(0); }
  img im; int pos = *(int*)&(header[0x0A]);
  im.w = *(int32_t *)&(header[0x12]); im.h = *(int32_t *)&(header[0x16]);
  int8_t *dat; fread(dat,w*h*3,1,f);
  for(int i=0;i<w*h*3;i+=3) { im.dat[i/3] = { dat[i], dat[i+1], dat[i+2] }; }
  return im; }
