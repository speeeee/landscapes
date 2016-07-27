#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#include <math.h>
#include <string.h>

#define PARSER ("nagyp")

typedef struct { uint8_t r; uint8_t g; uint8_t b; uint8_t a; } col;
typedef struct { col *dat; int w; int h; } img;

img read_img(FILE *);

int ceq(col a, col b) { return (a.r==b.r&&a.g==b.g&&a.b==b.b); }

col mcol(uint8_t r, uint8_t g, uint8_t b) { return (col) { r, g, b }; }

// load BMP
img read_img(FILE *f) { uint8_t header[54];
  if(fread(header,1,54,f)!=54) { printf("error: could not read file\n"); /*exit(0);*/ }
  img im; int pos = *(int*)&(header[0x0A]); int bpp = *(int*)&(header[0x1C])/8;
  im.w = *(int32_t *)&(header[0x12]); im.h = abs(*(int32_t *)&(header[0x16]));
  int isz = im.w*im.h*bpp;
  uint8_t *dat = malloc(isz*sizeof(uint8_t));
  int e = fread(dat,1,isz,f);
  im.dat = malloc(isz/bpp*sizeof(col));
  for(int i=0;i<isz;i+=bpp) { 
    im.dat[i/bpp] = (col) { dat[i], dat[i+1], dat[i+2], dat[i+3] }; }
  return im; }

void pr_img(img a) { printf("%i ",a.w); printf("%i ",a.h);
  for(int i=0;i<a.w*a.h;i++) { printf("%i,",a.dat[i].r);
    printf("%i,",a.dat[i].g); printf("%i,",a.dat[i].b);
    printf("%i ",a.dat[i].a); } }

// simple comparison main
int main(int argc,char **argv) {
  FILE *g = fopen("prog.bmp","rb");
  img bg = read_img(g); //printf("%i\n",(int)bg.dat[2].g);
  fclose(g); FILE *e = fopen("graph.bmp","rb"); img gr = read_img(e); fclose(e);
  pr_img(gr); pr_img(bg); return 0; }
