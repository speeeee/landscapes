#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#include <math.h>

typedef struct { int8_t r; int8_t g; int8_t b; } col;
typedef struct { col *dat; int w; int h; } img;
typedef struct { int *bl; col ps; int w; int h; } tile;

// load BMP
img read_img(FILE *f) { int8_t header[54];
  if(fread(header,54,1,f)!=54) { printf("error: could not read file\n"); exit(0); }
  img im; int pos = *(int*)&(header[0x0A]);
  im.w = *(int32_t *)&(header[0x12]); im.h = *(int32_t *)&(header[0x16]);
  int8_t *dat; fread(dat,w*h*3,1,f);
  for(int i=0;i<w*h*3;i+=3) { im.dat[i/3] = { dat[i], dat[i+1], dat[i+2] }; }
  return im; }

//          bool val of point, width of pt, height of pt, index of pt, image
int test_px(int t, int tw, int th, int ind, tile b) {
  if(b.w%tw!=0||b.h%th!=0) { return 0; }
  int i; for(i=0;i<tw*th&&b.bl[ind*b.w/tw+i+i/(b.w/tw)*b.w]==t;i++);
  return i==tw*th; }

// tile b should have a width and height that are both multiples of their counterparts 
// in tile a.
int tile_cmp(tile a, tile b) { int i;
  for(i=0;i<a.w*a.h&&test_px(a.bl[i],a.w,a.h,i,b);i++); return i==a.w*a.h; }
