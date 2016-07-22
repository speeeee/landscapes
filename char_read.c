#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#include <math.h>
#include <string.h>

typedef struct { uint8_t r; uint8_t g; uint8_t b; } col;
typedef struct { col *dat; int w; int h; } img;
typedef struct { int *bl; col ps; int w; int h; } tile;

typedef struct { double x; double y; } pos;
typedef struct { char *f; img a; img b; } fun;

int ceq(col a, col b) { return (a.r==b.r&&a.g==b.g&&a.b==b.b); }

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

tile silh(img a, col c) { tile n; n.bl = malloc(a.w*a.h*sizeof(int));
  for(int i=0;i<a.w*a.h;i++) { n.bl[i] = ceq(img.dat[i],c); } return n; }

char *eval(fun);
fun pimg(img,tile *);

// warning: function has no bounds checking yet.
fun pimg(img x, tile *tbl, int tsz) { int i;
  for(i=0;i<tsz&&tile_cmp(tbl[i],silh(x,tbl[i].ps));i++); return tbl[i]; }
/*char *eval(fun x) { fun a = pimg(x.a); fun b = pimg(x.b);
  char *e = malloc((strlen(a.f)+strlen(b.f)+strlen(x.f))*sizeof(char));
  strcpy(e,x.f); strcat(e,a.f); strcat(e,*/
