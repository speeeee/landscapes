#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#include <math.h>
#include <string.h>

#define PARSER ("nagyp")

typedef struct { uint8_t r; uint8_t g; uint8_t b; } col;
typedef struct { col *dat; int w; int h; } img;
typedef struct { int *bl; col ps; int w; int h; } tile;

typedef struct { int x; int y; int w; int h; } fra;
typedef struct { char *f; fra a; fra b; int8_t abottom; int8_t bbottom; } fun;
typedef struct { tile t; fun f; } gra;

gra *G_ftbl; int G_fsz = 0;

img read_img(FILE *);

int ceq(col a, col b) { return (a.r==b.r&&a.g==b.g&&a.b==b.b); }

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
    im.dat[i/bpp] = (col) { dat[i], dat[i+1], dat[i+2] }; }
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
  for(int i=0;i<a.w*a.h;i++) { n.bl[i] = ceq(a.dat[i],c); } return n; }

char *reade(FILE *f) { char *x = malloc(sizeof(char)); int c; int i;
  for(i=2;(c = fgetc(f)) != EOF;i++) { x = realloc(x,i*sizeof(char));
    x[i] = c; } x[i] = '\0'; return x; }

img extr(fra a, img b) { img q; q.w = a.w; q.h = a.h; 
  q.dat = malloc(q.w*q.h*sizeof(col));
  for(int i=0;i<q.w*q.h;i++) { q.dat[i] = b.dat[a.x+i%a.w+i/a.w*b.w]; } return q; }

// warning: function has no bounds checking yet.
fun pimg(img x, gra *tbl, int tsz) { int i;
  for(i=0;i<tsz&&tile_cmp(tbl[i].t,silh(x,tbl[i].t.ps));i++); return tbl[i].f; }
// very rough currently; will make nicer later.
char *eval(fun x, img bg) { fun a = pimg(extr(x.a,bg),G_ftbl,G_fsz); 
  fun b = pimg(extr(x.b,bg),G_ftbl,G_fsz);
  char *sa; if(a.abottom) { sa = a.f; } else { sa = eval(a,bg); }
  char *sb; if(b.bbottom) { sb = b.f; } else { sb = eval(b,bg); }
  char *e = malloc((strlen(sa)+strlen(sb)+strlen(x.f)+2)*sizeof(char));
  strcpy(e,x.f); strcat(e," "); strcat(e,sa); strcat(e," "); strcat(e,sb);

  char *com = malloc((strlen(e)+strlen(PARSER)+1)*sizeof(char));
  strcpy(com,PARSER); strcat(e," "); strcat(com,e); free(e); free(sa); free(sb);
  FILE *p = popen(com,"r"); free(com); char *r = reade(p); pclose(p); return r; }

int main(int argc,char **argv) { G_ftbl = malloc((++G_fsz)*sizeof(gra));
  int *bl = malloc(9*sizeof(int)); for(int i=0;i<9;i++) { bl[i] = i%2; }
  G_ftbl[0] = (gra) { (tile) { bl, (col) { 0, 255, 0 }, 3, 3 },
                      (fun) { "0", (fra) { 0,0,0,0 }, (fra) { 0,0,0,0 }, 1, 1 } };
  FILE *g = fopen("graph.bmp","rb");
  img bg = read_img(g); //printf("%i\n",(int)bg.dat[2].g);
  fclose(g); return 0; }
