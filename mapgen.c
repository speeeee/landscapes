#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#define MH 255
#define HH 127

#define SZ 2048

void ds_map(int sz,int8_t **bmap, int bh, int l, int r, int t, int b) {
  int x_cnt = (r+l)/2; int y_cnt = (t+b)/2;
  int cv = bmap[x_cnt][y_cnt] = (bmap[l][t]+bmap[r][t]+bmap[l][b]+bmap[r][b])/4
    - (rand()%bh-bh/2)/2;
  bmap[x_cnt][t] = ((bmap[l][t]+bmap[r][t])-(rand()%bh-bh/2))/2;
  bmap[x_cnt][b] = ((bmap[l][b]+bmap[r][b])-(rand()%bh-bh/2))/2;
  bmap[l][y_cnt] = ((bmap[l][t]+bmap[l][b])-(rand()%bh-bh/2))/2;
  bmap[r][y_cnt] = ((bmap[r][t]+bmap[r][b])-(rand()%bh-bh/2))/2;
  if(r-l>2) { int nbh = ceil((double)bh*(double)pow(2.0,-0.75));
    ds_map(sz,bmap,nbh,l,x_cnt,t,y_cnt); ds_map(sz,bmap,nbh,x_cnt,r,t,y_cnt);
    ds_map(sz,bmap,nbh,l,x_cnt,y_cnt,b); ds_map(sz,bmap,nbh,x_cnt,r,y_cnt,b); } }

/*void ds_mapf(int sz, int bh, int sp, int l, int r, int t, int b) {
  int x_cnt = (r+l)/2; int y_cnt = (t+b)/2;*/
  

void out_map(int sz,int8_t **bmap,FILE *f) { int8_t e = 0;
  for(int i=0;i<pow(sz,2);i++) {
    fwrite(&bmap[i%SZ][i/SZ],sizeof(int8_t),1,f);
    fwrite(&e,sizeof(int8_t),1,f); } } 

int main(int argc, char **argv) { int8_t **amap = malloc(SZ*sizeof(int8_t *));
  for(int i=0;i<SZ;i++) { amap[i] = malloc(SZ*sizeof(int8_t)); }
  int seed = time(NULL); srand(seed); 
  amap[0][0] = MH/2; amap[SZ-1][0] = MH/2;
  amap[SZ-1][SZ-1] = MH/2; amap[0][SZ-1] = MH/2;
  ds_map(SZ,amap,MH,0,SZ-1,SZ-1,0);

  FILE *f = fopen("output.nmp","wb"); out_map(SZ,amap,f); fclose(f);
  free(amap); return 0; }
