#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  INFINITY 0xFFFFFFFFFFFFFFFF
#define  UNSET    0xFFFFFFFF
#define  LOCKED   0xFFFFFFFE

typedef struct {
  uint32_t rows;
  uint32_t cols;
  uint32_t **data;
  uint32_t **best;
} dataset;

//uint64_t   OPT_Path(uint32_t originX, uint32_t originY, uint32_t targetX, int32_t targetY, uint32_t data[][], uint32_t best[][], uint32_t rows, uint32_t cols);
uint64_t   OPT_Path(uint32_t originX, uint32_t originY, uint32_t targetX, uint32_t targetY, dataset *info);
uint64_t   min(uint64_t w, uint64_t x, uint64_t y, uint64_t z);
dataset*   getData(uint32_t rows, uint32_t cols);
uint32_t** make2DArr(uint32_t rows, uint32_t cols);
void       printDataSet(dataset *info);
void       expand_buf(char     **buf, uint32_t *size);
uint32_t   print_help(char* str);

/**
 */
int main(int argc, char* argv[]) {
  uint32_t rows=80,cols=80; //default
  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%u",&rows); }
  if(argc > 2) { sscanf(argv[2],"%u",&cols); }
  uint32_t sX=0,   sY=0;    //default
  uint32_t eX=rows-1,eY=cols-1; //default
  if(argc > 3) { sscanf(argv[3],"%u",&sX); }
  if(argc > 4) { sscanf(argv[4],"%u",&sY); }
  if(argc > 5) { sscanf(argv[5],"%u",&eX); }
  if(argc > 6) { sscanf(argv[6],"%u",&eY); }

  dataset *info = getData(rows, cols);
  printDataSet(info);
  printf("\n%llu\n",OPT_Path(sX,sY,eX,eY,info));
  printDataSet(info);
  return 0;
}

uint64_t OPT_Path(uint32_t originX, uint32_t originY, uint32_t targetX, uint32_t targetY, dataset* info) {
  if(originX == targetX && originY == targetY) return (info->best)[originX][originY] = (info->data)[originX][originY];
  if((info->best)[originX][originY]== UNSET) {
     (info->best)[originX][originY] = LOCKED;
     (info->best)[originX][originY] = (info->data)[originX][originY] +
     min(
       originX+1 >=info->rows ? INFINITY : OPT_Path(originX+1,originY  ,targetX,targetY,info),
       originY+1 >=info->cols ? INFINITY : OPT_Path(originX  ,originY+1,targetX,targetY,info),
       originX   ==0          ? INFINITY : OPT_Path(originX-1,originY  ,targetX,targetY,info),
       originY   ==0          ? INFINITY : OPT_Path(originX  ,originY-1,targetX,targetY,info)
     );
  }
  return (info->best)[originX][originY];
}

uint64_t min(uint64_t w, uint64_t x, uint64_t y, uint64_t z) {
       if(w <= x && w <= y && w <= z)   return w;
  else if(x <= w && x <= y && x <= z)   return x;
  else if(y <= w && y <= x && y <= z)   return y;
  else /*(z <= w && z <= x && z <= y)*/ return z;
}

void       printDataSet(dataset *info) {
  uint32_t i,j;
  printf("\n");
  for(i=0; i<info->rows; ++i) {
    for(j=0; j<info->cols; ++j) {
      printf("%u, ",(info->data)[i][j]);
    }
    printf("\n");
  }
  printf("\n");
  for(i=0; i<info->rows; ++i) {
    for(j=0; j<info->cols; ++j) {
      printf("%u, ",(info->best)[i][j]);
    }
    printf("\n");
  }
}

dataset* getData(uint32_t rows, uint32_t cols) {
  uint32_t len=5;           //default
  uint32_t i,j,k,val,end,word;
  char     c;
  char     *buf  =  (char*   ) malloc(len  * sizeof(char));
  dataset *outSet = (dataset*) malloc(sizeof(dataset));
  outSet->rows = rows;
  outSet->cols = cols;
  outSet->data = make2DArr(rows,cols);
  outSet->best = make2DArr(rows,cols);

  // Read in Grid Data
  for(end=i=0;    !end && i<rows; ++i) {
    for(j=0;        !end && j<cols; ++j) {
      for(k=0,word=0; !end && !word  && (c=fgetc(stdin)); ++k) {
        if(k>=len)
          expand_buf(&buf,&len);
        switch(c) {
          case EOF : end = 1;  //conditionally
                     if(k==0)  //fallthrough
                       break;
          case '\n':           //fallthrough
          case ',' :           //fallthrough
          case ' ' : buf[k] = '\0';
                     sscanf(buf,"%u",&val);
                     outSet->data[i][j] = val;
                     word=1;
                     break;
          default  : buf[k] = c; break;
        }
      }
    }
  }
  return outSet;
}

uint32_t** make2DArr(uint32_t rows, uint32_t cols) {
  uint32_t i,len = cols * sizeof(uint32_t);
  uint32_t **arr = (uint32_t**) malloc(rows * sizeof(uint32_t*));
  for(i=0; i<rows; ++i) {
    arr[i] = (uint32_t*) malloc(len);
    memset(arr[i],UNSET,len);
  }
  return arr;
}

void expand_buf(char     **buf, uint32_t *size) {
  *size *= 2;
  *buf   = realloc(*buf,*size * sizeof(char));
}

uint32_t print_help(char* str) {
  printf("  Usage: %s <rows> <cols> \n",str);
  printf("  Calculates the minimum path from any cell in the left most colum \n");
  printf("  to any cell in the right most column in a rectangle read in from STDIN \n");
  printf("  with a height of <rows> and a width of <cols>. \n");
  printf("  Incorrectly formatted input data may cause unexpected behavior.\n");
  return 0;
}
