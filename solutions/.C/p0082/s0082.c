#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void     expand_buf(char ** const buf, uint32_t * const size);
uint32_t print_help(const char * const str);
/**
 * for each column moving left to right
 * for each cell in each column
 * traverse up and down the current colum to find the minimal path
 * from any cell in the previous column to current cell
 */
int main(int argc, char* argv[]) {
  uint32_t rows=80,cols=80; /* default */
  uint32_t len=5;           /* default */
  uint32_t i,j,k,val,end,word,path,best;
  uint32_t **data,*temp;
  char     c;
  char     *buf  = malloc(len  * sizeof(char));

  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%u",&rows); }
  if(argc > 2) { sscanf(argv[2],"%u",&cols); }

  data = (uint32_t**) malloc(rows * sizeof(uint32_t*));
  temp = (uint32_t*)  malloc(rows * sizeof(uint32_t ));
  for(i=0; i< rows; ++i)
    data[i] = (uint32_t*) malloc(cols * sizeof(uint32_t));

  /* Read in Grid Data */
  for(end=i=0;    !end && i<rows; ++i) {
    for(j=0;        !end && j<cols; ++j) {
      for(k=0,word=0; !end && !word  && (c=fgetc(stdin)); ++k) {
        if(k>=len)
          expand_buf(&buf,&len);
        switch(c) {
          case EOF : end = 1;  /* conditionally */
                     if(k==0)  /* fallthrough   */
                       break;
          case '\n':           /* fallthrough   */
          case ',' :           /* fallthrough   */
          case ' ' : buf[k] = '\0';
                     sscanf(buf,"%u",&val);
                     data[i][j] = val;
                     word=1;
                     break;
          default  : buf[k] = c; break;
        }
      }
    }
  }

  /* Process Rectangle Data */
  for(j=1; j<cols; ++j) {
    for(i=0; i<rows; ++i) {
      /* check down */
      for(k=1,path=0,best=data[i][j-1]; i+k < cols; path+=data[i+k][j],++k)
        if(best > data[i+k][j-1]+data[i+k][j]+path)
          best  = data[i+k][j-1]+data[i+k][j]+path;
      /* check up */
      for(k=1,path=0; i-k < cols; path+=data[i-k][j],++k)
        if(best > data[i-k][j-1]+data[i-k][j]+path)
          best  = data[i-k][j-1]+data[i-k][j]+path;
      temp[i] = best;
    }
    /* overwrite curr col with minimal paths values */
    for(i=0; i<rows; ++i)
      data[i][j] += temp[i];
  }
  /* find minimun in last column */
  for(i=1,best=0; i<rows; ++i)
    if(data[i][cols-1] < data[best][cols-1])
      best = i;

  printf("%u\n",data[best][cols-1]);
  return 0;
}

void expand_buf(char ** const buf, uint32_t * const size) {
  *size *= 2;
  *buf   = (char*) realloc(*buf,*size * sizeof(char));
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <rows> <cols> \n",str);
  printf("  Calculates the minimum path from any cell in the left most column \n");
  printf("  to any cell in the right most column in a rectangle read in from STDIN \n");
  printf("  with a height of <rows> and a width of <cols>. \n");
  printf("  Incorrectly formatted input data may cause unexpected behavior.\n");
  return 0;
}
