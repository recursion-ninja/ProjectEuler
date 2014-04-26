#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void     expand_buf(char ** const buf, uint32_t * const size);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t rows=80,cols=80; /* default */
  uint32_t len=5;           /* default */
  uint32_t i,j,k,val,end,word;
  uint32_t **data;
  char     c;
  char     *buf  = malloc(len  * sizeof(char));
  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&rows); }
  if(argc > 2) {  sscanf(argv[2],"%u",&cols); }

  data = (uint32_t**) malloc(rows * sizeof(uint32_t*));
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
  /* up rows */
  for(i=rows-1; i<rows ; --i) {  /* both loops break after control variable decrements from zero to uint32_t MAX_VALUE */
    for(j=i,k=cols-1; j<rows && k<cols; ++j,--k) {
           if(j+1 <  cols && k+1 <  rows) data[j][k] += (data[j+1][k] < data[j][k+1]) ? data[j+1][k] : data[j][k+1];
      else if(j+1 <  cols && k+1 >= rows) data[j][k] +=  data[j+1][k];
      else if(j+1 >= cols && k+1 <  rows) data[j][k] +=  data[j][k+1];
      else { ; } /* do nothing */
    }
  }
  /* across cols */
  for(i=cols-2; i<cols; --i) {  /* both loops break after control variable decrements from zero to uint32_t MAX_VALUE */
    for(j=0,k=i; j<rows && k<cols; ++j,--k) {
           if(j+1 <  cols && k+1 <  rows) data[j][k] += (data[j+1][k] < data[j][k+1]) ? data[j+1][k] : data[j][k+1];
      else if(j+1 <  cols && k+1 >= rows) data[j][k] +=  data[j+1][k];
      else if(j+1 >= cols && k+1 <  rows) data[j][k] +=  data[j][k+1];
      else { ; } /* do nothing */
    }
  }
/*
  for(i=0; i<rows; ++i) {
    printf("%02u: ",i);
    for(j=0; j<cols; ++j)
      printf("%02u ",data[i][j]);
    printf("\n");
  }
*/
  printf("%u\n",data[0][0]);
  return 0;
}
/*
uint32_t getOffset(uint32_t x, uint32_t y) {
  return ((x+1)*(x+2))/2 -((x+1)-(y-1)) +1;
}

void expand_arr(uint32_t **arr, uint32_t *size) {
  *size *= 2;
  *arr   = realloc(*arr,*size * sizeof(uint32_t));
}
*/
void expand_buf(char ** const buf, uint32_t * const size) {
  *size *= 2;
  *buf   = (char*) realloc(*buf,*size * sizeof(char));
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <rows> <cols> \n",str);
  printf("  Calculates the minimum path from the upper left corner \n");
  printf("  to the lower right corner of a rectangle read in from STDIN \n");
  printf("  with a height of <rows> and a width of <cols>. \n");
  printf("  Incorrectly formatted input data may cause unexpected behavior.\n");
  return 0;
}
