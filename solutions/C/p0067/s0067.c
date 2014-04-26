#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t getOffset(const uint32_t x, const uint32_t y);
void     expand_arr(uint32_t ** const arr, uint32_t * const size);
void     expand_buf(char     ** const buf, uint32_t * const size);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t size = 10,len = 5; /* default */
  uint32_t i,j,val,end,count,height;
  uint32_t *list = (uint32_t*) malloc(size * sizeof(uint32_t));
  char     *buf  = (char*    ) malloc(len  * sizeof(char));
  char     c;

  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }

  /* Read in Triangle Data */
  for(count=height=end=i=0; !end && (c=fgetc(stdin)); ) {
    if(i>=len)
      expand_buf(&buf,&len);
    switch(c) {
      case EOF : end = 1;  /* conditionally */
                 if(i==0)  /* fallthrough  */
                   break;
      case '\n': ++height; /* fallthrough */
      case ' ' : if(count>=size)
                   expand_arr(&list,&size);
                 buf[i] = '\0';
                 sscanf(buf,"%u",&val);
                 list[count++] = val;
                 i=0;
                 break;
      default  : buf[i++] = c; break;
    }
  }
  /* Process Triangle Data */
  /* both loops break after control variable decrements from zero to UINT32_MAX */
  for(i=height-2; i<height; --i)
    for(j=i; j<=i; --j)
      list[getOffset(i,j)] += (list[getOffset(i+1,j)] > list[getOffset(i+1,j+1)]) ? list[getOffset(i+1,j)] : list[getOffset(i+1,j+1)];

  printf("%u\n",list[0]);
  return 0;
}

uint32_t getOffset(const uint32_t x, const uint32_t y) {
  return ((x+1)*(x+2))/2 -((x+1)-(y-1)) +1;
}

void expand_arr(uint32_t ** const arr, uint32_t * const size) {
  *size *= 2;
  *arr   = realloc(*arr,*size * sizeof(uint32_t));
}

void expand_buf(char     ** const buf, uint32_t * const size) {
  *size *= 2;
  *buf   = realloc(*buf,*size * sizeof(char));
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s \n",str);
  printf("  Calculates the maximum sumation path from \n");
  printf("  top of pyramid to bottom read in from STDIN.\n");
  printf("  Incorrectly formatted input data may cause \n");
  printf("  unexpected behavior.\n");
  return 0;
}
