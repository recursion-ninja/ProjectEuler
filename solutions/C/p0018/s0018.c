#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t getOffset(const uint32_t x, const uint32_t y);
void     print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t size = 10,len = 5; /* default */
  uint32_t i,j,val,end,count,height,*list;
  char     *buf;
  int      c;

  if(argc > 1 &&!strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;

  list = malloc(size * sizeof *list);
  buf  = malloc(len  * sizeof *buf );
  /* Read in Triangle Data */
  for(count=height=end=i=0; !end && (c=fgetc(stdin)); ) {
    if(i>=len)
      buf = realloc(buf, (len*=2) * sizeof *buf);
    switch(c) {
      case EOF : end = 1;  /* conditionally */
                 if(i==0)  /* fallthrough  */
                   break;
      case '\n': ++height; /* fallthrough */
      case ' ' : buf[i] = '\0';
                 sscanf(buf,"%u",&val);
                 if(count>=size)
                   list = realloc(list, (size*=2) * sizeof *list);
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
  free(list);
  free(buf);
  return EXIT_SUCCESS;
}

uint32_t getOffset(const uint32_t x, const uint32_t y) {
  return ((x+1)*(x+2))/2 -((x+1)-(y-1)) +1;
}

void print_help(const char * const str) {
  printf("  Usage: %s \n",str);
  printf("  Calculates the maximum sumation path from \n");
  printf("  top of pyramid to bottom read in from STDIN.\n");
  printf("  Incorrectly formatted input data may cause \n");
  printf("  unexpected behavior.\n");
}
