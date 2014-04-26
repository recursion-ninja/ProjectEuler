#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * REMEMBER: One corner in each spiral layer is a perfect square
 * other corner values are a constant offest of the size of the square edge
 */
uint32_t print_help  (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit = 1001; /* default */
  uint64_t i,j,sum,sqr;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&limit); }

  /* Conditionally initialize sum & i */
  sum = limit&1 ? 1 : 0;
  i   = limit&1 ? 3 : 2;

  for(; i<=limit; i+=2)
    for(j=0,sqr=i*i; j<4; ++j)
      sum += sqr - (i-1)*j;

  printf("%llu\n",sum);
  return EXIT_SUCCESS;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Finds the sum of the diagonals on a spiral,\n");
  printf("  Whose edge is length <limit> and is of the form:\n\n");
  printf("        21 22 23 24 25 \n");
  printf("        20  7  8  9 10 \n");
  printf("        19  6  1  2 11 \n");
  printf("        18  5  4  3 12 \n");
  printf("        17 16 15 14 13 \n\n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 1001\n");
  return 0;
}
