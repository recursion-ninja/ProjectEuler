#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * Use memotization & dynamic programming
 */

void print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit = 1000000; /* default */
  uint32_t i, count,largest;
  uint32_t *memo;
  uint64_t curr;

  if(argc > 1 &&!strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&limit);

  memo = calloc(limit, sizeof *memo);

  for(i=1; i<limit; ++i)
    for(curr=i,count=0; !memo[i] && curr!=1; ++count,curr=(curr&1)?curr*3+1:curr/2)
      if(curr < i)
        memo[i] = count + memo[curr];

  for(i=0,largest=0; i<limit; ++i)
    if(memo[i] > memo[largest])
      largest = i;

  printf("%u\n",largest);

  free(memo);
  return EXIT_SUCCESS;
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the number below <limit> \n");
  printf("  which creates the longest Colatz Chain \n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 1000000\n");
}
