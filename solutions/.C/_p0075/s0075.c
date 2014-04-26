#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * NOTES:
 * Itterate through all possible points
 * Check for invalid points
 * Check point configrations that are right triangles
 * Don't count triangles twice
 */

uint32_t isRightTriangle(const uint32_t a, const uint32_t b, const uint32_t c);
void     print_help     (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit = 1500000; /* Default */
  uint32_t a,b,c,i,count;
  uint8_t  *memo;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
     sscanf(argv[1],"%u",&limit);

  memo = malloc(limit * sizeof *memo);

  for(c=2; c<=limit-2; ++c)
    for(b=1; b<c; ++b)
      for(a=1; a<=b && a+b+c<=limit; ++a)
        if(isRightTriangle(a,b,c))
          if(memo[(i=a+b+c-1)] < 2)
            (memo[i])++;

  for(i=count=0; i<limit; ++i)
    if(memo[i]==1)
      ++count;

  printf("%u\n",count);
  return EXIT_SUCCESS;
}

uint32_t isRightTriangle(const uint32_t a, const uint32_t b, const uint32_t c) {
  return a*a + b*b == c*c;
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculate the quantity of unique right triangles\n");
  printf("  formed by points O, P, Q such that:\n");
  printf("  O = (0,0), P = (a,b), Q = (c,d) where a,b,c,d are integers\n");
  printf("  and a,b,c,d are in the range [0, <limit>]\n");
  printf("  * limit  : a decimal number\n");
  printf("           : default = 50\n");
}
