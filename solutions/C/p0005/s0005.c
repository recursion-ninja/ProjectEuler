#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t gcd_32(const uint32_t u, const uint32_t v);
void     print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t product =  1;
  uint32_t limit   = 20; /* default */
  uint32_t i;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&limit);

  for(i=limit; i>0; --i)
    product *= (product < i) ? i : (!!(product%i)) ? i/gcd_32(product,i) : 1;

  printf("%llu\n",product);
  return EXIT_SUCCESS;
}

uint32_t gcd_32(const uint32_t u, const uint32_t v) {
  /* simple cases (termination) */
  if (u == v)
    return u;
  if (u == 0)
    return v;
  if (v == 0)
    return u;

  /* look for factors of 2 */
  if (~u & 1) { /* u is even */
    if (v & 1)  /* v is odd  */
      return gcd_32(u >> 1, v);
    else /* both u and v are even */
      return gcd_32(u >> 1, v >> 1) << 1;
  }
  if (~v & 1) /* u is odd, v is even */
    return gcd_32(u, v >> 1);

  /* reduce larger argument */
  if (u > v)
    return gcd_32((u - v) >> 1, v);
  return gcd_32((v - u) >> 1, u);
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates largest number\n");
  printf("  which is evenly divisible by all numbers\n");
  printf("  from 1 to limit (including limit)\n");
  printf("  * limit : a decimal number inclusive bound\n");
  printf("          : default = 20\n");
}
