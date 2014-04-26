#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

int print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t target = 10001; /* default */
  uint32_t count  = 0;
  uint32_t i,j,b;
  uint64_t *primes;
  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&target); }
  primes = (uint64_t*) malloc(target * sizeof(uint64_t));
  /* Check/add 2 & 3 statically when appropriate */
  if(target > 0) primes[count++] = 2;
  if(target > 1) primes[count++] = 3;
  /* primitively add new primes incrementing mod 6 */
  for(i=5; count<target; i+=6) {
    b = 1;
    for(j=0; b && j<count; ++j)
      b &= !!((i+0)%primes[j]);
    if(b)
      primes[count++] = i+0;
    b = count < target;
    for(j=0; b && j<count; ++j)
      b &= !!((i+2)%primes[j]);
    if(b)
      primes[count++] = i+2;
  }
  printf("%llu\n",primes[target-1]);
  return 0;
}

int print_help(const char * const str) {
  printf("  Usage: %s <index> \n",str);
  printf("  Calculates the value of the ith prime number\n");
  printf("  where i is equal to index\n");
  printf("  * index : a decimal number\n");
  printf("          : default = 10001\n");
  return 0;
}

