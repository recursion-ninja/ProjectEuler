#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
/**
 * DO NOT Use time-memory trade-off aproach
 * Will blow your stack.
 */
uint32_t isPrime(const uint32_t n);
uint32_t print_help(const char * const str);

int  main(int argc, char* argv[]) {
  uint32_t limit  = 2000000; /* default */
  uint32_t curr,toggle;
  uint64_t sum = 0;

  if(argc  > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc  > 1)  { sscanf(argv[1],"%u",&limit); }
  if(limit > 2) sum += 2;
  if(limit > 3) sum += 3;
  /* check each positive integer > 4 whose value mod 6 == 1,5 */
  for(curr=5,toggle=0; curr<limit; curr+=toggle?4:2,toggle=!toggle)
    if(isPrime(curr))
      sum += curr;
  printf("%llu\n",sum);
  return 0;
}

uint32_t isPrime(const uint32_t n) {
  uint32_t i,toggle,bound;
  uint32_t prime = !!(n%2) && !!(n%3);
  for(i=5,toggle=0,bound=(uint32_t)(sqrt(n)+1); prime && i<bound; i+=toggle?4:2) {
    prime &= !!(n%i);
  }
  return prime;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the sum of all primes \n");
  printf("  which are less then the limit \n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 2000000\n");
  return 0;
}
