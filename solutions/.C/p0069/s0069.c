#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
/**
 * REMEMBER:
 * Problem equivilent to largest product of primes < limit
 * Answer: 2*3*5*7*11*13*17 = 510510 < 1000000
 */
uint64_t isPrime   (const uint64_t n, uint64_t ** const primes, uint64_t * const count, uint64_t * const size);
void     addPrime  (const uint64_t n, uint64_t ** const primes, uint64_t * const count, uint64_t * const size);
uint64_t expand_arr(uint64_t ** const arr, uint64_t * const size);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t limit=1000000;   /* default */
  uint64_t count=0,size=10; /* default */
  uint64_t n,product;
  uint64_t *primes = malloc(size * sizeof(uint64_t));

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%llu",&limit); }

  for(n=2,product=1,count=0; product*n <= limit && n<=limit; ++n)
    if(isPrime(n,&primes,&count,&size))
      product *= n;
  printf("%llu\n",product);
  return 0;
}

uint64_t isPrime(const uint64_t n, uint64_t ** const primes, uint64_t * const count, uint64_t * const size) {
  uint64_t i,prime,bound;
  for(i=0,prime=1,bound=(uint64_t)sqrt(n)+1; prime && i<*count && (*primes)[i]<=bound; ++i)
    prime = n%(*primes)[i];
  if(prime)
    addPrime(n,primes,count,size);
  return prime;
}

void addPrime(const uint64_t n, uint64_t ** const primes, uint64_t * const count, uint64_t * const size) {
  if(*count >= *size)
    if(!expand_arr(primes,size))
      exit(1337); /* realloc failure */
  (*primes)[(*count)++] = n;
}

uint64_t expand_arr(uint64_t ** const primes, uint64_t * const size) {
  *size  *= 2;
  *primes = realloc(*primes, *size * sizeof(uint64_t));
  return *primes!=NULL;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the value of n <= <limit> \n");
  printf("  for which n/phi(n) is a maximum\n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 1000000\n");
  return 0;
}
