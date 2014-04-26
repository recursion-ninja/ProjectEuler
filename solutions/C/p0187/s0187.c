#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * Use the semiprime formula pi(x)^{(2)}
 * Uses (limit/32) bytes when gathering primes
 */

void     sieveOfEratosthenes_m2(const uint32_t limit, uint32_t ** const primes, uint32_t * const count);
uint32_t primeCount(const uint32_t n, const uint32_t * const arr, const uint32_t count);
void     print_help(char* str);

int main(int argc, char* argv[]) {
  uint32_t bound   = 100000000; /* default */
  uint32_t i,count,limit,semiPrimes;
  uint32_t *primes = NULL;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&bound);

  --bound; /* now less then or equal for all calculations */
  sieveOfEratosthenes_m2(bound/2,&primes,&count);
  limit = primeCount((uint32_t)floor(sqrt(bound)),primes,count);

  /* Use the semiprime formula */
  for(i=0,semiPrimes=0; i<limit; ++i)
    semiPrimes += -i +primeCount(bound/primes[i],primes,count);

  printf("%u\n",semiPrimes);

  free(primes);

  return EXIT_SUCCESS;
}

void sieveOfEratosthenes_m2(const uint32_t limit, uint32_t ** const primes, uint32_t * const count) {
  uint32_t i,j,size;
  uint8_t *bitSet = calloc(limit/16 +1, sizeof *bitSet);

/* Use bit shifting to use 1/16 memory  */
/* And improve cache performance        */
/* Only check odds                      */
/* Indexing:  x/2                       */
/* SetBit:    arr[n/8] |= 0x80 >> (n%8) */
/* GetBit:    arr[n/8]  & 0x80 >> (n%8) */
#define P_INDEX(x)     (x>>1)
#define SET_BIT(x,arr) (arr[(x>>3)] |= 0x80 >> (x%8))
#define GET_BIT(x,arr) (arr[(x>>3)]  & 0x80 >> (x%8))

  if(*primes != NULL)
    free(*primes);
  size    = 10;
  *primes = malloc(size * sizeof **primes);
  *count  = 0;

  if(limit < 2)
    return;

  for(i=3; i<=limit; i+=2)
    if(!GET_BIT(P_INDEX(i),bitSet))
      for(j=3*i; j<=limit; j+=2*i)
        SET_BIT(P_INDEX(j),bitSet);

  (*primes)[(*count)++] = 2;

  for(i=3; i<=limit; i+=2) {
    if(!GET_BIT(P_INDEX(i),bitSet)) {
      if(*count >= size)
        *primes = realloc(*primes, (size*=2) * sizeof **primes);
      (*primes)[(*count)++] = i;
    }
  }
  *primes = realloc(*primes, *count * sizeof **primes);
  free(bitSet);
}

uint32_t primeCount(const uint32_t n, const uint32_t * const arr, const uint32_t count) {
  uint64_t low,hi,mid;
  low = 0;
  hi = count;
  while(low < hi) { /* binary search */
    mid = low/2 + hi/2;
    if(arr[mid] == n)
      return  mid+1;
    if(arr[mid] >  n)
      hi  = mid;
    else
      low = mid + 1;
  }
  return low;
}

void print_help(char* str) {
  printf("  Usage: %s <limit>\n",str);
  printf("  Finds the number of semiprimes less then <limit>\n");
  printf("  * limit    : a decimal number exclusive bound\n");
  printf("             : default = 100000000\n");
}
