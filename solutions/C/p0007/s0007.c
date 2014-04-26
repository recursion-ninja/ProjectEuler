#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t overEstimateBound(const uint32_t target);
uint32_t primesBelowBound(const uint32_t x);
void     sieveOfEratosthenes_m2(const uint32_t limit, uint32_t ** const primes, uint32_t * const count);
void     print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t target  = 10001; /* default */
  uint32_t count   = 0;
  uint32_t *primes = NULL;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&target);

  sieveOfEratosthenes_m2(overEstimateBound(target),&primes,&count);

  printf("%u\n",primes[target-1]);
  free(primes);
  return EXIT_SUCCESS;
}

uint32_t overEstimateBound(const uint32_t target) {
  uint32_t hi,low,mid,tmp;
  for(hi=1; primesBelowBound(hi) < target; hi*=2);
  low = hi/2;
  while(low < hi) { /* binary search */
    mid = low/2 + hi/2;
    tmp = primesBelowBound(mid);
    if(tmp == target)
      return  mid+1;
    if(tmp >  target)
      hi  = mid;
    else
      low = mid + 1;
  }
  return low+1;
}

uint32_t primesBelowBound(const uint32_t x) {
  return (uint32_t)(((double)x)/log(x));
}

/* Less Then Or Equal */
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

void print_help(const char * const str) {
  printf("  Usage: %s <index> \n",str);
  printf("  Calculates the value of the ith prime number\n");
  printf("  where i is equal to index\n");
  printf("  * index : a decimal number\n");
  printf("          : default = 10001\n");
}

