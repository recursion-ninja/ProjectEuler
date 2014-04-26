#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  START_SIZE 10

/**
 * Precompute primes in space
 * Precompute prime upper bound knowing:
 * Any prime p > limit/2 will not be part of the solution
 * Start search at highest prime less then the bound
 * Use binary search to check primality
 * Use short cuircuit logic to end search as soon as possible
 */
void     gatherPrimes(const uint32_t limit, uint32_t ** const primes,  uint32_t * const count, uint32_t * const size);
uint32_t getMaximumPrimeIndex(const uint32_t limit, const uint32_t * const primes, const uint32_t count);
uint32_t inList(const uint32_t n, const uint32_t * const arr, const uint32_t count);
uint32_t print_help(char* str);

int main(int argc, char* argv[]) {
  uint32_t i,j,x,sum,bound,numMax,lenMax;
  uint32_t limit    = 1000000;  /* default */
  uint32_t size     = START_SIZE ;
  uint32_t count    = 0;
  uint32_t *primes;

  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&limit); }

  lenMax = numMax = 1;
  primes = malloc(size * sizeof *primes);
  gatherPrimes(limit,&primes,&count,&size);
  bound  = getMaximumPrimeIndex(limit,primes,count);

  for(i=bound; i>lenMax-1; --i)  { /* for each prime under bound */
    for(j=i,sum=0; j<count; --j) { /* try adding consecutive primes */
      sum += primes[j];
      if( (x=i-j) > lenMax && inList(sum,primes,count) )
        lenMax = x, numMax = sum;
      if(sum>=limit)
        break;
    }
  }

  printf("%u\n",numMax);
  return EXIT_SUCCESS;
}

void gatherPrimes(const uint32_t limit, uint32_t ** const primes, uint32_t * const count, uint32_t * const size) {
  uint32_t i,j;
  uint8_t *isComposite = calloc(limit+1, sizeof *isComposite);

  for(i=2; i<=limit; ++i)
    if(!isComposite[i])
      for(j=i*2; j<=limit; j+=i)
        isComposite[j] = 1;

  for(i=2; i<=limit; ++i) {
    if(!isComposite[i]) {
      if(*count >= *size)
        *primes = realloc(*primes, (*size*=2) * sizeof **primes);
      (*primes)[(*count)++] = i;
    }
  }
  free(isComposite);
}

uint32_t getMaximumPrimeIndex(const uint32_t limit, const uint32_t * const primes, const uint32_t count) {
  uint32_t hi,mid,low,target;
  target = limit/2;
  hi  = count; /* exclusive bound */
  low = 0;
  while(low < hi) {
    mid = low/2 + hi/2;
    if(primes[mid] <= target && primes[mid+1] > target)
      return mid;
    if(primes[mid]  > target)
     hi  = mid;
    else
     low = mid + 1;
  }
  return primes[0] < primes[count-1] ? primes[0] : primes[count-1];
}


uint32_t inList(const uint32_t n, const uint32_t * const arr, const uint32_t count) {
  uint32_t hi,mid,low;
  hi  = count; /* exclusive bound */
  low = 0;
  while(low < hi) {
    mid = low/2 + hi/2;
    if(arr[mid] == n)
      return 1;
    if(arr[mid]  > n)
     hi  = mid;
    else
     low = mid + 1;
  }
  return 0;
}

uint32_t print_help(char* str) {
  printf("  Usage: %s <limit>\n",str);
  printf("  Searches the prime number below low <limit> \n");
  printf("  That can be expressed as the longest sum of consecutive primes \n");
  printf("  * limit : a decimal number inclusive bound\n");
  printf("          : default = 1000000\n");
  return 0;
}
