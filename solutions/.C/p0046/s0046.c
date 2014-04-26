#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  START_SIZE 10
/**
 * Brtue force te search space
 * Precompute primes  in space
 * Precompute squares in space
 * Use binary search to check primality & squaredness
 */
void     gatherPrimes( const uint32_t limit, uint32_t ** const primes,  uint32_t * const count, uint32_t * const size);
void     gatherSquares(const uint32_t limit, uint32_t ** const squares, uint32_t * const count, uint32_t * const size);
uint32_t isPrime( const uint32_t n, const uint32_t * const primes , const uint32_t count);
uint32_t isSquare(const uint32_t n, const uint32_t * const squares, const uint32_t count);
uint32_t binarySearch(const uint32_t n, const uint32_t * const arr, const uint32_t count);
uint32_t print_help(char* str);

int main(int argc, char* argv[]) {
  uint32_t i,j,found,counterExample;
  uint32_t lower    = 9;      /* default */
  uint32_t upper    = 10000;  /* default */
  uint32_t pSize    = START_SIZE ;
  uint32_t sSize    = START_SIZE ;
  uint32_t pCount   = 0;
  uint32_t sCount   = 0;
  uint32_t *primes  = malloc(pSize * sizeof *primes);
  uint32_t *squares = malloc(sSize * sizeof *squares);

  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&lower); }
  if(argc > 2) {  sscanf(argv[1],"%u",&upper); }

  gatherPrimes( upper,&primes ,&pCount,&pSize);
  gatherSquares(upper,&squares,&sCount,&sSize);
  counterExample = 0;

  for(i=lower; i<=upper && !counterExample; i+=2) {
    if(!isPrime(i,primes,pCount)) {
      for(j=found=0; !found && j<pCount && primes[j]<i; ++j)
        if(isSquare((i-primes[j])/2,squares,sSize))
          found=1;
      if(!found)
        counterExample = i;
    }
  }
  printf("Min Counter Example: %u\n",counterExample);
  return 0;
}

uint32_t isSquare(const uint32_t n, const uint32_t * const squares, const uint32_t count) {
  return binarySearch(n,squares,count);
}


uint32_t isPrime( const uint32_t n, const uint32_t * const primes , const uint32_t count) {
  return binarySearch(n,primes,count);
}

uint32_t binarySearch(const uint32_t n, const uint32_t * const arr, const uint32_t count) {
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

void gatherSquares(const uint32_t limit, uint32_t ** const squares, uint32_t * const count, uint32_t * const size) {
  uint32_t i,sq;
  for(i=1; (sq=i*i)<limit; ++i) {
    if(*count >= *size)
      *squares = realloc(*squares, (*size*=2) * sizeof **squares);
    (*squares)[(*count)++] = sq;
  }
}


uint32_t print_help(char* str) {
  printf("  Usage: %s <lower> <upper> \n",str);
  printf("  Searches the range [<lower>, <upper>] for the minimized counter example \n");
  printf("  to Goldbach's less famous conjecture: \n");
  printf("  \"Every odd composite number is the sum of a prime and twice a square\"\n");
  printf("  * lower : a decimal number inclusive bound\n");
  printf("          : default = 9\n");
  printf("  * upper : a decimal number inclusive bound\n");
  printf("          : default = 10000\n");
  return 0;
}
