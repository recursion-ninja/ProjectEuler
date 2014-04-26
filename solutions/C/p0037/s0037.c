#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * NOTES:
 * Pre-sieve primes
 * Use logs to inteligently avoid stringification
 */

void     gatherPrimes(const uint32_t limit, uint64_t ** const arr, uint32_t * const count);
int32_t  indexOf(const uint64_t n, const uint64_t * const arr, const uint32_t  count);
int      isSingleDigit(const uint64_t p);
int      isLeftAndRightTruncateablePrime(const uint64_t i, const uint64_t const * const primes, const uint32_t count);
int      isLeftTruncateablePrime (const uint64_t i, const uint64_t const * const primes, const uint32_t count);
int      isRightTruncateablePrime(const uint64_t i, const uint64_t const * const primes, const uint32_t count);
uint64_t truncateLeft (const uint64_t x);
uint64_t truncateRight(const uint64_t x);
void     print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit = 740000; /* default */
  uint32_t i,count;
  uint64_t sum;
  uint64_t *primes = NULL;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]), EXIT_SUCCESS; }
  if(argc > 1) {  sscanf(argv[1],"%u",&limit); }

  gatherPrimes(limit,&primes,&count);

  for(i=sum=0; i<count; ++i)
    if(isLeftAndRightTruncateablePrime(i,primes,count))
      sum+=primes[i];

  printf("%llu\n",sum);
  return EXIT_SUCCESS;
}

void gatherPrimes(const uint32_t limit, uint64_t ** const arr, uint32_t * const count) {
  uint32_t  i,j,size;
  uint8_t  *isPrime;

  size = 10;
  if(*arr!=NULL)
    free(*arr);
  *arr = malloc(size * sizeof **arr);
  *count = 0;

  isPrime = malloc((limit+1) * sizeof *isPrime);
  memset(isPrime,1,limit+1);

  for(i=2; i<=limit; ++i)
    if(isPrime[i])
      for(j=2*i; j<=limit; j+=i)
        isPrime[j] = 0;

  for(i=2; i<=limit; ++i) {
    if(isPrime[i]) {
      if(*count >= size)
        *arr = realloc(*arr, (size*=2) * sizeof **arr);
      (*arr)[(*count)++] = i;
    }
  }
  *arr = realloc(*arr, *count * sizeof **arr);
}

int32_t indexOf(const uint64_t n, const uint64_t * const primes, const uint32_t count) {
  uint64_t low,hi,mid;
  low = 0;
  hi = count;
  while(low < hi) {    /* binary search */
    mid = low/2 + hi/2;
    if(primes[mid] == n)
      return  mid;
    if(primes[mid] >  n)
      hi  = mid;
    else
      low = mid + 1;
  }
  return -1;
}

int isSingleDigit(const uint64_t p) {
  return p < 10;
}

int isLeftAndRightTruncateablePrime(const uint64_t i, const uint64_t const * const primes, const uint32_t count) {
  return !isSingleDigit(primes[i])
       && isLeftTruncateablePrime(i,primes,count)
       && isRightTruncateablePrime(i,primes,count);
}

int isLeftTruncateablePrime(const uint64_t i, const uint64_t const * const primes, const uint32_t count) {
  uint64_t n;
   int32_t x;
  if(isSingleDigit(primes[i]))
    return 1;

  n = truncateLeft(primes[i]);
  x = indexOf(n,primes,count);
  return x >= 0 && isLeftTruncateablePrime(x, primes, count);
}

int isRightTruncateablePrime(const uint64_t i, const uint64_t const * const primes, const uint32_t count) {
  uint64_t n;
   int32_t x;
  if(isSingleDigit(primes[i]))
    return 1;

  n = truncateRight(primes[i]);
  x = indexOf(n,primes,count);
  return x >= 0 && isRightTruncateablePrime(x, primes, count);
}

uint64_t truncateLeft(const uint64_t x) {
  uint8_t  digits  = ((uint8_t) floor(log10(x))) + 1;
  uint64_t divisor = (uint64_t) pow(10,digits-1);
  return x % divisor;
}

uint64_t truncateRight(const uint64_t x) {
  return x / 10;
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the sum of all left & right truncateable primes less then <limit>\n");
  printf("  Note only eleven such primes exist and they are all below the default limit\n");
  printf("  * limit    : a decimal number exclusive bound\n");
  printf("             : default = 740000\n");
}
