#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t isPrime(const int64_t x);
void     expandPrimeListTo(uint64_t ** const primes, uint32_t * const count, uint32_t * const size, uint32_t * const biggest, const uint32_t x);
void     addToPrimeList(uint64_t ** const primes, uint32_t * const count, uint32_t * const size, const uint64_t x);
uint32_t isInOrderedList(const uint64_t * const list, const uint32_t count, const uint64_t x);
uint32_t print_help  (const char * const str);

int main(int argc, char* argv[]) {
   int64_t limit=1000; /* Default */
   int64_t a,b;
   int64_t maxA,maxB;
  uint64_t n;
  uint64_t maxN;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%lld",&limit); }

  maxA = maxB = ~0;
  maxN = 0;
  for(a=-limit+1; a < limit; ++a) {
    for(b=-limit+1; b < limit; ++b) {
      for(n=0; isPrime(n*n + a*n + b); ++n) { ; }
      if(n > maxN) {
        maxN = n;
        maxA = a;
        maxB = b;
      }
    }
  }
  printf("max(n): %llu <=> a: %lld b: %lld\nans(a*b): %lld\n",maxN,maxA,maxB,maxA*maxB);
  return 0;
}

uint32_t isPrime(const int64_t n) {
  static uint64_t *primes  = NULL;
  static uint32_t  count   = 0;
  static uint32_t  size    = 0;
  static uint32_t  biggest = 0;
  uint64_t x;

  /* Make sure prime list storage is initialized */
  if(primes == NULL)
    primes = malloc( (size=2) * sizeof *primes);
  if(count  == 0)
    primes[count++] = biggest = 2;

  x = (n < 0) ? -n : n; /* abs(n) */

  if(x > biggest)
    expandPrimeListTo(&primes,&count,&size,&biggest,x);
  return isInOrderedList(primes,count,x);
}

void expandPrimeListTo(uint64_t ** const primes, uint32_t * const count, uint32_t * const size, uint32_t * const biggest, const uint32_t x) {
  uint64_t i,j;
  uint32_t prime;
  for(i=*biggest+1; i<=x; ++i) {
    for(j=0,prime=1; prime && j<*count; ++j)
      prime = i % (*primes)[j];
    if(prime)
      addToPrimeList(primes,count,size,i);
  }
  *biggest = x;
}

void addToPrimeList(uint64_t ** const primes, uint32_t * const count, uint32_t * const size, const uint64_t x) {
  while(*count >= *size)
    *primes = realloc(*primes, (*size *= 2) * sizeof **primes);
  (*primes)[(*count)++] = x;
}

uint32_t isInOrderedList(const uint64_t * const list, const uint32_t count, const uint64_t x) {
  uint32_t low  = 0;     /* inclusive */
  uint32_t high = count; /* exclusive */
  uint32_t mid;
  while(low < high) {
    mid = low/2 + high/2;
    if(list[mid] == x)
      return 1;
    if(list[mid] >  x)
      high = mid;
    else
      low  = mid + 1;
  }
  return 0;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Finds the value of a & b which maiximize the range [0,n]:\n");
  printf("  Such that X*X + a*X + b is prime for all X in [0,n]\n");
  printf("  |a|, |b| less then <limit> \n");
  printf("  * limit : a decimal number \n");
  printf("          : default = 1000\n");
  return 0;
}
