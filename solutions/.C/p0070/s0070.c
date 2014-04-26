#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  BUFLEN  21 /* Safely holds UINT64_MAX */
#define  DECIMAL 10

/**
 * NOTES:
 * DON'T Brute force the search space
 * Know that to minimize n/phi(n), n must be prime
 * Know that n & phi(n) are not permutations when n is prime
 * Know that semi-primes are the next best minimization
 * Seive Primes up to limit first
 * Then generate semi-primes up to limit
 * Use an O(n) isPermutation method
 */

void     gatherPrimes (const uint32_t limit, uint64_t ** const arr, uint32_t * const count);
void     getSemiPrimes(const uint32_t limit, uint64_t ** const arr, uint32_t * const count, const uint64_t * const primes, const uint32_t pCount);
uint64_t phi          (const uint64_t n, const uint64_t * const primes, const uint32_t count);
uint64_t gcd_bin      (const uint64_t u, const uint64_t v);
uint32_t inList       (const uint64_t n, const uint64_t * const primes, const uint32_t  count);
uint32_t isPermutation(const uint64_t a, const uint64_t b);
void     print_help   (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit = 10000000; /* default */
  uint32_t pCount,spCount;
  uint64_t i,m,n,x,y;
  uint64_t *primes = NULL;
  uint64_t *semiPrimes = NULL;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]), EXIT_SUCCESS; }
  if(argc > 1) {  sscanf(argv[1],"%u",&limit); }

  x=UINT64_MAX;
  y=1;
  pCount=spCount=0;

  gatherPrimes( limit,&primes,&pCount);
  getSemiPrimes(limit,&semiPrimes,&spCount,primes,pCount);

  for(i=0; i<spCount; ++i) {
    m = semiPrimes[i];
    n = phi(m,primes,pCount);
    if(isPermutation(m,n))
      if(m*y < n*x) /* m/n < x/y */
        x=m, y=n;
  }

  printf("%llu/%llu\n",x,y);
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

void getSemiPrimes(const uint32_t limit, uint64_t ** const arr, uint32_t * const count, const uint64_t * const primes, const uint32_t pCount) {
  uint32_t i,j,size,prod;

  size = 10;
  if(*arr!=NULL)
    free(*arr);
  *arr = malloc(size * sizeof **arr);
  *count = 0;

  for(i=0; i<pCount; ++i) {
    for(j=i; j<pCount && (prod=primes[i]*primes[j]) <= limit; ++j) {
      if(*count >= size)
        *arr = realloc(*arr, (size*=2) * sizeof **arr);
      (*arr)[(*count)++] = prod;
    }
  }
  *arr = realloc(*arr, *count * sizeof **arr);
}

uint64_t phi(const uint64_t n, const uint64_t * const primes, const uint32_t count) {
  uint64_t m,o,i,d,bound;
  // Base case
  if(n < 2)
    return 0;
  // Is Prime? (Lehmer's conjecture)
  if(inList(n,primes,count))
    return n-1;
  // Even number?
  if(!(n & 1)) {
    int m = n >> 1;
    return !(m & 1) ? phi(m,primes,count)<<1 : phi(m,primes,count);
  }
  // Find (smallest) prime factor using list of primes
  for(i=0,bound=(uint64_t)sqrt(n)+1; i<count && primes[i]<=bound && (n%primes[i]!=0); ++i);
  m = primes[i];
  o = n/m;
  d = gcd_bin(m, o);
  /* Use phi()'s multiplicative property recursively */
  /*  MEMOTIZATION HERE ? */
  return d==1 ?  phi(m,primes,count)*phi(o,primes,count)
              : (phi(m,primes,count)*phi(o,primes,count)*d)/phi(d,primes,count);
}

uint32_t inList(const uint64_t n, const uint64_t * const primes, const uint32_t count) {
  uint64_t low,hi,mid;
  low = 0;
  hi = count;
  while(low < hi) {    /* binary search */
    mid = low/2 + hi/2;
    if(primes[mid] == n)
      return  1;
    if(primes[mid] >  n)
      hi  = mid;
    else
      low = mid + 1;
  }
  return 0;
}

uint64_t gcd_bin(const uint64_t u, const uint64_t v) {
  /* simple cases (termination) */
  if(u == v)  return u;
  if(u == 0)  return v;
  if(v == 0)  return u;
  /* look for even numbers  */
  if( ~u & 1) {
    if(v & 1) return gcd_bin(u >> 1, v);           /* u is even, v is odd  */
    else      return gcd_bin(u >> 1, v >> 1) << 1; /* u is even, v is even */
  }
  if( ~v & 1) return gcd_bin(u, v >> 1);           /* u is odd,  v is even */
  /* reduce larger argument */                     /* u is odd,  v is odd  */
  return (u > v) ? gcd_bin((u - v) >> 1, v)
                 : gcd_bin((v - u) >> 1, u);
}

uint32_t isPermutation(const uint64_t a, const uint64_t b) {
  static uint8_t * aCount = NULL;
  static uint8_t * bCount = NULL;
  static char    * aStr   = NULL;
  static char    * bStr   = NULL;
  uint32_t i;

  if(aCount==NULL)
     aCount = malloc(DECIMAL * sizeof *aCount);
  if(bCount==NULL)
     bCount = malloc(DECIMAL * sizeof *bCount);
  if(aStr  ==NULL)
     aStr   = malloc(BUFLEN  * sizeof *aStr);
  if(bStr  ==NULL)
     bStr   = malloc(BUFLEN  * sizeof *bStr);

  snprintf(aStr,BUFLEN,"%llu",a);
  snprintf(bStr,BUFLEN,"%llu",b);
  memset(aCount,0,DECIMAL);
  memset(bCount,0,DECIMAL);

  for(i=0; aStr[i]; ++i)
    ++(aCount[aStr[i]-'0']);

  for(i=0; bStr[i]; ++i)
    ++(bCount[bStr[i]-'0']);

  for(i=0; i<DECIMAL; ++i)
    if(aCount[i] != bCount[i])
      return 0;

  return 1;
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the minimum value of n / phi(n)  for 0 < n <= <limit>\n");
  printf("  where phi(n) is a permutation of n\n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 10000000\n");
}
