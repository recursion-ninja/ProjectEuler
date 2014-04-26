#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
/**
 * REMEMBER:
 * Problem is equivilent to:
 * Summation(i=2,limit=1000000) { phi(i) }
 */
uint64_t gcd_bin   (const uint64_t u, const uint64_t v);
uint64_t phi       (const uint64_t n, uint64_t ** const primes, uint64_t * const count, uint64_t * const size);
uint32_t isPrime   (const uint64_t n, uint64_t ** const primes, uint64_t * const count, uint64_t * const size);
void     addPrime  (const uint64_t n, uint64_t ** const primes, uint64_t * const count, uint64_t * const size);
uint32_t isInArr   (const uint64_t n, uint64_t ** const primes, const uint64_t   count);
void     expand_arr(uint64_t ** const arr, uint64_t * const size);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t limit=1000000;   /* default */
  uint64_t count=0,size=10; /* default */
  uint64_t i;
  uint64_t *primes = malloc(size * sizeof(uint64_t));
  uint64_t sum;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%llu",&limit); }

  for(i=2,sum=0,count=0; i<=limit; ++i)
    sum += phi(i,&primes,&count,&size);
  printf("%llu\n",sum);
  return 0;
}

uint64_t phi(const uint64_t n, uint64_t ** const primes, uint64_t * const count, uint64_t * const size) {
  uint64_t m,o,i,d,bound;
  /* Base case */
  if(n < 2)
    return 0;
  /* Is Prime? (Lehmer's conjecture) */
  if(isPrime(n,primes,count,size))
    return n-1;
  /* Even number? */
  if(!(n & 1)) {
    int m = n >> 1;
    return !(m & 1) ? phi(m,primes,count,size)<<1 : phi(m,primes,count,size);
  }
  /* Find (smallest) prime factor using list of primes */
  for(i=0,bound=(uint64_t)sqrt(n)+1; i<*count && (*primes)[i]<=bound && (n%(*primes)[i])!=0; ++i);
  m = (*primes)[i];
  o = n/m;
  d = gcd_bin(m, o);
  /* Use phi()'s multiplicative property recursively */
  return d==1 ?  phi(m,primes,count,size)*phi(o,primes,count,size)
              : (phi(m,primes,count,size)*phi(o,primes,count,size)*d)/phi(d,primes,count,size);
}

uint32_t isPrime(const uint64_t n, uint64_t ** const primes, uint64_t * const count, uint64_t * const size) {
  uint64_t i,bound;
  uint32_t prime;
  for(i=0,prime=1,bound=(uint64_t)sqrt(n)+1; prime && i<*count && (*primes)[i]<=bound; ++i)
    prime = n%(*primes)[i];
  if(prime)
    addPrime(n,primes,count,size);
  return prime || n==2; /* catches false negative */
}

void addPrime(const uint64_t n, uint64_t ** const primes, uint64_t * const count, uint64_t * const size) {
  if(*count >= *size)
    expand_arr(primes,size);
  if(!isInArr(n,primes,*count))
    (*primes)[(*count)++] = n;
}

void expand_arr(uint64_t ** const primes, uint64_t * const size) {
  *size  *= 2;
  *primes = (uint64_t*) realloc(*primes, *size * sizeof(uint64_t));
}

uint32_t isInArr(const uint64_t n, uint64_t ** const primes, const uint64_t count) {
  uint64_t hi,low,mid,val;
  low = 0; hi = count; /* set bounds    */
  while(low < hi) {    /* binary search */
    mid = low/2 + hi/2;
    val = (*primes)[mid];
    if(val == n) return  1;
    if(val >  n) hi  = mid;
    if(val <  n) low = mid+1;
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

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the number of proper reduced fractions \n");
  printf("  whose denominator is <= <limit> \n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 1000000\n");
  return 0;
}
