#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef enum {DEFICIENT,PERFECT,ABUNDANT} divisorCount_t;

void     sieveOfEratosthenes_m2(const uint32_t limit, uint32_t ** const primes, uint32_t * const count);
void     factor(const uint32_t n, uint32_t *** const factors, uint32_t * const count, const uint32_t * const primes, const uint32_t pCount);
void     addFactor(const uint32_t f, uint32_t e, uint32_t *** const factors, uint32_t * const count, uint32_t * const size);
void     freeFactors(uint32_t *** const factors, uint32_t * const count);
int      inList(const uint32_t n, const uint32_t * const primes, const uint32_t count);
uint32_t divisorSum(uint32_t ** factors, const uint32_t count);
void     print_help(const char * const str);

/**
 * If the prime factors on a number N are A^a * b^b * c^c
 * Then the sum of N's divisor's are (A^0+...+A^a)(B^0+...+B^b)(C^0+...+C^c)
 * And  the sum of N's PROPER divisor's are (A^0+...+A^a)(B^0+...+B^b)(C^0+...+C^c) - N
 * Example:
 * Prime Factors: 120 = 2^3 * 3^1 * 5^1
 * Propper Divisor Sum: (1+2+2^2+2^3)(1+3)(1+5)-120 = 15⋅4⋅6-120 = 240
 */

int main(int argc, char* argv[]) {
  uint32_t limit = 28123; /* Default */
  uint32_t i,j,total,pCount,fCount,properDivisors;
  uint32_t *primes=NULL,**factors=NULL;
  uint8_t  *isAbundantSum=NULL;
  divisorCount_t *divCount=NULL;

  if(argc > 1 &&!strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&limit);

  isAbundantSum = calloc(limit, sizeof *isAbundantSum);
  divCount      = calloc(limit, sizeof *divCount);
  divCount[1]   = 0;

  sieveOfEratosthenes_m2(limit,&primes,&pCount);

  for(i=2; i<limit; ++i) {
    factor(i,&factors,&fCount,primes,pCount);
    properDivisors = divisorSum(factors,fCount) - i;
    divCount[i] = properDivisors <= i
                ? properDivisors != i
                ? DEFICIENT : PERFECT : ABUNDANT;
  }

  for(i=2; i<limit; ++i)
    if(divCount[i]==ABUNDANT)
      for(j=2; j<=i; ++j)
        if(divCount[j]==ABUNDANT && i+j < limit)
          isAbundantSum[i+j] = 1;

  for(i=1,total=0; i<limit; ++i)
    if(!isAbundantSum[i])
      total += i;

  printf("%u\n",total);
  freeFactors(&factors,&fCount);
  free(primes);
  free(isAbundantSum);
  free(divCount);
  return EXIT_SUCCESS;
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

void factor(const uint32_t n, uint32_t *** const factors, uint32_t * const count, const uint32_t * const primes, const uint32_t pCount) {
    uint32_t x,e,i,size;

    if(*factors!=NULL)
      freeFactors(factors,count);

    *factors = malloc((size=10) * sizeof **factors);
    *count   = 0;

    for(i=0,x=n; x > 1 && i < pCount; ++i) {
      if(inList(x,primes,pCount)) {
        addFactor(x,1,factors,count,&size);
        break;
      }
      for(e=0; !(x%primes[i]); ++e)
        x/=primes[i];
      if(e)
        addFactor(primes[i],e,factors,count,&size);
    }
    if(*count==0)
      addFactor(n,1,factors,count,&size);

    *factors = realloc(*factors, *count * sizeof **factors);
}

void addFactor(const uint32_t f, uint32_t e, uint32_t *** const factors, uint32_t * const count, uint32_t * const size) {
  uint32_t *set = malloc( 2 * sizeof *set);
  set[0] = f;
  set[1] = e;
  if(*count>=*size)
    *factors = realloc(*factors, (*size *= 2) * sizeof **factors);
  (*factors)[(*count)++] = set;
}

void freeFactors(uint32_t *** const factors, uint32_t * const count) {
  uint32_t i;
  for(i=0; i<*count; ++i)
    free((*factors)[i]);
  free(*factors);
  *factors = NULL;
  *count   = 0;
}

int inList(const uint32_t n, const uint32_t * const primes, const uint32_t count) {
  uint32_t low,hi,mid;
  low = 0;
  hi = count;
  while(low < hi) { /* binary search */
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

uint32_t divisorSum(uint32_t ** factors, const uint32_t count) {
    uint32_t i,j,divSum,expSum;
    for(i=0,divSum=1; i<count; ++i) {              /* foreach prime factor */
      for(j=0,expSum=0; j<=factors[i][1]; ++j)     /* foreach exp */
         expSum += (uint32_t)pow(factors[i][0],j); /* sum up exponentiations */
      divSum *= expSum;                            /* multiply sums of exponentions */
    }
    return divSum;
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the sum of all positive integers less then <limit> \n");
  printf("  which cannot be written as the sum of two abundant numbers. \n");
  printf("  Number is Deficient iff d(n) < n \n");
  printf("  Number is Abundant  iff d(n) > n \n");
  printf("  Number is Perfect   iff d(n) = n \n");
  printf("  Where d(n) = sum of all propper divisors of n \n");
  printf("  * limit  : a decimal number\n");
  printf("           : default = 28123\n");
}
