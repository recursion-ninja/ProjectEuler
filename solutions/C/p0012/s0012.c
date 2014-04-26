#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
/**
 * REMEMBER:
 * if prime factors of n = a^x * b^y * c^z
 * TotalDivisors(n) = (x+1)*(y+1)*(z+1)
 */
uint64_t getTriangleNumber(const uint32_t x);
uint32_t countDivisors(const uint64_t n, uint64_t *primes, uint64_t *exp, uint32_t *size);
void     factor(uint64_t n, uint64_t *primes, uint64_t *exp, uint32_t *size, uint32_t *factors);
void     addFactor(const uint64_t n, const uint64_t e, uint64_t * primes, uint64_t * exp, uint32_t * size, uint32_t * factors);
void     expand(uint64_t ** const arr1, uint64_t ** const arr2, uint32_t * size);
void     print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t target = 500;
  uint32_t i,size=10;
  uint64_t *primes = malloc(size * sizeof *primes);
  uint64_t *exp    = malloc(size * sizeof *exp   );

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&target);

  for(i=1; countDivisors(getTriangleNumber(i),primes,exp,&size)<=target; ++i);
  printf("%llu\n",getTriangleNumber(i));

  free(primes);
  free(exp);
  return EXIT_SUCCESS;
}

uint64_t getTriangleNumber(const uint32_t x) {
  uint64_t n = x;
  return (n*(n+1))/2;
}

uint32_t countDivisors(const uint64_t n, uint64_t *primes, uint64_t *exp, uint32_t *size) {
  uint32_t i,factors = 0;
  uint32_t divs = 1;
  factor(n, primes, exp, size, &factors);
  for(i=0; i<factors; ++i)
    divs *= (exp[i])+1;
  return divs;
}

void factor(uint64_t n, uint64_t *primes, uint64_t *exp, uint32_t *size, uint32_t *factors) {
  uint32_t i,e,found,toggle,bound=(uint32_t)floor(sqrt(n))+1;
  for(i=2,e=0,found=0; n > 1 && !(n%i); n/=i,e++,found=1);
  if(found)
    addFactor(i,e,primes,exp,size,factors);
  for(i=3,e=0,found=0; n > 1 && !(n%i); n/=i,e++,found=1);
  if(found)
    addFactor(i,e,primes,exp,size,factors);
  for(i=5,toggle=0; n > 1 && i < bound; i+=toggle?4:2) {
    for(e=0,found=0; !(n%i); n/=i,e++,found=1);
    if(found)
      addFactor(i,e,primes,exp,size,factors);
  }
  if(n > 1) /* is prime */
    addFactor(n,1,primes,exp,size,factors);
}

void addFactor(const uint64_t n, const uint64_t e, uint64_t * primes, uint64_t * exp, uint32_t * size, uint32_t * factors) {
  if(*factors==*size)
    expand(&primes,&exp,size);
  primes[(*factors)  ] = n;
  exp[   (*factors)++] = e;
}

void expand(uint64_t ** const arr1, uint64_t ** const arr2, uint32_t * size) {
  *size *= 2;
  *arr1 = realloc(*arr1, *size * sizeof **arr1);
  *arr2 = realloc(*arr2, *size * sizeof **arr2);
}

void print_help(const char * const str) {
  printf("  Usage: %s <divisors> \n",str);
  printf("  Calculates the first triangle number\n");
  printf("  which is divisable by <divisors> numbers \n");
  printf("  * divisors : a decimal number\n");
  printf("            : default = 500\n");
}
