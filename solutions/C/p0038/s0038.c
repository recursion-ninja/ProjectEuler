#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  BASE          10
#define  DIGITS        (BASE-1)
#define  MAX_FACTORIAL 18

/**
 * Permute through all pandigital numbers of length 9
 * Use logrithms to avoid stringification.
 * Break and print first positive result.
 */

uint64_t factorial(const uint32_t x);
uint32_t getDigits(const uint32_t n);
uint32_t getPermutation(uint32_t index,uint8_t digits);
uint32_t getPandigitalMultiple(const uint32_t n);
int      pandigitalMultiple(const uint32_t x, const uint32_t n, const uint32_t m);
int      print_help(char* str);

int main(int argc, char* argv[]) {
  int found,index;
  found=index=0;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }

  for(index=factorial(DIGITS)-1; !found && index >=0; --index)
    if(getPandigitalMultiple(getPermutation((uint32_t)index,DIGITS)) > 1)
      found = index;

  printf("%u\n",getPermutation(found,DIGITS));

  return EXIT_SUCCESS;
}

uint64_t factorial(const uint32_t x) {
  static uint64_t memo[MAX_FACTORIAL];
  static int first = 1;

  if(first) {
    memset(memo,0,MAX_FACTORIAL);
    first=0;
  }

  if(x > MAX_FACTORIAL)
    return factorial(MAX_FACTORIAL);

  if(x < 2)
    return 1;

  if(memo[x]==0)
    memo[x] = x*factorial(x-1);

  return memo[x];
}

uint32_t getDigits(const uint32_t n) {
  return 1+ (uint32_t) floor(log10(n));
}

uint32_t getPermutation(uint32_t index,uint8_t digits) {
  static uint8_t elem[DIGITS] = {1,2,3,4,5,6,7,8,9};
  uint32_t sum,i;
  uint32_t fact = factorial(digits-1);
  uint32_t mul  = (uint32_t)pow(10,digits-1);

  for(i=0; i<DIGITS; ++i)
    elem[i] = i+1;

  for(sum=0; digits;) {
    i      = fact ? index/fact : 0;
    sum   += elem[i] * mul;
    mul   /= 10;
    index %= fact;
    fact   = --digits ? fact / digits : 0;
    while(++i<DIGITS)
      elem[i-1] = elem[i];
  }
  return sum;
}

uint32_t getPandigitalMultiple(const uint32_t n) {
  uint32_t divisor,q,r;

  for(divisor = (uint32_t) pow(BASE,getDigits(n)-1); divisor > 1; divisor/=BASE) {
    q = n / divisor;
    r = n % divisor;
    if(pandigitalMultiple(q,r,2))
       return q;
  }
  return 1;
}

int pandigitalMultiple(const uint32_t x, const uint32_t n, const uint32_t m) {
  uint32_t product,magnitude,quocient,remainder;

  product = x*m;

  if(n == product)
    return !!1;
  if(n  < product)
    return 0;

  magnitude = (uint32_t)pow(BASE,getDigits(n) - getDigits(product));
  quocient  = n / magnitude;
  remainder = n % magnitude;

  return (quocient==product) && pandigitalMultiple(x, remainder, m+1);
}

int print_help(char* str) {
  printf("  Usage: %s \n",str);
  printf("  Finds the maximum pandigital number which can be written as the\n");
  printf("  concatenated product of an integer with [1,2,..,n] where n > 1\n");
  return 0;
}
