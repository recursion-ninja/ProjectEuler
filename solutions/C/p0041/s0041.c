#include <gmp.h>
#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  MAX_DIGITS     9
#define  MAX_FACTORIAL 18
/**
 * Permute through all pandigital numbers of length n, n going from 9-2
 * Use Miller-Rabin primality testing on each pandigital number.
 * Break and print first positive result.
 * Runs in \sum\limits_{n=2}^{9}n! = 409112
 */
uint32_t getPermutation(uint32_t index,uint8_t digits);
uint64_t factorial(const uint32_t x);
int      isPrime(const uint32_t n);
int      print_help(char* str);

int main(int argc, char* argv[]) {
  int found,index;
  uint8_t digits;
  found=index=0;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }

  for(digits=MAX_DIGITS; !found && digits>1; --digits)
    for(index=factorial(digits)-1; !found && index >=0; --index)
      if(isPrime(getPermutation((uint32_t)index,digits)))
        found = 1;

  printf("Max pandigital prime: %u\n",getPermutation(++index,++digits));

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

uint32_t getPermutation(uint32_t index,uint8_t digits) {
  static uint8_t elem[MAX_DIGITS] = {1,2,3,4,5,6,7,8,9};
  uint32_t sum,i;
  uint32_t fact = factorial(digits-1);
  uint32_t mul  = (uint32_t)pow(10,digits-1);

  for(i=0; i<MAX_DIGITS; ++i)
    elem[i] = i+1;

  for(sum=0; digits;) {
    i      = fact ? index/fact : 0;
    sum   += elem[i] * mul;
    mul   /= 10;
    index %= fact;
    fact   = --digits ? fact / digits : 0;
    while(++i<MAX_DIGITS)
      elem[i-1] = elem[i];
  }
  return sum;
}

int isPrime(const uint32_t n) {
  int primality;
  mpz_t x;
  mpz_init(x);
  mpz_set_ui(x,n);
  primality = mpz_probab_prime_p(x,10);
  mpz_clear(x);
  return primality;
}

int print_help(char* str) {
  printf("  Usage: %s \n",str);
  printf("  Finds the maximum N-pandigital prime number\n");
  return 0;
}
