#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  BITS_IN_BYTE   8
#define  FACTORIAL_MAX 18

/**
 * NOTES:
 * Just applied probability
 * Make sure to use memotization for fctorial calculations!
 * Use bitstring permutation to efficinetly calculate subsets!
 */

uint32_t validNumberOfRounds(const uint32_t rounds);
uint32_t getFirstPermutation(const uint32_t setBits);
uint32_t getNextPermutation (const uint32_t v);
uint64_t getPermutationValue(const uint32_t perm);
uint64_t factorial          (const uint32_t x);
uint64_t gcd_bin            (const uint64_t u, const uint64_t v);
void     reduceFraction     (uint64_t * const numerator, uint64_t * const denominator);
void     invertFraction     (uint64_t * const numerator, uint64_t * const denominator);
void     print_help         (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t rounds = 15; /* default */
  uint64_t i,j,limit,answer,numerator,denominator;
  uint32_t maxFails,perm;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&rounds);

  if(!validNumberOfRounds(rounds))
    return print_help(argv[0]), EXIT_FAILURE;

  numerator   = 0;
  denominator = factorial(rounds+1);
  maxFails    = rounds/2 - ((rounds&1) ? 0 : 1);

  for(i=0; i<=maxFails; ++i) {
    limit = factorial(rounds) / (factorial(rounds-i)*factorial(i));
    perm  = getFirstPermutation(i);
    for(j=0; j<limit; ++j,perm=getNextPermutation(perm))
      numerator += getPermutationValue(perm);
  }

  reduceFraction(&numerator,&denominator);
  answer = denominator / numerator;

  printf("Win %%: %llu/%llu\nPay $: %llu\n",numerator,denominator,answer);
  return EXIT_SUCCESS;
}

uint32_t validNumberOfRounds(const uint32_t rounds) {
  return 0 < rounds && rounds <= FACTORIAL_MAX;
}

/* setBits = 3  looks like 0x00000007 */
/* setBits = 14 looks like 0x00003FFF */
uint32_t getFirstPermutation(const uint32_t setBits) {
  if(setBits > sizeof setBits * BITS_IN_BYTE)
    return ~0;
  return (setBits <= 1) ? setBits : ((getFirstPermutation(setBits-1) << 1) | 1);
}

uint32_t getNextPermutation(const uint32_t v) {
  uint32_t t = (v | (v - 1)) + 1;
  return v==0 ? 0 :  t | ((((t & -t) / (v & -v)) >> 1) - 1);
}

uint64_t getPermutationValue(const uint32_t perm) {
  static uint32_t length = sizeof perm * BITS_IN_BYTE;
  uint32_t i,mask;
  uint64_t result;

  for(i=0,result=mask=1; i<length; ++i,mask<<=1)
    if(mask & perm)
      result  *= (i+1);

  return result;
}

uint64_t factorial(const uint32_t x) {
  static uint64_t *memo = NULL;

  if(memo==NULL)
    memo = calloc(FACTORIAL_MAX, sizeof *memo);

  if(x > FACTORIAL_MAX)
    return factorial(FACTORIAL_MAX);

  if(x < 2)
    return 1;

  if(memo[x]==0)
    memo[x] = x*factorial(x-1);

  return memo[x];
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

void reduceFraction(uint64_t * const numerator, uint64_t * const denominator) {
  uint64_t divisor = gcd_bin(*numerator,*denominator);
  *numerator   /= divisor;
  *denominator /= divisor;
}

void invertFraction(uint64_t * const numerator, uint64_t * const denominator) {
  uint64_t temporary;
   temporary   = *numerator;
  *numerator   = *denominator;
  *denominator =  temporary;
}

void print_help(const char * const str) {
  printf("  Usage: %s <turnss> \n",str);
  printf("  Calculate the probability that the player will win,\n");
  printf("  and what the house should pay out the folloing game of chance:\n\n");
  printf("  A certian game of chance consists of <turns> rounds. \n");
  printf("  On each round the player picks from a bag of blue & red disks,\n");
  printf("  notes the disk's color, and return the disk to the bag.\n");
  printf("  During the first round the bag contains one red and one blue disk.\n");
  printf("  On each subsequent round a single red disk is added to the bag.\n");
  printf("  The player wins if after all rounds he/she has picked more blue then red disks\n");
  printf("  * turns : a decimal number [1,18]\n");
  printf("          : default = 15\n");
}
