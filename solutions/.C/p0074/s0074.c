#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * Obviously memotization is an efficientcy requirement due to optimal sub-problem structure.
 * Use a memotization table to stroe previously computed chain lengths
 * However, do not use linked list to store chain linkage.
 * It maximizes cache misses and consequently does not scale well
 * Use a memotization array for chain linkage ie: arr[j] == j->next
 * CONCLUSION: use 2 memotization arrays, one for values and one for chain linkage
 */
#define  UNSET    0 /* <-- don't change value, not sure why code is value dependant */
#define  BUF_LEN 11 /* Safely holds UINT32_MAX */

/* precompute factorial values */
static const uint32_t factorial[] = { 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880 };
/* global memotization tables */
static uint32_t *memo_data;
static uint32_t *memo_next;

void     initializeMemotizationTable(const uint32_t x);
uint32_t getMaxFactorialChainValue(const uint32_t x);
uint32_t isInList(const uint32_t cVal, const uint32_t tVal);
uint32_t setChainLengths(const uint32_t cVal, const uint32_t tVal);
uint32_t setChainLengths_cycle(const uint32_t cVal, const uint32_t tVal, const uint32_t x);
uint32_t digitFactorial(const uint32_t x);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit  = 1000000; /* default */
  uint32_t target = 60;      /* default */
  uint32_t i,found,cycle,startVal,prevVal,currVal;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&limit ); }
  if(argc > 2) {  sscanf(argv[2],"%u",&target); }

  initializeMemotizationTable(limit);

  for(i=found=0; i<limit; ++i) {
    if(memo_data[i] == UNSET) { /* not yet calculated */
      startVal = prevVal = currVal = i;
      /* loop until pre-calculated value found */
      /*  OR  until cycle generated */
      for(cycle=0; !cycle && memo_data[currVal]==UNSET;) { /* value not-pre-calculated */
        prevVal = currVal;
        currVal = digitFactorial(currVal);
        cycle   = isInList(startVal,currVal);
        memo_next[prevVal] = currVal;
      }
      setChainLengths(startVal,currVal);
    }
    if(memo_data[i]==target)
      ++found;
  }
  printf("Digit Factorial Chains (non-repeating) of length %u: %u\n",target, found);
  return 0;
}

void initializeMemotizationTable(const uint32_t x) {
  /* Calculate more than nessiary space, but don't waste too much space */
  uint32_t size = getMaxFactorialChainValue(x);
  memo_data = malloc(size * sizeof *memo_data);
  memo_next = malloc(size * sizeof *memo_next);
  if(memo_data == NULL ||
     memo_next == NULL)
    exit(EXIT_FAILURE);

  memset(memo_data, UNSET, size);
  memset(memo_next, UNSET, size);
  memo_data[0] = 2;
  memo_next[0] = 1;
}

uint32_t getMaxFactorialChainValue(const uint32_t x) {
  uint32_t i,val;
  char* buffer = malloc(BUF_LEN * sizeof *buffer);
  snprintf(buffer,BUF_LEN,"%u",x);
  if(buffer[0]!='9') /* if the first digit is not 9 subtract one */
    buffer[0] -= 1;
  for(i=1; buffer[i]; ++i)
    buffer[i] = '9'; /* make all following digits 9 */
  sscanf(buffer,"%u",&val);
  val = digitFactorial(val);
  if(val > x)
    val = getMaxFactorialChainValue(val);
  return val > x ? getMaxFactorialChainValue(val) : x;
}


uint32_t digitFactorial(const uint32_t x) {
  static char  *buffer = NULL;
  uint32_t i,outVal;

  if(buffer==NULL) /* Initialize once */
    buffer = malloc(BUF_LEN * sizeof *buffer);

  snprintf(buffer,BUF_LEN,"%u",x);
  for(i=outVal=0; buffer[i]; ++i)
    outVal += factorial[ buffer[i]-'0' ];
  return outVal;
}

uint32_t setChainLengths_cycle(const uint32_t cVal, const uint32_t tVal, const uint32_t x) {
  if(cVal==UNSET || cVal==tVal)
    return x+1;
  else
    return memo_data[cVal] = setChainLengths_cycle(memo_next[cVal],tVal,x+1);
}

uint32_t setChainLengths(const uint32_t cVal, const uint32_t tVal) {
  if(cVal==UNSET)
    return 0;
  if(memo_data[cVal]!=UNSET)
    return memo_data[cVal];
  if(cVal==tVal)
    return memo_data[cVal] = setChainLengths_cycle(memo_next[cVal],tVal,0);
  else
    return memo_data[cVal] = setChainLengths(memo_next[cVal],tVal) + 1;
}

uint32_t isInList(const uint32_t cVal, const uint32_t tVal) {
  if(cVal==UNSET)
    return 0;
  if(cVal==tVal)
    return 1;
  return isInList(memo_next[cVal], tVal);
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> <length>\n",str);
  printf("  Calculates the number of digit factorial chains \n");
  printf("  begginning with a natural number less then <limit> \n");
  printf(" who's non-repeating chain has a length of exactly  <length> \n\n");
  printf(" EXAMPLE: digit factorial chain sarting with 69 has length 5 \n");
  printf(" 69 --> 363600 --> 1454 --> 169 --> 363601 (--> 1454)\n\n");
  printf("  * limit  : a decimal number\n");
  printf("           : default = 1000000\n");
  printf("  * length : a decimal number\n");
  printf("           : default = 60\n");
  return 0;
}
