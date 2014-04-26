#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  DECIMAL 10

/**
 * Don't just brute force the search
 * look for canidates based on the modulo of the last 9 digits
 * then check canidates for their leading digits
 * generate the leading digits using the following identity:
 * F(n): ( phi^n - (1-phi)^n ) / sqrt(5)
 * also not we only care about the first 9 digits.
 * take log10 of the number to get the digit count.
 * reduce:  log10( phi^n / sqrt(5) )
 * to this: n*log10(phi) - log10(sqrt(5));
 * precompute these values
 * use doubles since precision will hold for the first nine digits
 */

uint32_t nextFibNumber(void);
uint32_t suffixIsPandigital(uint32_t num);
uint32_t prefixIsPandigital(uint32_t num);
uint32_t isPandigitalNumber(uint32_t num);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t target = 1; /* default */
  uint32_t fibIndex,fibNum,counter;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%lu",&target); }

  for(counter=0,fibIndex=3,fibNum=nextFibNumber(); counter<target; ++fibIndex,fibNum=nextFibNumber())
    if(fibIndex > 540 && suffixIsPandigital(fibNum) && prefixIsPandigital(fibIndex))
      ++counter;

  --fibIndex; /* undo increment in loop epilogue */
  printf("The %uth double-pandigital fibonacci number is term: %u\n",target,fibIndex);
  return EXIT_SUCCESS;
}

uint32_t nextFibNumber(void) {
  static const uint32_t modulo    = 1000000000;
  static       uint32_t temporary = 1;
  static       uint32_t fibMinus0 = 1;
  static       uint32_t fibMinus1 = 1;
  temporary = fibMinus0;
  fibMinus0 = (fibMinus0 + fibMinus1) % modulo;
  fibMinus1 = temporary;
  return fibMinus0;
}

uint32_t suffixIsPandigital(uint32_t num) {
  return isPandigitalNumber(num);
}

uint32_t prefixIsPandigital(uint32_t fibIndex) {
  static const double log10phi   = 0.20898764024997873;
  static const double log10sqrt5 = 0.3494850021680094;
  double       tVal;
  uint32_t     upper;
  tVal = fibIndex * log10phi - log10sqrt5;
  upper = pow(10,tVal-(uint64_t)tVal +8);
  return isPandigitalNumber(upper);
}

uint32_t isPandigitalNumber(uint32_t num) {
  static const uint32_t minVal  = 123456789;
  static const uint32_t maxVal  = 987654321;
  static const uint8_t  length  = DECIMAL;
  static       uint8_t *boolArr = NULL;

  if(num < minVal || maxVal < num)
    return 0;

  if(boolArr==NULL) /* initialize once */
    boolArr = malloc(length * sizeof *boolArr);

  memset(boolArr, 0, length);

  for(; num>0; num/=10 )
    if(boolArr[num %10]++ > 0)
      return 0;
  return boolArr[0]==0;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <occurance> \n",str);
  printf("  Calculates the <occurance>th double-pandigital fibonacci number term. \n");
  printf("  A double-pandigital number has pandigital first and last nine digits. \n");
  printf("  * term  : a positive decimal number\n");
  printf("          : default = 1\n\n");
  return 0;
}
