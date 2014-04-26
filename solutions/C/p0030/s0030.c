#include <gmp.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  DECIMAL 10

/**
 * a = exponent
 * b = base of numbering system
 * c = current number of digits
 * upperbound: digitCount(c*(b-1)^2) > c
 */

void     initializeIncrementor(mpz_t value, const uint32_t base);
uint32_t digitCountInsufficient(const uint32_t digits, const uint32_t base, const uint32_t exp);
uint32_t digitCount(mpz_t value, const uint32_t base);
uint32_t sumOfSquareDigitsEqualsNumber(mpz_t value, const uint32_t digits, const uint32_t base, const uint32_t exp);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t exp    = 5;       /* default */
  uint32_t base   = DECIMAL; /* default */
  uint32_t digits;
  mpz_t    sum,curr;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&exp ); }
  if(argc > 2) {  sscanf(argv[2],"%u",&base); }
  if(base < 2 || base > 62)
    return EXIT_FAILURE;

  for(mpz_init_set_ui(sum,0), initializeIncrementor(curr,base),digits=2; digitCountInsufficient(digits,base,exp); ++digits)
    for(; digitCount(curr,base) == digits; mpz_add_ui(curr,curr,1))
      if(sumOfSquareDigitsEqualsNumber(curr,digits,base,exp))
        mpz_add(sum,sum,curr);
  gmp_printf("%Zd\n",sum);
  return EXIT_SUCCESS;
}

void initializeIncrementor(mpz_t value, const uint32_t base) {
  mpz_init(value);
  mpz_ui_pow_ui(value,base,2);
  mpz_sub_ui(value,value,1);
}

uint32_t digitCountInsufficient(const uint32_t digits, const uint32_t base, const uint32_t exp) {
  uint32_t bufferLen,outToken;
  char    *buffer;
  mpz_t    value;
  mpz_init(value);
  mpz_ui_pow_ui(value,base-1,exp);
  mpz_mul_ui(value,value,digits);
  bufferLen = mpz_sizeinbase(value,base) + 2; /* Don't trust this value, read manual */
  buffer    = malloc((bufferLen) * sizeof *buffer);
  mpz_get_str(buffer,base,value);
  mpz_clear(value);
  outToken = strlen(buffer) >= digits;
  free(buffer);
  return outToken;
}

uint32_t digitCount(mpz_t value, const uint32_t base) {
  static char  *buffer = NULL;
  static uint32_t size = 10;
  uint32_t numberLength;

  if(buffer==NULL)
    buffer = malloc(size * sizeof *buffer);
  numberLength = mpz_sizeinbase(value,base) + 2; /* Don't trust this value, read manual */
  if(numberLength > size)
    buffer = realloc(buffer, (size*=2) * sizeof *buffer);
  mpz_get_str(buffer,base,value);
  return strlen(buffer);
}

uint32_t sumOfSquareDigitsEqualsNumber(mpz_t value, const uint32_t digits, const uint32_t base, const uint32_t exp) {
  static const uint32_t digitStrLen = 2;
  static uint32_t size= 10;
  static char *buffer = NULL;
  static char *digit  = NULL;
  uint32_t i,bufferLen,outToken;
  mpz_t temp,sum;
  mpz_init(temp);
  mpz_init(sum);

  if(buffer==NULL)
    buffer= malloc( size * sizeof *buffer);

  if(digit ==NULL)
    digit = calloc( digitStrLen , sizeof *digit);

  bufferLen = mpz_sizeinbase(value,base) +2; /* Don't trust this value, read manual */
  if(bufferLen > size)
    buffer = realloc(buffer, (size*=2) * sizeof *buffer);
  mpz_get_str(buffer,base,value);

  for(mpz_set_ui(sum,0),i=0; i<digits; ++i) {
    digit[0] = buffer[i];
    mpz_set_str(temp,digit,base);
    mpz_pow_ui(temp,temp,exp);
    mpz_add(sum,sum,temp);
  }
  outToken = mpz_cmp(sum,value) == 0;
  mpz_clear(temp);
  mpz_clear(sum);
  return outToken;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <exponent> <base>\n",str);
  printf("  Calculates the sum of all numbers in <base> numbering system \n");
  printf("  whose digits raised to <exponent> and summed together equals the original number \n");
  printf("  * exponent  : a positive decimal number\n");
  printf("              : default = 5");
  printf("  * base      : a decimal number in range [2,62]\n");
  printf("              : default = 10\n");
  return 0;
}
