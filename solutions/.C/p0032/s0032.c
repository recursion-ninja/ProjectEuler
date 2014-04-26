#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  DECIMAL       10

/**
 * NOTES:
 * Since Products are counted only once, search through products!
 * Sheck each product for a valid solution
 */

uint32_t isSolutionProduct(const uint32_t product, const uint32_t base);
uint32_t isPandigitalTrio (const uint32_t base, uint32_t a, uint32_t b, uint32_t c);
void     print_help       (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t i,limit,product,productSum;
  uint32_t base = DECIMAL;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;

  for(i=0,limit=1; i<base/2-1; ++i)
    limit *= base;

  productSum = 0;
  for(product=1; product<limit; ++product)
    if(isSolutionProduct(product,base))
      productSum += product;

  printf("%u\n",productSum);
  return EXIT_SUCCESS;
}

uint32_t isSolutionProduct(const uint32_t product, const uint32_t base) {
  uint32_t i, max = (uint32_t) ceil(sqrt(product));

  for(i=2; i<=max; ++i)
    if(!(product % i))
      if(isPandigitalTrio(base, product, i, product/i))
        return 1;

  return 0;
}

uint32_t isPandigitalTrio(const uint32_t base, uint32_t a, uint32_t b, uint32_t c) {
  static uint8_t *counter = NULL;
  uint32_t i,isPandigital;

  if(counter==NULL)
    counter = malloc(base * sizeof *counter);

  memset(counter,0,base);
  /* no need for early exits, just make the code understandable */

  for(; a; a/=base)
    ++(counter[a%base]);

  for(; b; b/=base)
    ++(counter[b%base]);

  for(; c; c/=base)
    ++(counter[c%base]);

  isPandigital = counter[0] == 0;

  for(i=1; isPandigital && i<base; ++i)
    isPandigital = counter[i] == 1;

  return isPandigital;
}

void print_help(const char * const str) {
  printf("  Usage: %s <base> \n",str);
  printf("  Calculate sum of product such that:\n");
  printf("  a * b = c and the concatenation of 'abc' is pandigital in base <base>\n");
  printf("  * base  : a decimal number [2,62]\n");
  printf("          : default = 10\n");
}
