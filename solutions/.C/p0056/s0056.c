#include <gmp.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  BASE 10
/**
 * NOTES:
 * DO NOT trust mpz_sizeinbase value to correctly return number of digits (read the docs)
 */

uint32_t print_help  (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit=100; /* Default */
  uint32_t size =10;
  uint32_t a,b,i,strLen;
  uint64_t max,sum;
  char     *buf;
  mpz_t    product;

  mpz_init(product);

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&limit); }

  buf = (char*) malloc(size * sizeof(char));
  max = 0;
  for(a=1; a<limit; ++a) {
    for(b=1; b<limit; ++b) {
      mpz_ui_pow_ui(product,a,b);
      strLen = mpz_sizeinbase(product,BASE)+2;
      if(strLen >= size)
        buf = (char*) realloc(buf, (size *= 2) * sizeof(char));
      gmp_sprintf(buf,"%Zd",product);
      for(i=sum=0; buf[i]; ++i)
        sum += buf[i]-48;
      if(sum > max)
         max = sum;
    }
  }
  printf("%llu\n",max);

  return 0;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  For the numbers a^b; a,b < <limit> ");
  printf("  Finds the largest sum of digits\n");
  printf("  * limit : a decimal number \n");
  printf("          : default = 100\n");
  return 0;
}
