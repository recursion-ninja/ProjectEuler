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
  uint64_t target=1000; /* Default */
  uint64_t i=1,strLen,digits,size=10;
  char     *buf;
  mpz_t    fibMinus_2;
  mpz_t    fibMinus_1;
  mpz_t    fibMinus_0;

  mpz_init_set_ui(fibMinus_2,1);
  mpz_init_set_ui(fibMinus_1,1);
  mpz_init_set_ui(fibMinus_0,2);

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%llu",&target); }

  buf = (char*) malloc(size * sizeof(char));

  if(target!=1) {
    for(i=3; i<UINT32_MAX; ++i) {
      strLen = mpz_sizeinbase(fibMinus_0,BASE) +2;
      if(strLen >= size)
        buf = (char*) realloc(buf, (size *= 2) * sizeof(char));
      gmp_sprintf(buf,"%Zd",fibMinus_0);
      digits = strlen(buf); /* get 'len' value AFTER string converstion */
      if(digits >= target)
        break;

      mpz_set(fibMinus_2,fibMinus_1);
      mpz_set(fibMinus_1,fibMinus_0);
      mpz_add(fibMinus_0,fibMinus_1,fibMinus_2);
    }
  }
  printf("%lluth term contains %llu digits\n",i,target);

  return 0;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <digits> \n",str);
  printf("  Finds the smallest fibonacci number term which contains at least <digits> digits \n");
  printf("  * digits : a decimal number \n");
  printf("           : default = 1000\n");
  return 0;
}
