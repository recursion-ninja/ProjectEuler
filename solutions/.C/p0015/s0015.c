#include <gmp.h>
#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t rows = 20;  /* default */
  uint64_t cols = 20;  /* default */
  uint64_t i;
  mpz_t numer;
  mpz_t denom;
  mpz_init(numer);
  mpz_init(denom);
  mpz_set_ui(numer,1);
  mpz_set_ui(denom,1);

  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%llu",&rows); }
  if(argc > 2) { sscanf(argv[2],"%llu",&cols); }

  for(i=2; i<=rows+cols; ++i)
    mpz_mul_ui(numer,numer,i);
  for(i=2; i<=rows; ++i)
    mpz_mul_ui(denom,denom,i);
  for(i=2; i<=cols; ++i)
    mpz_mul_ui(denom,denom,i);
  mpz_tdiv_q(numer,numer,denom);
  gmp_printf("%Zd\n", numer);
  return 0;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <rows> <cols> \n",str);
  printf("  Calculates the sum of number of paths which exist \n");
  printf("  to travel from one corner to the opposite corner  \n");
  printf("  of a <rows> by <cols> grid. \n");
  printf("  Can specify digits buffer \n");
  printf("  * rows   : a decimal number\n");
  printf("           : default = 20\n");
  printf("  * cols   : a decimal number\n");
  printf("           : default = 20\n");
  return 0;
}
