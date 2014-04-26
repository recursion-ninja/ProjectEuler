#include <gmp.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t rows = 20;  /* default */
  uint64_t cols = 20;  /* default */
  mpz_t numer,denom;
  mpz_init(numer);
  mpz_init(denom);

  if(argc > 1 &&!strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%llu",&rows);
  if(argc > 2)
    sscanf(argv[2],"%llu",&cols);

  /* (rows+cols)!/((rows)!*(cols)!) */
  mpz_fac_ui(numer,rows+cols);
  mpz_fac_ui(denom,rows);
  mpz_tdiv_q(numer,numer,denom);
  mpz_fac_ui(denom,cols);
  mpz_tdiv_q(numer,numer,denom);

  gmp_printf("%Zd\n", numer);

  mpz_clear(numer);
  mpz_clear(denom);
  return EXIT_SUCCESS;
}

void print_help(const char * const str) {
  printf("  Usage: %s <rows> <cols> \n",str);
  printf("  Calculates the sum of number of paths which exist \n");
  printf("  to travel from one corner to the opposite corner  \n");
  printf("  of a <rows> by <cols> grid. \n");
  printf("  Can specify digits buffer \n");
  printf("  * rows   : a decimal number\n");
  printf("           : default = 20\n");
  printf("  * cols   : a decimal number\n");
  printf("           : default = 20\n");
}
