#include <gmp.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  ASCII_OFFSET 48

void print_help(const char * const  str);

int main(int argc, char* argv[]) {
  uint64_t num  = 2;    /* default */
  uint64_t exp  = 1000; /* default */
  uint64_t base = 10;   /* default */
  uint32_t i,sum;
  char    *str;
  mpz_t    x;
  mpz_init(x);

  if(argc > 1 &&!strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%llu",&num);
  if(argc > 2)
    sscanf(argv[2],"%llu",&exp);
  if(argc > 3)
    sscanf(argv[3],"%llu",&base);

  mpz_set_ui(x,num);
  mpz_pow_ui(x,x,exp);
  str = malloc( (mpz_sizeinbase(x,base)+2) * sizeof *str);
  gmp_sprintf(str,"%Zd",x);

  for(i=sum=0; str[i]; ++i)
    sum += str[i] - ASCII_OFFSET;

  printf("%u\n",sum);
  free(str);
  mpz_clear(x);
  return EXIT_SUCCESS;
}

void print_help(const char * const str) {
  printf("  Usage: %s <number> <exp> <base> \n",str);
  printf("  Calculates the sum of the digit values encoded in <base>\n");
  printf("  of <number> raised to <exp>\n");
  printf("  * number : a decimal number\n");
  printf("           : default = 2\n");
  printf("  * exp    : a decimal number\n");
  printf("           : default = 1000\n");
  printf("  * base   : a decimal number\n");
  printf("           : default = 10\n");
}
