#include <gmp.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  ASCII_OFFSET 48

void print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t number = 100; /* Default */
  uint32_t i,sum;
  char     *str;
  mpz_t factorial;

  if(argc > 1 &&!strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&number);

  mpz_init(factorial);
  mpz_fac_ui(factorial,number);

  str = malloc((mpz_sizeinbase(factorial,10)+2) * sizeof *str);
  gmp_sprintf(str,"%Zd",factorial);

  for(i=sum=0; str[i]; ++i)
    sum += str[i] - ASCII_OFFSET;

  printf("%u\n",sum);
  free(str);
  mpz_clear(factorial);
  return EXIT_SUCCESS;
}

void print_help(const char * const str) {
  printf("  Usage: %s <num> <base> \n",str);
  printf("  Calculates the sum of digit values encoded in <base> \n");
  printf("  of <num> factiorial \n");
  printf("  * num    : a decimal number\n");
  printf("           : default = 100\n");
  printf("  * base   : a decimal number\n");
  printf("           : default = 10\n");
}
