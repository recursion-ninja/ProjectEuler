#include <gmp.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t number = 100; /* Default */
  uint32_t i,sum,len;
  char     *str;
  mpz_t factorial;

  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%u",&number);   }

  mpz_init(factorial);
  mpz_set_ui(factorial,1);

  for(i=1; i<=number; ++i)
    mpz_mul_ui(factorial,factorial,i);

  len = mpz_sizeinbase(factorial,10);
  str = (char*) malloc(len+1 * sizeof(char));
  gmp_sprintf(str,"%Zd\n",factorial);

  for(i=0,sum=0; i<len-1; ++i)
    sum += str[i]-48;

  printf("%u\n",sum);
  return 0;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <lead> <base> <digits> \n",str);
  printf("  Calculates the sum of large digits encoded in <base> \n");
  printf("  and prints out the first <lead> digits of the sum \n");
  printf("  or  prints out the full sum if <lead> negative. \n");
  printf("  Can specify digits buffer \n");
  printf("  * lead   : a decimal number\n");
  printf("           : default = 10\n");
  printf("  * base   : a decimal number\n");
  printf("           : default = 10\n");
  printf("  * digits : a decimal number\n");
  printf("           : default = 10\n");
  return 0;
}
