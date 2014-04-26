#include <gmp.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t print_help(const char * const  str);

int main(int argc, char* argv[]) {
  uint64_t num  = 2;    /* default */
  uint64_t exp  = 1000; /* default */
  uint64_t base = 10;   /* default */
  uint64_t sum  = 0;
  uint32_t i,len;
  char    *str;
  mpz_t    x;
  mpz_init(x);

  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%llu",&num); }
  if(argc > 2) { sscanf(argv[2],"%llu",&exp); }
  if(argc > 3) { sscanf(argv[3],"%llu",&base); }
  mpz_set_ui(x,num);
  mpz_pow_ui(x,x,exp);
  len = mpz_sizeinbase(x,base);
  str = (char*) malloc( (len+2) * sizeof(char));
  gmp_sprintf(str,"%Zd\n",x);
  for(i=0,sum=0; i<len; ++i)
    sum += str[i]-48;
  printf("%llu\n",sum);
  return 0;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <number> <exp> <base> \n",str);
  printf("  Calculates the sumation of digit values \n");
  printf("  of <number> raised to <exp> encoded in <base> \n");
  printf("  Can specify digits buffer \n");
  printf("  * number : a decimal number\n");
  printf("           : default = 2\n");
  printf("  * exp    : a decimal number\n");
  printf("           : default = 1000\n");
  printf("  * base   : a decimal number\n");
  printf("           : default = 10\n");
  return 0;
}
