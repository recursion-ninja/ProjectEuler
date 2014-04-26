#include <gmp.h>
#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t lead     = 10;  /* default */
  uint32_t base     = 10;  /* default */
  uint32_t i,j,size = 10;
  int      c,end;
  char    *buf;
  mpz_t    n;
  mpz_t    sum;

  if(argc > 1 &&!strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&lead);
  if(argc > 2)
    sscanf(argv[2],"%u",&base);

  buf = malloc(size * sizeof *buf);
  mpz_init(n);
  mpz_init(sum);

  for(i=0,end=0; !end; ++i) {
    for(j=0; (c=fgetc(stdin))!=EOF && c!='\n'; ++j) {
      if(j >= size)
        buf = realloc(buf, (size *= 2) * sizeof *buf);
      buf[j] = c;
    }
    buf[j] = '\0';
    end = c==EOF;
    if(end)
      break;
    mpz_set_str(n, buf, base);
    mpz_add(sum,sum,n);
  }

  if(!lead)
    gmp_printf("%Zd\n", sum);
  else {
    buf       = realloc(buf, (mpz_sizeinbase(sum, base)+2) * sizeof *buf);
    gmp_sprintf(buf,"%Zd\n", sum);
    buf[lead] = '\0';
    printf("%s\n",buf);
  }
  free(buf);
  mpz_clear(n);
  mpz_clear(sum);
  return EXIT_SUCCESS;
}

void print_help(const char * const str) {
  printf("  Usage: %s <lead> <base> \n",str);
  printf("  Calculates the sum of large digits encoded in <base> \n");
  printf("  and prints out the first <lead> digits of the sum \n");
  printf("  or  prints out the full sum if <lead> is zero. \n");
  printf("  Can specify digits buffer \n");
  printf("  * lead   : a decimal number\n");
  printf("           : default = 10\n");
  printf("  * base   : a decimal number\n");
  printf("           : default = 10\n");
}
