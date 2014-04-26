#include <gmp.h>
#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void     expand(char ** const arr, uint32_t * const digits);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t lead   = 10;  /* default */
  uint32_t base   = 10;  /* default */
  uint32_t digits = 10;  /* default */
  uint32_t i,j,end;
  char c,*buf,*out;
  mpz_t n;
  mpz_t sum;

  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%u",&lead);   }
  if(argc > 2) { sscanf(argv[2],"%u",&base);   }
  if(argc > 3) { sscanf(argv[3],"%u",&digits); }

  buf = malloc(digits+1 * sizeof(char));
  mpz_init(n);
  mpz_init(sum);

  for(i=0,end=0; !end; ++i) {
    for(j=0; (c=fgetc(stdin))!=EOF && c!='\n'; ++j) {
      if(j >= digits)
        expand(&buf,&digits);
      buf[j] = c;
    }
    buf[j] = '\0';
    end = c==EOF;
    if(end)
      break;
    mpz_set_str(n, buf, base);
    mpz_add(sum,sum,n);
  }

  out = (char*) malloc( (digits+i) * sizeof(char));
  if(!lead)
    gmp_printf("%Zd\n", sum);
  else {
    gmp_sprintf(out,"%Zd\n", sum);
    out[lead] = '\0';
    printf("%s\n",out);
  }
  return 0;
}

void     expand(char ** const arr, uint32_t * const digits) {
  *digits *= 2;
  *arr = realloc(*arr,*digits * sizeof(char));
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <lead> <base> <digits> \n",str);
  printf("  Calculates the sum of large digits encoded in <base> \n");
  printf("  and prints out the first <lead> digits of the sum \n");
  printf("  or  prints out the full sum if <lead> is zero. \n");
  printf("  Can specify digits buffer \n");
  printf("  * lead   : a decimal number\n");
  printf("           : default = 10\n");
  printf("  * base   : a decimal number\n");
  printf("           : default = 10\n");
  printf("  * digits : a decimal number\n");
  printf("           : default = 10\n");
  return 0;
}
