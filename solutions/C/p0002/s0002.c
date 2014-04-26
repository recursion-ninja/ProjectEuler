#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

int       isDivisible(const uint64_t num, const uint64_t * const divisors, const uint32_t dcount);
uint32_t  countCommas(const char * const str);
void      getDefaultDivisors(uint64_t ** const divisors, uint32_t * const count);
void      parseDivisors(uint64_t ** const divisors, uint32_t * const count, const char * const str);
void      print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t fib_minus_2 = 1;
  uint64_t fib_minus_1 = 2;
  uint64_t fib_minus_0 = fib_minus_1 + fib_minus_2;
  uint64_t summation   = 0;
  uint64_t limit       = 4000000;/* default */
  uint32_t count       = 1;      /* default */
  uint64_t *divisors   = NULL;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%llu",&limit);
  if(argc > 2)
    parseDivisors(&divisors,&count,argv[2]);
  else
    getDefaultDivisors(&divisors,&count);

  if(isDivisible(fib_minus_2,divisors,count))
    summation += fib_minus_2;
  if(isDivisible(fib_minus_1,divisors,count))
    summation += fib_minus_1;
  while(fib_minus_0 < limit) {
    if(isDivisible(fib_minus_0,divisors,count))
      summation += fib_minus_0;
    fib_minus_2 = fib_minus_1;
    fib_minus_1 = fib_minus_0;
    fib_minus_0 = fib_minus_1 + fib_minus_2;
  }
  printf("%llu\n",summation);
  free(divisors);
  return EXIT_SUCCESS;
}

int isDivisible(const uint64_t num, const uint64_t * const divisors, const uint32_t count) {
  uint32_t i,divisable;
  for(i=divisable=0; !divisable && i<count; ++i)
    divisable |= !(num % divisors[i]);
  return divisable;
}

void getDefaultDivisors(uint64_t ** const divisors, uint32_t * const count) {
  static const uint64_t defaults[1] = {2};
  static const uint32_t quantity    = 1;

  if(*divisors!=NULL)
    free(*divisors);

  *count = quantity;
  *divisors = malloc(*count * sizeof **divisors);
  memcpy(*divisors,defaults,*count * sizeof **divisors);
}

void parseDivisors(uint64_t ** const divisors, uint32_t * const count, const char * const str) {
  uint64_t tmp;
  uint32_t i,j,k,len,size=10;
  char     *buf = malloc(size * sizeof *buf);

  if(*divisors!=NULL)
    free(*divisors);

  *count    = countCommas(str)+1;
  *divisors = malloc(*count * sizeof **divisors);
  len = strlen(str);

  for(i=0,k=0; i<*count; ++i,++k) {
    for(j=0; str[k]!=',' && k<len; ++j,++k) {
      if(j >= size)
        buf = realloc(buf, (size*=2) * sizeof *buf);
      buf[j] = str[k];
    }
    if(j >= size)
      buf = realloc(buf, (size*=2) * sizeof *buf);
    buf[j] = '\0';
    sscanf(buf,"%llu",&tmp);
    (*divisors)[i] = tmp;
  }
}

uint32_t countCommas(const char * const str) {
  uint32_t i,commas;

  for(i=commas=0; str[i]; ++i)
    if(str[i]==',')
      ++commas;

  return commas;
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the sum of all fibonacci numbers\n");
  printf("  which are divisible by at least one divisor\n");
  printf("  and less then the limit\n");
  printf("  * limit    : a decimal number exclusive bound\n");
  printf("             : default = 4,000,000\n");
  printf("  * divisors : a decimal number csv list (no spaces)\n");
  printf("             : default = 2\n");
}
