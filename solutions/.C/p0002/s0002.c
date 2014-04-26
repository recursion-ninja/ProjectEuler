#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t  isDivisible(const uint64_t num, const uint64_t * const divisors, const uint32_t dcount);
uint32_t  countCommas(const char * const str);
uint64_t* parseDivisors(uint32_t * const count, const char * const str);
void      expand(char ** const arr, uint32_t * const size);
uint32_t  print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t fib_minus_2 = 1;
  uint64_t fib_minus_1 = 2;
  uint64_t fib_minus_0 = fib_minus_1 + fib_minus_2;
  uint64_t summation   = 0;
  uint64_t limit       = 4000000;/* default */
  uint32_t count       = 1;      /* default */
  uint64_t *divisors;
  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%llu",&limit); }
  divisors = parseDivisors(&count,(argc > 2) ? argv[2]: NULL);

  if(isDivisible(fib_minus_2,divisors,count)) { summation += fib_minus_2; }
  if(isDivisible(fib_minus_1,divisors,count)) { summation += fib_minus_1; }
  while(fib_minus_0 < limit ) {
    if(isDivisible(fib_minus_0,divisors,count))
      summation += fib_minus_0;
    fib_minus_2 = fib_minus_1;
    fib_minus_1 = fib_minus_0;
    fib_minus_0 = fib_minus_1 + fib_minus_2;
  }
  printf("%llu\n",summation);
  return 0;
}

uint32_t isDivisible(const uint64_t num, const uint64_t * const divisors, const uint32_t dcount) {
  uint32_t out = 0;
  uint32_t i;
  for(i=0; !out && i<dcount; ++i)
    out |= !(num % divisors[i]);
  return out;
}
uint64_t* parseDivisors(uint32_t * const count, const char * const  str) {
  uint64_t *out;
  uint64_t tmp;
  uint32_t i,j,k,len,size=10;
  char     *buf = malloc(size * sizeof(char));

  *count = (str==NULL) ? 1 : countCommas(str)+1;
  out    = malloc(*count * sizeof(uint64_t));
  if(str==NULL)
   out[0] = 2;
  else {
    len = strlen(str);
    for(i=0,k=0; i<*count; ++i,++k) {
      for(j=0; str[k]!=',' && k<len; ++j,++k) {
        if(j >= size)
          expand(&buf,&size);
        buf[j] = str[k];
      }
      if(j >= size)
        expand(&buf,&size);
      buf[j] = '\0';
      sscanf(buf,"%llu",&tmp);
      out[i] = tmp;
    }
  }
  return out;
}

uint32_t countCommas(const char * const str) {
  uint32_t limit,i, commas;
  if(str==NULL)
    return 0;
  for(i=commas=0,limit=strlen(str); i<limit; ++i)
    if(str[i]==',')
      ++commas;
  return commas;
}

void expand(char ** const arr, uint32_t * const size) {
  *size *= 2;
  *arr   = realloc(*arr,*size * sizeof(char));
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the sum of all fibonacci numbers\n");
  printf("  which are divisible by at least one divisor\n");
  printf("  and less then the limit\n");
  printf("  * limit    : a decimal number exclusive bound\n");
  printf("             : default = 4,000,000\n");
  printf("  * divisors : a decimal number csv list (no spaces)\n");
  printf("             : default = 2\n");
  return 0;
}
