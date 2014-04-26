#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t  isDivisible(const uint64_t num, const uint64_t * const divisors, const uint32_t dcount);
uint64_t* parseDivisors(uint32_t * const count, const char * const str);
uint32_t  countCommas(const char * const str);
void      expand(char ** const arr, uint32_t * const size);
void      print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t i          = 0;
  uint64_t summation  = 0;
  uint32_t count      = 2;    /* default */
  uint64_t limit      = 1000; /* default */
  uint64_t *divisors;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%llu",&limit);

  divisors = parseDivisors(&count,(argc > 2) ? argv[2]: NULL);

  for(i=1; i<limit; ++i)
    if(isDivisible(i,divisors,count))
      summation += i;

  printf("%llu\n",summation);
  return EXIT_SUCCESS;
}

uint64_t* parseDivisors(uint32_t * const count, const char * const str) {
  uint64_t *out;
  uint64_t tmp;
  uint32_t i,j,k,len,size=10;
  char     *buf = malloc(size * sizeof *buf);

  *count = (str==NULL) ? 2 : countCommas(str)+1;
  out    = malloc(*count * sizeof *out);
  if(str==NULL) {
   out[0] = 3;
   out[1] = 5;
  }
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

void expand(char ** const arr, uint32_t * const size) {
  *size *= 2;
  *arr   = realloc(*arr,*size * sizeof **arr);
}

uint32_t isDivisible(const uint64_t num, const uint64_t * const  divisors, const uint32_t dcount) {
  uint32_t out = 0;
  uint32_t i;
  for(i=0; !out && i<dcount; ++i)
    out |= !(num % divisors[i]);
  return out;
}

uint32_t countCommas(const char * const str) {
  int limit = strlen(str);
  int i;
  int commas = 0;
  if(str==NULL)
    return 0;
  for(i=0; i<limit; ++i)
    if(str[i]==',')
      ++commas;
  return commas;
}

void  print_help(const char * const str) {
  printf("  Usage: %s <limit> <divisors>\n",str);
  printf("  Calculates the sum of all numbers which are\n");
  printf("  divisible by one or more of the divisors\n");
  printf("  and less then the limit\n");
  printf("  * limit    : a decimal number exclusive bound\n");
  printf("             : default = 1000\n");
  printf("  * divisors : a decimal number csv list (no spaces)\n");
  printf("             : default = 3,5\n");
}
