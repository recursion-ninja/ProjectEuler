#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * NOTES:
 * Use logs to inteligently avoid stringification
 * Utilize computer's binary representation for base 2
 * Utilize computer's log10 coprocessor for base 10
 */

int       isPalindrome_num(uint32_t num, const uint8_t base);
uint8_t*  parseBases (const char * const str, uint32_t * const count);
uint32_t  countCommas(const char * const str);
void      print_help (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit = 1000000; /* default */
  uint32_t i,j,count;
  uint64_t sum;
  uint8_t *base    = NULL;
  char    *baseStr = NULL;
  int     isMultiPalindrome;
  count = 2;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]), EXIT_SUCCESS; }
  if(argc > 1) {  sscanf(argv[1],"%u",&limit); }
  if(argc > 2) {  baseStr = argv[2]; }

  base = parseBases(baseStr,&count);

  for(i=sum=0; i<limit; ++i) {
    isMultiPalindrome = 1;
    for(j=0; j<count; ++j)
      isMultiPalindrome &= isPalindrome_num(i,base[j]);
    if(isMultiPalindrome)
      sum += i;
  }

  printf("%llu\n",sum);
  return EXIT_SUCCESS;
}

int isPalindrome_num(uint32_t num, const uint8_t base) {
  uint8_t  digits;
  uint32_t divisor,quocient,remainder;

  if(base==0)
    return 0;
  if(base==1)
    return 1; //tally system, must be palindrome

  if(num==0)
    return 1;

  digits  = ((uint8_t) floor(log(num)/log(base))) + 1;
  divisor = (uint32_t) pow(base,digits-1);

  while(digits > 1) {
    quocient  = num / divisor;
    remainder = num % base;
    if(quocient != remainder)
      return 0;
    num     %= divisor;
    num     -= remainder;
    num     /= base;
    divisor /= base*base;
    digits  -= 2;
  }
  return 1;
}

uint8_t*  parseBases (const char * const str, uint32_t * const count) {
  static const uint8_t defaultBases[2] = {2,10};
  uint8_t *out;
  uint8_t tmp;
  uint32_t i,j,k,len,size=10;
  char     *buf = malloc(size * sizeof *buf);

  if(str==NULL) {
    *count = sizeof(defaultBases)/sizeof(defaultBases[0]);
    return (uint8_t*) &defaultBases;
  }

  *count = countCommas(str)+1;
  out    = malloc(*count * sizeof *out);
  len    = strlen(str);
  for(i=0,k=0; i<*count; ++i,++k) {
    for(j=0; str[k]!=',' && k<len; ++j,++k) {
      if(j >= size)
        buf = realloc(buf,(size*=2)*sizeof(*buf));
      buf[j] = str[k];
    }
    if(j >= size)
      buf = realloc(buf,(size*=2)*sizeof(*buf));
    buf[j] = '\0';
    sscanf(buf,"%c",&tmp);
    out[i] = tmp;
  }
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

void print_help(const char * const str) {
  printf("  Usage: %s <limit> <bases>\n",str);
  printf("  Calculates the sum of all number less then <limit>\n");
  printf("  which is a palindrome in each of <bases> representation\n");
  printf("  * limit    : a decimal number exclusive bound\n");
  printf("             : default = 1000000\n");
  printf("  * bases    : a decimal number csv list (no spaces)\n");
  printf("             : default = 2,10\n");
}
