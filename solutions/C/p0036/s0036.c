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
int       isPalindrome_bin(const uint32_t num);
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
  uint32_t square;

  if(base==0) /* Base zero is non-sense           */
    return 0;
  if(base==1) /* Tally system, must be palindrome */
    return 1;
  if(base==2) /* Leverage bitwise operations      */
    return isPalindrome_bin(num);
  if(num==0)  /* Log(0) doesn't compute           */
    return 1; /* But single digit is palindrome   */

  digits  = ((uint8_t) floor(log(num)/log(base))) + 1;
  divisor = (uint32_t) pow(base,digits-1);
  square  = base*base;

  while(digits > 1) {
    quocient  = num / divisor;
    remainder = num % base;
    if(quocient != remainder)
      return 0;
    num     %= divisor;
    num     /= base;
    divisor /= square;
    digits  -= 2;
  }
  return 1;
}

int isPalindrome_bin(const uint32_t num) {
  static const uint8_t MultiplyDeBruijnBitPosition[32] = {
     0,  1, 28,  2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17,  4, 8,
    31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18,  6, 11,  5, 10, 9
  };
  uint32_t x = num;
  uint8_t  b;

  if(num==0)   /* Only palindrome with 'trailing/leading' zeros              */
    return 1;  /* Since zero is a single digit number                        */
  if(!(num&1)) /* leading zeros not allowed => trailing zeros must not exist */
    return 0;

  /* reverse bits of x */
  x = ((x & 0x55555555) <<  1) | ((x & 0xAAAAAAAA) >>  1);
  x = ((x & 0x33333333) <<  2) | ((x & 0xCCCCCCCC) >>  2);
  x = ((x & 0x0F0F0F0F) <<  4) | ((x & 0xF0F0F0F0) >>  4);
  x = ((x & 0x00FF00FF) <<  8) | ((x & 0xFF00FF00) >>  8);
  x = ((x & 0x0000FFFF) << 16) | ((x & 0xFFFF0000) >> 16);

  /* get number of trailing zeros */
  b = MultiplyDeBruijnBitPosition[((uint32_t)((x & -x) * 0x077CB531U)) >> 27];

  x >>= b;


  /* use XOR to check if it is a palindrome */
  return !(x ^ num);
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
