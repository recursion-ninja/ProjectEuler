#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  MAX_RADIX 18 /*limited by size of uint64_t : 2^63 <  18*18! < 2^64 */

/**
 * max digits == radix defines an implicit upper bound of search space
 */

void     uitoa(const uint64_t value, char * const str, const uint32_t radix);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t radix=10,print=0; /* default */
  uint32_t j;
  uint64_t i,sum,total,limit;
  uint64_t *fact;
  char     *str;

  /* parse args */
  if(argc  > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc  > 1) {  sscanf(argv[1],"%u",&radix); }
  if(argc  > 2) {  sscanf(argv[2],"%u",&print); }
  if(radix > MAX_RADIX || radix < 2)         { return print_help(argv[0]); }

  /* set radix specific data */
  str  = (char*    ) malloc((radix+1) * sizeof(char    ));
  fact = (uint64_t*) malloc( radix    * sizeof(uint64_t));
  for(i=fact[0]=1; i<radix; ++i)
    fact[i] = fact[i-1]*i;
  limit = radix * fact[radix-1];

  /* search for qualifying numbers */
  for(i=radix,total=0; i<limit; ++i) {
     uitoa(i,str,radix);
    for(j=sum=0; j<radix && str[j]; ++j)
      sum += fact[ str[j] - (47 < str[j] && str[j] < 58 ? 48 : 55) ];
    if(sum==i) {
      if(print)
        printf("%s_(%u) %llu_(10)\n",str,radix,i);
      total += i;
    }
  }
  printf("total: %llu\n",total);
  return 0;
}

void uitoa(const uint64_t value, char * const str, const uint32_t radix) {
  static char dig[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  uint32_t n=0;
  uint32_t i;
  char     tmp;

  i = value;
  do {
    str[n++] = dig[i%radix];
    i /= radix;
  } while (i);
  str[n] = '\0';
  for(i=0; i<n/2; ++i) { /* reverse string */
    tmp        = str[  i  ];
    str[  i  ] = str[n-i-1];
    str[n-i-1] = tmp;
  }
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <radix> <print>\n",str);
  printf("  Calculates the sum of all numbers whose digit's (in base <radix>) \n");
  printf("  factorial values sum up to the number's value. \n");
  printf("  Prints each qualifying number in base <radix> if <print> is true. \n");
  printf("  * radix : unsigned integer in range [2,18] \n");
  printf("          : default 10 \n");
  printf("  * print : unsigned integer interpreted as a boolean value \n");
  printf("          : default 0 \n");
  return 0;
}
