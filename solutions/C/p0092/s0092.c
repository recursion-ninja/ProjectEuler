#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  BUF_LEN 21 /* Safely Holds UIN64_MAX */

uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t i,j,eighty_nine,one;
  uint32_t limit=10000000; /* default */
  uint64_t n;
  uint8_t  *memo; /* array 1 = 1; 89 = 89, 0 = unset */
  char     *buf;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&limit); }

  memo = (uint8_t*) calloc(limit,    sizeof(uint8_t));
  buf  = (char*   ) malloc(BUF_LEN * sizeof(char   ));
  one  = eighty_nine = 0;

  for(i=1; i<limit; ++i) {
    for(n=i; n!=1 && n!=89;) {
      snprintf(buf,BUF_LEN,"%llu",n);
      for(j=n=0; buf[j]; ++j)
        n += (buf[j]-48)*(buf[j]-48);
      if(memo[n]) /* if convergence fo N has alread been calculated */
        n = memo[n];
    }
    memo[i] = n;
    if(memo[i]==1)
      ++one;
    else
      ++eighty_nine;
  }
  printf("%u \n",eighty_nine);
  return 0;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  If we take a given number, and sum up the square of each digit,\n");
  printf("  and repeat the process for the new number infinitly,\n");
  printf("  all number will get stuck in a loop originating with 1 or 89\n");
  printf("  This calculates how many numbers less then <limit> converge to 89\n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 10000000\n");
  return 0;
}

