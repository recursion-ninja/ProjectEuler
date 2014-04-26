#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint64_t largestPrimeFactor_naive(uint64_t n);
int print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t composite   = 600851475143ULL; /* default */
  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%llu",&composite); }
  printf("%llu\n",largestPrimeFactor_naive(composite));
  return 0;
}

uint64_t largestPrimeFactor_naive(uint64_t n) {
  uint64_t i=1;
  while(++i < n)
    while(!(n%i))
      n /= i;
  return n;
}

int print_help(const char * const str) {
  printf("  Usage: %s <composite> \n",str);
  printf("  Calculates largest prime factor of composite\n");
  printf("  * composite : a decimal composite number \n");
  printf("              : default = 600,851,475,143\n");
  return 0;
}
