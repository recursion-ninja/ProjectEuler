#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * Use algrbra to reduce:
 * ( a*(a+1)*(2a+1)/6 - b*(b+1)*(2b+1)/6 ) - ( a*(a+1)/2 - b*(b+1)/2 )^2
 * To:
 * ( a*a*(a+1)*(a+1) - 2*a*b*(a+1)*(b+1) + b*b*(b+1)*(b+1) )/4 - ( a*(a+1)*(2*a+1) - b*(b+1)*(2*b+1) )/6
 * For 0 <= b < a
 */
void print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t upper  = 100; /* default */
  uint32_t lower  = 0;
  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&upper);
  if(argc > 2)
    sscanf(argv[2],"%u",&lower);

  uint64_t b = (uint64_t)lower; /* lower bound (inclusive) */
  uint64_t a = (uint64_t)upper; /* upper bound (inclusive) */
  uint64_t result = ( a*a*(a+1)*(a+1) - 2*a*b*(a+1)*(b+1) + b*b*(b+1)*(b+1) )/4 - ( a*(a+1)*(2*a+1) - b*(b+1)*(2*b+1) )/6 ;

  printf("%llu\n",result);
  return EXIT_SUCCESS;
}

void print_help(const char * const str) {
  printf("  Usage: %s <upper> <lower> \n",str);
  printf("  Calculates the difference between\n");
  printf("  The square of the sum and\n");
  printf("  the sum of the squares\n");
  printf("  of the natural numbers in the range\n");
  printf("  [lower, upper] (inclusively)\n");
  printf("  * upper : a decimal number inclusive bound\n");
  printf("          : default = 100\n");
  printf("  * lower : a decimal number inclusive bound\n");
  printf("          : default = 0\n");
}
