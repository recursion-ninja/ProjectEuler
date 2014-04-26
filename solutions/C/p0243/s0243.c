#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
/**
 * NOTES:
 * denominator is of the following:
 * 2*3*5*7*11*13....P*C
 * where P is the greatest prime factor
 * and C is some composite number or 1
 */
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t x=15499,y=94744; /* default */
  const uint64_t count    = 11;
  const uint64_t primes[] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31};
  uint64_t i,j,b,n,d,numer,denom;

  if(argc > 1 && !strcmp(argv[1],"--help"))  { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%llu",&x); }
  if(argc > 2) {  sscanf(argv[2],"%llu",&y); }

  for(i=0,n=d=numer=denom=b=1; b && i<count; ++i) {
    numer *= primes[i]-1;
    denom *= primes[i];
    if(numer*y < denom*x) {
      for(j=1; b && j<primes[i]; ++j) {
        n = numer*j;
        d = denom*j;
        if(n*y < (d-1)*x)
          b=0;
      }
    }
  }
  printf("%llu/%llu : %llu\n",n,d-1,d);
  return 0;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <numer> <denom>\n",str);
  printf("  Calculates the value of the smallest denominator D \n");
  printf("  which has a ratio R(D) of reduced proper fractions \n");
  printf("  to all fractions with denominator D \n");
  printf("  where R(D) < <numer>/<denom> \n");
  printf("  * numer : a decimal number\n");
  printf("          : default = 15499\n");
  printf("  * denom : a decimal number\n");
  printf("          : default = 94744\n");
  return 0;
}
