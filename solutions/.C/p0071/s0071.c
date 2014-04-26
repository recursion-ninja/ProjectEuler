#include <stdio.h>
#include <stdint.h>
#include <string.h>

uint32_t gcd(const uint32_t a, const uint32_t b);
uint32_t print_help(const char* str);

int main(int argc, char* argv[]) {
  uint64_t z=1000000; /* default */
  uint64_t b;         /* biggest numerator found still left of target */
  uint64_t p,q;       /* closest numerator and denominator to target */
  uint64_t n,d;       /* running numerator and denominator */
  uint64_t x=3,y=7;   /* target fraction */

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%llu",&z); }
  if(argc > 2) {  sscanf(argv[2],"%llu",&x); }
  if(argc > 3) {  sscanf(argv[3],"%llu",&y); }

  for(d=2,b=0,p=x,q=y; d<=z; ++d) {
    for(n=b+1; n<d; ++n) {
      if(gcd(n,d) == 1) {
        if(n*y < d*x) { b = n; }
        else          { break; }
      }
    }
    if(b*q > d*p || b==0) { p=b; q=d; }
  }
  printf("%llu/%llu\n",p,q);
  return 0;
}

uint32_t gcd(const uint32_t a, const uint32_t b) { return b==0 ? a : gcd(b, a%b); }

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> <numer> <denom>\n",str);
  printf("  Calculates the reduced proper fraction \n");
  printf("  which is the immedate predecessor of <numer>/<denom> \n");
  printf("  in the linearly ordered set of all reduced proper fractions \n");
  printf("  with denominators <= <limit> \n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 1000000\n");
  printf("  * numer : a decimal number\n");
  printf("          : default = 3\n");
  printf("  * denom : a decimal number\n");
  printf("          : default = 7\n");
  return 0;
}
