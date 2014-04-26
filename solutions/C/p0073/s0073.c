#include <stdio.h>
#include <stdint.h>
#include <string.h>
/**
 * REMEMBER:
 * a/b < c/d iff a*d < c*b
 */
uint32_t gcd_bin(const uint32_t u, const uint32_t v);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t limit=12000; /* default limit */
  uint32_t count;
  uint32_t b;           /* boolean control */
  uint64_t q;           /* previous loop's lowest numerator    */
  uint64_t n,d;         /* current numerator & denominator     */
  uint64_t w=1,x=3;     /* lower bound numerator & denominator */
  uint64_t y=1,z=2;     /* upper bound numerator & denominator */

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%llu",&limit); }
  if(argc > 2) {  sscanf(argv[2],"%llu",&w); }
  if(argc > 3) {  sscanf(argv[3],"%llu",&x); }
  if(argc > 4) {  sscanf(argv[4],"%llu",&y); }
  if(argc > 5) {  sscanf(argv[5],"%llu",&z); }

  for(count=0,q=1,d=2; d<=limit; ++d) {
    for(b=0,n=q; n<d; ++n) {            /* begin just outside lower bound */
      if(!b && n*x > d*w) { q=n; b=1; } /* within lower bound */
      if( b && gcd_bin(n,d) == 1) {
        if(n*x > d*w && n*z < d*y)
          ++count;
        else
          break;  /* exceeded upper bound */
      }
    }
  }
  printf("%u\n",count);
  return 0;
}

uint32_t gcd_bin(const uint32_t u, const uint32_t v) {
    /* simple cases (termination) */
    if(u == v)  return u;
    if(u == 0)  return v;
    if(v == 0)  return u;
    /* look for even numbers  */
    if( ~u & 1) {
      if(v & 1) return gcd_bin(u >> 1, v);           /* u is even, v is odd  */
      else      return gcd_bin(u >> 1, v >> 1) << 1; /* u is even, v is even */
    }
    if( ~v & 1) return gcd_bin(u, v >> 1);           /* u is odd,  v is even */
    /* reduce larger argument */                     /* u is odd,  v is odd  */
    return (u > v) ? gcd_bin((u - v) >> 1, v)
                   : gcd_bin((v - u) >> 1, u);
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> <lo numer> <lo denom> <hi numer> <hi denom>\n",str);
  printf("  Calculates the number of reduced proper fraction \n");
  printf("  in the linearly ordered set of all reduced proper fractions \n");
  printf("  with denominators <= <limit> \n");
  printf("  and which are greater then <lo numer>/<lo denom> \n");
  printf("  and which are less    then <hi numer>/<hi denom> \n");
  printf("  Note the bounds are exclusive. \n");
  printf("  * limit    : a decimal number\n");
  printf("             : default = 120000\n");
  printf("  * lo numer : a decimal number\n");
  printf("             : default = 1\n");
  printf("  * lo denom : a decimal number\n");
  printf("             : default = 3\n");
  printf("  * hi numer : a decimal number\n");
  printf("             : default = 1\n");
  printf("  * hi denom : a decimal number\n");
  printf("             : default = 2\n");
  return 0;
}
