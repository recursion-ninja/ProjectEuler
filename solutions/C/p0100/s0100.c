#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * Note the following:
 * P(BB) = 1/2 ==> n*(n-1)/k*(k-1) = 1/2
 * The following holds:
 * n*(n-1)/k*(k-1) = 1/2
 * n*(n-1) = k*(k-1)/2
 * n^2 - n = (k^2 - k)/2
 * n^2 - n + 1/4 = (k^2 - k)/2 + 1/4
 * n^2 - n + 1/4 = (k^2 - k + 1/4)/2 + 1/8
 * (n - 1/2)^2 = ( k - 1/2)^2/2 + 1/8
 * n - 1/2 = sqrt( ( k - 1/2)^2/2 + 1/8)
 * n = 1/2 + sqrt( ( k - 1/2)^2/2 + 1/8 )
 * n = 1/2 + sqrt( 4*( k - 1/2)^2/8 + 1/8 )
 * n = 1/2 + sqrt( (4*( k - 1/2)^2 + 1)/8 )
 * n = 1/2 + sqrt( (8*( k - 1/2)^2 + 2)/16 )
 * n = 1/2 + sqrt( (8*( k - 1/2)^2 + 2) )/4
 * n = ( 2 + sqrt( 8*( k - 1/2)^2 + 2) ) )/4
 * n = ( 2 + sqrt( 8*k^2 - 8*k + 8*1/4 + 2) ) )/4
 * n = ( 2 + sqrt( 8*k^2 - 8*k + 2 + 2) ) )/4
 * n = ( 2 + sqrt( 8*k^2 - 8*k + 4) ) )/4
 * n = ( 2 + sqrt( 4*(2*k^2 - 2k + 1) )/4
 * n = ( 2 + sqrt( 4*(k^2 + k*k - 2k + 1) )/4
 * n = ( 2 + sqrt( 4*(k^2 + (k-1)^2) )/4
 * n = ( 2 + 2*sqrt( k^2 + (k-1)^2 )/4
 * n = ( 1 + sqrt( k^2 + (k-1)^2 )/2
 * ==> k and k-1 are pythagoreath triples
 * Holy crap that is gonna reduce the search space!
 * Find Pythagorean triples (a,b,c) ST a<b<c & b=a+1
 * Then:
 * a = k-1, b = k, c = 2n - 1
 * Use the following recurrance equation to generate triples:
 * s_0 = 7, t_0 = 5 : (3,4,5)
 * s_x+1 = 3*s_x + 4*t_x
 * t_x+1 = 2*s_x + 3*t_x
 * a_x+1 = (s-1)/2
 * b_x+1 = (s-1)/2 +1
 * c_x+1 = t_x+1
 */

void getNextConstrainedPythagoreanTriple(uint64_t * const a, uint64_t * const b, uint64_t * const c);
void print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t lower      = 1000000000000; /* default */
  uint64_t k,n,a,b,c;

  if(argc > 1&& !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%llu",&lower);

  for(a=b=c=n=k=0; k<lower; getNextConstrainedPythagoreanTriple(&a,&b,&c),k=b);

  n = (c+1)/2;
  printf("%llu : %llu\n",n,k);
  return EXIT_SUCCESS;
}

void getNextConstrainedPythagoreanTriple(uint64_t * const a, uint64_t * const b, uint64_t * const c) {
  static uint64_t s = 7;
  static uint64_t t = 5;
  uint64_t x,y;

  *a = (s-1)/2;
  *b = (s-1)/2 + 1;
  *c = t;

  x = 3*s + 4*t;
  y = 2*s + 3*t;
  s = x;
  t = y;
}

void print_help(const char * const str) {
  printf("  Usage: %s <lower>\n",str);
  printf("  Calculates the least values n,k\n");
  printf("  Where n is the number of marked objects\n");
  printf("  Where k is the total number of objects\n");
  printf("  Where the probability of randomly choosing two marked objects is 1/2 (50%%)\n");
  printf("  And the total number of objects is greater then or equal to <lower>\n");
  printf("  * lower    : a decimal number exclusive bound\n");
  printf("             : default = 1000000000000\n");
}
