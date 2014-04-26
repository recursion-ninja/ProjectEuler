#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  BUF_LEN 21

uint32_t isIncNumber(const uint64_t n);
uint32_t isDecNumber(const uint64_t n);
uint32_t isBncNumber(const uint64_t n);
uint32_t isOrdNumber(const uint64_t n, const uint32_t b);
uint32_t matchesRatio(const uint32_t num, const uint32_t den, const uint64_t lhs, const uint64_t rhs);
uint32_t gcd_bin(const uint32_t u, const uint32_t v);
uint32_t getDigit(const char c);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t i,bouncy,total;
  uint32_t num    = 99;  /* default */
  uint32_t den    = 100; /* fixed   */
  uint32_t gcd;
  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&num); }
  if(argc > 2) {  sscanf(argv[2],"%u",&den); }
  if(num  >= den)
    return print_help(argv[0]);

  gcd  = gcd_bin(num,den);
  num /= gcd;
  den /= gcd;

  for(i=total=100,bouncy=0; 1; ++i,++total) {
    if(isBncNumber(i))
      ++bouncy;
    if(matchesRatio(num,den,bouncy,total))
      break;
  }
  printf("%llu / %llu\n",bouncy,total);
  return 0;
}

uint32_t matchesRatio(const uint32_t num, const uint32_t den, const uint64_t lhs, const uint64_t rhs) {
  return num!=0 && den!=0 && lhs!=0 && rhs!=0 && (lhs*den) == (rhs*num);
/*  return lhs > 0 && rhs > 0 && !(lhs%num) && !(rhs%den) && lhs*den == rhs*num; */
}
uint32_t getDigit(const char c)        { return c-48; }
uint32_t isBncNumber(const uint64_t n) { return !isIncNumber(n) && !isDecNumber(n); }
uint32_t isIncNumber(const uint64_t n) { return isOrdNumber(n,1); }
uint32_t isDecNumber(const uint64_t n) { return isOrdNumber(n,0); }

uint32_t isOrdNumber(const uint64_t n, const uint32_t b) {
  uint32_t i,ordered,len;
  char     *str = (char*) malloc(BUF_LEN * sizeof(char));
  snprintf(str,BUF_LEN,"%llu",n);
  len = strlen(str);
  for(i=ordered=1; ordered && i<len; ++i)
    ordered = ordered && b ? getDigit(str[i-1]) <= getDigit(str[i]) : getDigit(str[i-1]) >= getDigit(str[i]);
  free(str);
  return ordered;
}
/*
  char str[65];
  snprintf(str,n,"%llu",n);
  uint32_t len = strlen(str);
  uint32_t i;
  uint32_t inc = 1;
  for(i=1; inc && i<len; ++i)
    inc &= getDigit(str[i-1]) < getDigit(str[i]);
  return inc;
}
*/

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
  printf("  Usage: %s <sum> \n",str);
  printf("  Calculates the pythagorean tripples\n");
  printf("  which a^2 + b^2 = c^2 and a+b+c = sum\n");
  printf("  * sum : a decimal number\n");
  printf("        : default = 1000\n");
  return 0;
}

