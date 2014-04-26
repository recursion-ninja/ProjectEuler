#include <stdio.h>
#include <stdint.h>
#include <string.h>

uint64_t getPent(uint32_t n);
uint32_t  isPent(uint64_t n);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t m,n,s,d,l,x,y,z;
  uint32_t a=1,b,c,i,j;
  x=y=z=0-1;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }

  for(i=5,l=0; a; ++i,l=m) {
    m = getPent(i);
    if(m-l > z) /* break when minimum found */
      break;
    for(j=i-1; j>0; --j) {
      n = getPent(j);
      s = m + n;
      d = m - n;
      b = isPent(s);
      c = isPent(d);
      if(b && c) {
        a = 0;
        x = m;
        y = n;
        z = d; /* difference to be minimized */
      }
    }
  }
  /* print result */
  printf("%llu - %llu = %llu\n",x,y,z);
  return 0;
}

uint64_t getPent(uint32_t n) {
  return (n*(3*n-1))/2;
}

uint32_t isPent(uint64_t n) {
  uint64_t hi,low,mid,val;
  /* set bounds */
  for(hi=2; (val = getPent(hi)) <= n; hi*=2);
  low = hi/2;
  /* binary search */
  while(low < hi) {
    mid = low/2 + hi/2;
    val = getPent(mid);
    if(val == n)
      return  1;
    if(val >  n)
      hi  = mid;
    if(val <  n)
      low = mid+1;
  }
  return 0;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s \n",str);
  printf("  Finds the pair of pentagonal numbers, Pj and Pk, \n");
  printf("  for which their sum and difference are pentagonal \n");
  printf("  and D = |Pk − Pj| is minimised \n");
  return 0;
}
