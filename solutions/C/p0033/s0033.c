#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t gcd(const uint32_t a, const uint32_t b);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t digits = 2;
  uint32_t i,j,k,n,d,x,y,limit,start,numer,denom,div_a,div_b,match;
  char     *str_num,*str_den;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&digits); }

  str_num = (char*) malloc((digits+1) * sizeof(char));
  str_den = (char*) malloc((digits+1) * sizeof(char));

  for(i=0,limit=1; i<digits; ++i,limit*=10);
  for(i=1,start=1; i<digits; ++i,start*=10);

  for(denom= ++start,x=y=1; denom<limit; ++denom) {
    if(denom%10==0)
      continue;
    for(numer=start; numer<limit; ++numer) {
      if(numer%10==0)
        continue;
      if(numer >= denom)
        break;
      div_a = gcd(numer,denom);
      if(div_a == 1)
        continue;
      /* else ratio exists */
      snprintf(str_num,digits+1,"%u",numer);
      snprintf(str_den,digits+1,"%u",denom);
      for(i=0,match=0; !match && i<digits; ++i)
        for(j=0; !match && j<digits; ++j)
          if(str_num[i] == str_den[j])
            match = 1;
      if(!match)
        continue;
      /* else matching digits exist */
      /* remove matching digits     */
      for(k= i; k<digits+1; ++k)
        str_num[k-1] = str_num[k];
      for(k= j; k<digits+1; ++k)
        str_den[k-1] = str_den[k];
      sscanf(str_num,"%u",&n);
      sscanf(str_den,"%u",&d);
      div_b = gcd(n,d);
      if(numer/div_a == n/div_b && denom/div_a == d/div_b) {
        printf("%u/%u : %u/%u\n",numer,denom,n,d);
        x *= numer;
        y *= denom;
      }
    }
  }
  while((div_a = gcd(x,y))>1) {
    x/=div_a;
    y/=div_a;
  }
  printf("%u/%u\n",x,y);
  return 0;
}

uint32_t gcd(const uint32_t a, const uint32_t b) { return (a == 0) ? b : gcd(b%a,a); }

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <digits> \n",str);
  printf("  Calculates the fractions with <digits> in the numberator \n");
  printf("  and <digits> in the denominator for which \n");
  printf("  incorrect reduction technique produces valid result\n\n");
  printf("  * digits : a decimal number\n");
  printf("           : default = 2\n");
  return 0;
}
