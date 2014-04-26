#include <stdio.h>
#include <stdint.h>
#include <string.h>

int print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t sum     = 1000; /* default */
  uint32_t a,b,c;
  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&sum); }

  for(c=sum-3; c>2; --c)
    for(b=sum-c-1, a=sum-c-b; b>a; --b,++a)
        if(a*a + b*b == c*c)
          printf("%u^2 + %u^2 = %u^2; a*b*c = %u\n",a,b,c,a*b*c);
  return 0;
}

int print_help(const char * const str) {
  printf("  Usage: %s <sum> \n",str);
  printf("  Calculates the pythagorean tripples\n");
  printf("  which a^2 + b^2 = c^2 and a+b+c = sum\n");
  printf("  * sum : a decimal number\n");
  printf("        : default = 1000\n");
  return 0;
}
