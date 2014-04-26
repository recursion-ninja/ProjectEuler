#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t  digitPowers(const uint32_t power, const uint32_t base);
void      print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t base = 10; /* default */
  uint32_t i,count,numbers;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&base);

  for(i=1,count=0; (numbers=digitPowers(i,base))!=0; ++i)
    count += numbers;

  printf("%u\n",count);
  return EXIT_SUCCESS;
}

uint32_t digitPowers(const uint32_t power,const uint32_t base) {
  uint64_t i,j,prod,count,lower,upper;

  for(i=0,upper=1; i<power; ++i)
    upper *= base;

  lower = upper / base;

  for(i=count=prod=0; prod < upper; ++i) {
    for(j=0,prod=1; j<power; ++j)
      prod *= i;
    if(lower <= prod && prod < upper)
      ++count;
  }

  return count;
}

void  print_help(const char * const str) {
  printf("  Usage: %s <base> \n",str);
  printf("  Calculates the total quantity of numbers\n");
  printf("  Comprised of 'X' digits in base <base>\n");
  printf("  Which are also a perfect power of degree 'X'\n");
  printf("  * base    : a decimal number\n");
  printf("            : default = 10\n");
}
