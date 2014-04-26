#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define BUFFER 21 //Safely hold string representation of uint64

int isPalindrome_num(const uint64_t n);
int isPalindrome_str(const char * const str );
int print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t digits_multiplicand=3, /* default */
           digits_multiplier  =3; /* default */
  uint64_t ubound_multiplicand=1,
           ubound_multiplier  =1,
           large = 0,
           i,j;
  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%u",&digits_multiplicand); }
  if(argc > 2) { sscanf(argv[2],"%u",&digits_multiplier  ); }
  for(i=0; i<digits_multiplicand; ++i) ubound_multiplicand *= 10;
  for(i=0; i<digits_multiplier;   ++i) ubound_multiplier   *= 10;
  for(i=0; i<ubound_multiplicand; ++i)
    for(j=i; j<ubound_multiplier; ++j)
      if(isPalindrome_num(i*j))
        large = (i*j > large) ? i*j : large;
  printf("%llu\n",large);
  return 0;
}

int isPalindrome_num(const uint64_t n) {
  char str[BUFFER];
  snprintf(str,BUFFER,"%llu",n);
  return isPalindrome_str(str);
}

int isPalindrome_str(const char * const str) {
  uint32_t end   = strlen(str);
  uint32_t isPal = 1;
  uint32_t i;
  uint32_t limit = end/2 ;
  for(i=0; isPal && i<limit; ++i)
    isPal = (str[i] == str[end-i-1]);
  return isPal;
}

int print_help(const char * const str) {
  printf("  Usage: %s <f_digits> <l_digits>\n",str);
  printf("  Calculates largest palindromic number\n");
  printf("  which is the product of two numbers\n");
  printf("  who's multiplicand has f_digits and\n");
  printf("  who's multiplier   has l_digits\n");
  printf("  * f_digits : a decimal number \n");
  printf("             : default = 3\n");
  printf("  * l_digits : a decimal number \n");
  printf("             : default = 3\n");
  return 0;
}
