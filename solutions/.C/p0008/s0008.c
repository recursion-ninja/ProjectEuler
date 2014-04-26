#include <stdio.h>
#include <ctype.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint32_t getDigit(const uint32_t c);
int print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t biggest = 0;
  uint64_t product = 0;
  uint64_t count   = 0;
  uint32_t digits  = 5; /* default */
  uint32_t index   = 0;
  uint32_t i;
  char c;
  uint64_t *buffer;
  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&digits); }
  buffer = (uint64_t*) malloc(digits * sizeof(uint64_t));

  while((c=fgetc(stdin)) != EOF) {
    /* Ignore non-digit characters */
    if(!isdigit(c))
      continue;
    /* Add new digit to buffer overwitting oldest digit in buffer */
    buffer[index++] = getDigit(c);
    index %= digits;
    count++;
    /* Do not calculate until the buffer has been filled */
    if(count < digits)
      continue;
    /* Calculate buffer and compare result */
    product = 1;
    for(i=0; i<digits; ++i)
      product *= buffer[i];
    if(product > biggest)
      biggest = product;
  }
  if(count == 0)
    printf("No digits passed to stdin\n");
  if(count < digits)
    printf("Too few digits to fill buffer\n");
  printf("product: %llu index: %llu\n",biggest,count-digits);
  return 0;
}

uint32_t getDigit(const uint32_t c) { return c-48; }

int print_help(const char * const str) {
  printf("  Usage: %s <digits> \n",str);
  printf("  Calculates the value of the largest product\n");
  printf("  of consecutive <digits> numbers read from stdin\n");
  printf("  * digits : a decimal number\n");
  printf("           : default = 5\n");
  return 0;
}
