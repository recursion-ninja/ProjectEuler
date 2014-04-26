#include <gmp.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  PRECISION 4096
#define  DECIMAL_POINT 1
#define  NULL_TERMINATOR 1

/**
 * Problem expects you to count the digits BEFORE & AFTER the decimal point!
 */

void     gatherSquares(const uint32_t limit, uint32_t ** const arr, uint32_t * const count);
uint32_t inList(const uint32_t n, const uint32_t * const arr, const uint32_t count);
void     print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit  = 100; /* default */
  uint32_t digits = 100; /* default */
  uint32_t i,j,sum,bufLen,charCount,sqCount=10;
  uint32_t *squares;
  char     *buffer;
  mpf_t    irrational;

  if(argc > 1 && !strcmp(argv[1],"--help")) return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)    sscanf(argv[1],"%u",&limit );
  if(argc > 2)    sscanf(argv[2],"%u",&digits);

  mpf_set_default_prec(PRECISION);
  mpf_init(irrational);

  charCount = digits + DECIMAL_POINT;
  bufLen    = charCount + NULL_TERMINATOR;

  gatherSquares(limit,&squares,&sqCount);
  buffer  = malloc(bufLen * sizeof *buffer);

  for(i=sum=0; i<=limit; ++i) {
    if(inList(i,squares,sqCount))
      continue;
    mpf_sqrt_ui(irrational,i);
    gmp_snprintf(buffer,bufLen,"%.*Ff",charCount,irrational);
    for(j=0; j<digits+DECIMAL_POINT; ++j)
      if(buffer[j]!='.')
        sum += buffer[j]-'0';
  }
  printf("%u\n",sum);
  return EXIT_SUCCESS;
}

void gatherSquares(const uint32_t limit, uint32_t ** const arr, uint32_t * const count) {
  uint32_t i,sq,size;
  size=10;
  *count = 0;

  *arr   = malloc(size * sizeof **arr);
  for(i=0; (sq=i*i)<=limit; ++i) {
    if(*count >= size)
      *arr = realloc(*arr, (size*=2) * sizeof **arr);
    (*arr)[(*count)++] = sq;
  }
  *arr = realloc(*arr, *count * sizeof **arr);
}

uint32_t inList(const uint32_t n, const uint32_t * const arr, const uint32_t count) {
  uint32_t hi,mid,low;
  hi  = count; /* exclusive upper bound */
  low = 0;     /* inclusive lower bound */
  while(low < hi) {
    mid = low/2 + hi/2;
    if(arr[mid]==n)
      return 1;
    if(arr[mid] >n)
      hi  = mid;
    else
      low = mid + 1;
  }
  return 0;
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> <digits>\n",str);
  printf("  Calculates the digital sum of the first <digits> digits \n");
  printf("  of each irrational square root <= <limit>\n");
  printf("  * limit    : a decimal number\n");
  printf("             : default = 100\n");
  printf("  * digits   : a decimal number\n");
  printf("             : default = 100\n");
}
