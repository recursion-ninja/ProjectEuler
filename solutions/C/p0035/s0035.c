#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  BUFLEN  21 /* Safely holds UINT64_MAX */
#define  DECIMAL 10

/**
 * NOTES:
 * Seive primes up to limit
 * Check each prime for cirularity
 * Keep a tally of qualifying primes
 */

uint32_t isCircularPrime(const uint32_t index, const uint64_t * const primes, const uint32_t count);
void     rotateString   (char * const buffer);
void     gatherPrimes   (const uint32_t limit, uint64_t ** const arr, uint32_t * const count);
int      inList         (const uint64_t n, const uint64_t * const primes, const uint32_t  count);
void     print_help     (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit = 1000000; /* default */
  uint32_t i,tally,count;
  uint64_t *primes = NULL;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]), EXIT_SUCCESS; }
  if(argc > 1) {  sscanf(argv[1],"%u",&limit); }

  gatherPrimes(limit,&primes,&count);

  for(i=tally=0; i<count; ++i)
    if(isCircularPrime(i,primes,count))
      ++tally;

  printf("%u\n",tally);
  return EXIT_SUCCESS;
}

uint32_t isCircularPrime(const uint32_t index, const uint64_t * const primes, const uint32_t count) {
  static char *buffer = NULL;
  uint32_t i,length;
  uint64_t value;  

  if(buffer==NULL)
    buffer = malloc(BUFLEN * sizeof *buffer);

  if(primes[index] < 10)
    return 1;

  snprintf(buffer,BUFLEN,"%llu",primes[index]);
  length = strlen(buffer);
  for(i=1; i<length; ++i) {
    rotateString(buffer);
    sscanf(buffer,"%llu",&value);
    if(!inList(value,primes,count))
      return 0;
  }
  return 1;
}

void rotateString(char * const buffer) {
  uint32_t length = strlen(buffer);
  uint32_t i;

  for(i=length; i>0; --i)
    buffer[i] = buffer[i-1];

  buffer[0] = buffer[length];
  buffer[length] = '\0';
}

void gatherPrimes(const uint32_t limit, uint64_t ** const arr, uint32_t * const count) {
  uint32_t  i,j,size;
  uint8_t  *isPrime;

  size = 10;
  if(*arr!=NULL)
    free(*arr);
  *arr = malloc(size * sizeof **arr);
  *count = 0;

  isPrime = malloc((limit+1) * sizeof *isPrime);
  memset(isPrime,1,limit+1);

  for(i=2; i<=limit; ++i)
    if(isPrime[i])
      for(j=2*i; j<=limit; j+=i)
        isPrime[j] = 0;

  for(i=2; i<=limit; ++i) {
    if(isPrime[i]) {
      if(*count >= size)
        *arr = realloc(*arr, (size*=2) * sizeof **arr);
      (*arr)[(*count)++] = i;
    }
  }
  *arr = realloc(*arr, *count * sizeof **arr);
}

int inList(const uint64_t n, const uint64_t * const primes, const uint32_t count) {
  uint64_t low,hi,mid;
  low = 0;
  hi = count;
  while(low < hi) { /* binary search */
    mid = low/2 + hi/2;
    if(primes[mid] == n)
      return  1;
    if(primes[mid] >  n)
      hi  = mid;
    else
      low = mid + 1;
  }
  return 0;
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the numer of circular primes less then <limit>\n");
  printf("  Example: 197, 971 719 are all circular primes because\n");
  printf("  each number is prime and each possible roptation is also prime\n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 1000000\n");
}
