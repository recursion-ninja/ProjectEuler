#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

void     sieveOfEratosthenes_m2(const uint32_t limit, uint32_t ** const primes, uint32_t * const count);
void     print_help(const char * const str);

int  main(int argc, char* argv[]) {
  uint32_t limit   = 2000000; /* default */
  uint32_t *primes = NULL;
  uint32_t count;
  uint64_t sum;

  if(argc  > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc  > 1)
    sscanf(argv[1],"%u",&limit);

  sieveOfEratosthenes_m2(limit-1,&primes,&count);
  for(uint32_t i=sum=0; i< count; ++i)
    sum += primes[i];

  printf("%llu\n",sum);
  free(primes);
  return EXIT_SUCCESS;
}

/* Less Then Or Equal */
void sieveOfEratosthenes_m2(const uint32_t limit, uint32_t ** const primes, uint32_t * const count) {
  uint32_t i,j,size;
  uint8_t *bitSet = calloc(limit/16 +1, sizeof *bitSet);

/* Use bit shifting to use 1/16 memory  */
/* And improve cache performance        */
/* Only check odds                      */
/* Indexing:  x/2                       */
/* SetBit:    arr[n/8] |= 0x80 >> (n%8) */
/* GetBit:    arr[n/8]  & 0x80 >> (n%8) */
#define P_INDEX(x)     (x>>1)
#define SET_BIT(x,arr) (arr[(x>>3)] |= 0x80 >> (x%8))
#define GET_BIT(x,arr) (arr[(x>>3)]  & 0x80 >> (x%8))

  if(*primes != NULL)
    free(*primes);
  size    = 10;
  *primes = malloc(size * sizeof **primes);
  *count  = 0;

  if(limit < 2)
    return;

  for(i=3; i<=limit; i+=2)
    if(!GET_BIT(P_INDEX(i),bitSet))
      for(j=3*i; j<=limit; j+=2*i)
        SET_BIT(P_INDEX(j),bitSet);

  (*primes)[(*count)++] = 2;

  for(i=3; i<=limit; i+=2) {
    if(!GET_BIT(P_INDEX(i),bitSet)) {
      if(*count >= size)
        *primes = realloc(*primes, (size*=2) * sizeof **primes);
      (*primes)[(*count)++] = i;
    }
  }
  *primes = realloc(*primes, *count * sizeof **primes);
  free(bitSet);
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the sum of all primes \n");
  printf("  which are less then the limit \n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 2000000\n");
}
