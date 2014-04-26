#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  START_SIZE 10

/**
 * NOTES:
 * Simply precompute primes less then sqrt of limit,
 * then precompute squares, cubes and forths of primes less then limit
 * then brute force the combination search space,
 * save sums to avoid double counting!
 * runs in O(n^{13/12})
 */

void     gatherPrimes     (const uint32_t limit, uint32_t ** const arr, uint32_t * const count);
void     gatherPrimePowers(const uint32_t limit, const uint32_t power, uint32_t ** const arr, uint32_t * const count, const uint32_t * const primes, const uint32_t primeCount);
void     addToList        (const uint32_t x, uint32_t ** const arr, uint32_t * const count, uint32_t * const size);
void     disposeOfList    (uint32_t ** const arr, uint32_t * const count);
void     mergeSortUniq    (uint32_t * const list, uint32_t * const count);
void     print_help       (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit = 50000000; /* default */
  uint32_t pCount,sCount,cCount,fCount,lCount,size;
  uint32_t i,j,k,sum1,sum2,sum3;
  uint32_t *primes,*squares,*cubes,*fourths,*list;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%u",&limit);

  primes=squares=cubes=fourths=NULL;

  gatherPrimes(floor(sqrt(limit)),&primes,&pCount);
  gatherPrimePowers(limit,2,&squares,&sCount,primes,pCount);
  gatherPrimePowers(limit,3,&cubes,  &cCount,primes,pCount);
  gatherPrimePowers(limit,4,&fourths,&fCount,primes,pCount);

  disposeOfList(&primes,&pCount);

  lCount=0;
  size  = START_SIZE;
  list  = malloc(size * sizeof *list);


  for(i=0; i<fCount && (sum1 = fourths[i]) < limit; ++i)
    for(j=0; j<cCount && (sum2 = sum1 + cubes[j]) < limit; ++j)
      for(k=0; k<sCount && (sum3 = sum2 + squares[k]) < limit; ++k)
        addToList(sum3,&list,&lCount,&size);

  disposeOfList(&squares,&sCount);
  disposeOfList(&cubes,  &cCount);
  disposeOfList(&fourths,&fCount);

  mergeSortUniq(list,&lCount);

  printf("%u\n",lCount);
  return EXIT_SUCCESS;
}

void gatherPrimes(const uint32_t limit, uint32_t ** const arr, uint32_t * const count) {
  uint32_t  i,j,size;
  uint8_t  *isPrime;

  size = START_SIZE;
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

void gatherPrimePowers(const uint32_t limit, const uint32_t power, uint32_t ** const arr, uint32_t * const count, const uint32_t * const primes, const uint32_t primeCount) {
  uint32_t i,j,size,prod;

  size = START_SIZE;
  if(*arr!=NULL)
    free(*arr);
  *arr = malloc(size * sizeof **arr);
  *count = 0;

  for(i=0; i<primeCount; ++i) {
    for(j=0,prod=1; j<power; ++j)
      prod *= primes[i];
    if(prod >= limit)
      break;
    addToList(prod,arr,count,&size);
  }

  *arr = realloc(*arr, *count * sizeof **arr);
}

void addToList(const uint32_t x, uint32_t ** const arr, uint32_t * const count, uint32_t * const size) {
  if(*count >= *size)
    *arr = realloc(*arr, (*size*=2) * sizeof **arr);
  (*arr)[(*count)++] = x;
}

void disposeOfList(uint32_t ** const arr, uint32_t * count) {
  if(*arr != NULL)
    free(*arr);
  *arr   = NULL;
  *count = 0;
}

void mergeSortUniq(uint32_t * const list, uint32_t * const count) {
  uint32_t  *arrA,*arrB;
  uint32_t  a = *count / 2;
  uint32_t  b = *count - a;
  uint32_t  i,x,y;

  if(*count < 2)
    return;

  arrA = malloc(a * sizeof *list);
  arrB = malloc(b * sizeof *list);

  for(i=0; i<a; ++i)
    arrA[i] = list[i];
  for(i=0; i<b; ++i)
    arrB[i] = list[i+a];

  /* values of a & b may change when removing dupicates */
  mergeSortUniq(arrA,&a);
  mergeSortUniq(arrB,&b);

  for(i=x=y=0; i<*count && x<a && y<b; ++i) {
    if(arrA[x] >  arrB[y])
      list[i] =  arrB[y++];
    else if(arrA[x] <  arrB[y])
      list[i] =  arrA[x++];
    else /* arrA[x] == arrB[y] */
      list[i] =  arrA[x++], ++y;
  }
  while(i<*count && x<a)
    list[i++] = arrA[x++];
  while(i<*count && y<b)
    list[i++] = arrB[y++];

  *count = i;

  free(arrA);
  free(arrB);
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the quantity of numbers x of the form x = a^2 + b^3 + c^4\n");
  printf("  where x <=  <limit> && a,b,c are prime\n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 50000000\n");
}
