#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  DEFICEINT 0x000BCDEF
#define  ABUNDANT  0xFEDCB000
#define  PERFECT   0x00088000

void     addToSet  (uint32_t  ** const arr, uint32_t * const size, uint32_t * const count, const uint32_t num);
void     expand_arr(uint32_t  ** const arr, uint32_t * const size);
uint32_t print_help(const char * const str);

/**
 * If the prime factors on a number N are A^a * b^b * c^c
 * Then the sum of N's divisor's are (A^0+...+A^a)(B^0+...+B^b)(C^0+...+C^c)
 * And  the sum of N's PROPER divisor's are (A^0+...+A^a)(B^0+...+B^b)(C^0+...+C^c) - N
 * Example:
 * Prime Factors: 120 = 2^3 * 3^1 * 5^1
 * Propper Divisor Sum: (1+2+2^2+2^3)(1+3)(1+5)-120 = 15⋅4⋅6-120 = 240
 */

int main(int argc, char* argv[]) {
  uint32_t limit = 28123; /* Default */
  uint32_t i,j,k,l,n,total,pow,divsum,expsum;
  uint32_t size_p=10,count_p;
  uint32_t size_d=10,count_d;
  uint32_t size_e=10,count_e;
  uint32_t *list_primes = (uint32_t*) malloc(size_p * sizeof(uint32_t));
  uint32_t *list_divs   = (uint32_t*) malloc(size_d * sizeof(uint32_t));
  uint32_t *list_exps   = (uint32_t*) malloc(size_d * sizeof(uint32_t));
  uint32_t *seive; /* 0: cannot be written as sum of two abundant numbers, 1: can be */
  uint32_t *memo;  /* Stores number data <deficient|perfect|abundant> */
  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%u",&limit);   }

  memo  = (uint32_t*) malloc(limit * sizeof(uint32_t));
  seive = (uint32_t*) calloc(limit , sizeof(uint32_t));
  memo[1] = 0;

  for(count_p=0,i=2; i<limit; ++i) {
    for(count_d=count_e=j=0,n=i; n>1 && j<count_p; ++j) {
      for(k=0; !(n%list_primes[j]); ++k)
        n/=list_primes[j];
      if(k) {
        addToSet(&list_divs,&size_d,&count_d,list_primes[j]);
        addToSet(&list_exps,&size_e,&count_e,k);
      }
    }
    if(count_d==0) {
      addToSet(&list_primes,&size_p,&count_p,i);
      addToSet(&list_divs  ,&size_d,&count_d,i);
      addToSet(&list_exps  ,&size_e,&count_e,1);
    }
    for(j=0,divsum=1; j<count_d; ++j) {         /* foreach prime factor */
      for(k=0,expsum=0; k<=list_exps[j]; ++k) { /* foreach exp */
        for(l=0,pow=1; l<k; ++l)                /* raise to power */
          pow *= list_divs[j];
        expsum += pow;                          /* sum up exponentiations */
      }
      divsum *= expsum;                         /* multiply sums of exponentions */
    }
    divsum -= i;                                /* subtract base number */
    memo[i] = divsum <= i ? divsum != i ? DEFICEINT : PERFECT : ABUNDANT;
    if(memo[i]==ABUNDANT)
      for(j=1; j<=i; ++j)
        if(memo[j]==ABUNDANT && i+j < limit)
          seive[i+j] = 1;
  }
  for(i=1,total=0; i<limit; ++i)
    if(!seive[i])
      total += i;

  printf("%u\n",total);
  return 0;
}

void addToSet  (uint32_t  ** const arr, uint32_t * const size, uint32_t * const count, const uint32_t num) {
/*void     addToSet(uint32_t   **arr, uint32_t *size, uint32_t *count, uint32_t num) {*/
  if(*count>=*size)
    expand_arr(arr,size);
  (*arr)[(*count)++] = num;
}

void expand_arr(uint32_t ** const arr, uint32_t * const size) {
  *size *= 2;
  *arr   = (uint32_t*) realloc(*arr, *size * sizeof(uint32_t));
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the sum of all positive integers less then <limit> \n");
  printf("  which cannot be written as the sum of two abundant numbers. \n");
  printf("  Number is Deficient iff d(n) < n \n");
  printf("  Number is Abundant  iff d(n) > n \n");
  printf("  Number is Perfect   iff d(n) = n \n");
  printf("  Where d(n) = sum of all propper divisors of n \n");
  printf("  * limit  : a decimal number\n");
  printf("           : default = 28123\n");
  return 0;
}
