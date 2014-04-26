#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
/**
 * NOTES:
 * Euler's totient : phi(n)
 * Resilience      : R(n)
 * R(n) == phi(n)/n-1
 * use running prime list,dynamic memory allocation
 */
uint64_t phi       (uint64_t n, uint64_t **primes, uint32_t *count, uint32_t *size);
uint64_t gcd_bin   (uint64_t u, uint64_t v);
uint32_t isPrime   (uint64_t n, uint64_t **primes, uint32_t *count, uint32_t *size);
void     addPrime  (uint64_t n, uint64_t **primes, uint32_t *count, uint32_t *size);
uint32_t isInArr   (uint64_t n, uint64_t **primes, uint32_t count);
void     print_arr (uint64_t  *arr, uint32_t count);
uint32_t print_help(char* str);

int main(int argc, char* argv[]) {
  uint64_t x=15499,y=94744; //default
  uint64_t count=0,size=10; //default
  uint64_t i,n;
  uint64_t *primes = malloc(size * sizeof(uint64_t));

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%llu",&x); }
  if(argc > 2) {  sscanf(argv[2],"%llu",&y); }

  uint64_t a=x,b=y;

//  for(i=2,count=0; i<1000000; ++i)
//    printf("i: %llu\t%llu\n",i,phi(i,&primes,&count,&size));
/**/
  for(i=2,count=0; 1; ++i) {
    n = phi(i,&primes,&count,&size);
    if(!(i%1000000))
      printf("%llu/%llu %llu/%llu\n",n,i,n*b,(i-1)*a);
    if(n*b < (i-1)*a)
      break;
  }
/**/
  --i;
  printf("%llu < %llu\n",n*b,(i-1)*a);
  printf("%llu/%llu < %llu/%llu\n",n,i,x,y);
  return 0;
}

uint64_t phi(uint64_t n, uint64_t **primes, uint32_t *count, uint32_t *size) {
  uint64_t m,o,i,d,bound;
  // Base case
  if(n < 2)
    return 0;
  // Is Prime? (Lehmer's conjecture)
  if(isPrime(n,primes,count,size))
    return n-1;
  // Even number?
  if(!(n & 1)) {
    int m = n >> 1;
    return !(m & 1) ? phi(m,primes,count,size)<<1 : phi(m,primes,count,size);
  }
  // Find (smallest) prime factor using list of primes
  for(i=0,bound=(uint64_t)sqrt(n)+1; i<*count && (*primes)[i]<=bound && (n%(*primes)[i])!=0; ++i);
  m = (*primes)[i];
  o = n/m;
  d = gcd_bin(m, o);
  // Use phi()'s multiplicative property recursively
  //MEMOTIZATION HERE
  return d==1 ?  phi(m,primes,count,size)*phi(o,primes,count,size)
              : (phi(m,primes,count,size)*phi(o,primes,count,size)*d)/phi(d,primes,count,size);
}

uint32_t isPrime(uint64_t n, uint64_t **primes, uint32_t *count, uint32_t *size) {
  uint32_t i,bound,prime;
  for(i=0,prime=1,bound=(uint64_t)sqrt(n)+1; prime && i<*count && (*primes)[i]<=bound; ++i)
    prime = n%(*primes)[i];
  if(prime)
    addPrime(n,primes,count,size);
  return prime || n==2; //catches false negative
}

void addPrime(uint64_t n, uint64_t **primes, uint32_t *count, uint32_t *size) {
  if(*count >= *size)
    *primes = realloc(*primes, (*size*=2) * sizeof **primes);
  if(!isInArr(n,primes,*count))
    (*primes)[(*count)++] = n;
}

uint32_t isInArr(uint64_t n, uint64_t **primes, uint64_t count) {
  uint64_t hi,low,mid,val;
  low = 0; hi = count; // set bounds
  while(low < hi) {    // binary search
    mid = low/2 + hi/2;
    val = (*primes)[mid];
    if(val == n) return  1;
    if(val >  n) hi  = mid;
    if(val <  n) low = mid+1;
  }
  return 0;
}

void print_arr(uint64_t *arr, uint32_t count) {
  uint32_t i;
  for(i=0; i<count; ++i)
    printf("%llu,",arr[i]);
  printf("\n");
}

uint64_t gcd_bin(uint64_t u, uint64_t v) {
    /* simple cases (termination) */
    if(u == v)  return u;
    if(u == 0)  return v;
    if(v == 0)  return u;
    /* look for even numbers  */
    if( ~u & 1) {
      if(v & 1) return gcd_bin(u >> 1, v);           /* u is even, v is odd  */
      else      return gcd_bin(u >> 1, v >> 1) << 1; /* u is even, v is even */
    }
    if( ~v & 1) return gcd_bin(u, v >> 1);           /* u is odd,  v is even */
    /* reduce larger argument */                     /* u is odd,  v is odd  */
    return (u > v) ? gcd_bin((u - v) >> 1, v)
                   : gcd_bin((v - u) >> 1, u);
}

uint32_t print_help(char* str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the values of euler's totient (phi fnction) \n");
  printf("  from 2 to <limit> inclusively\n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 1000000\n");
  return 0;
}
