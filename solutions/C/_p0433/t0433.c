#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

//uint32_t getOffset(const uint32_t x, const uint32_t y);
void     expand_arr(uint32_t **arr, uint32_t *size);
void     expand_buf(char     **buf, uint32_t *size);
uint32_t print_help(char* str);
/**
 *
 */

uint32_t getOffset(const uint32_t x, const uint32_t y) {
  return ((x)*(x+1))/2 -((x)-(y)) - 1;
}
uint32_t gcd_depth(const uint32_t u, const uint32_t v, const uint32_t depth, uint32_t * const data) {
  if(v == 0)
    return depth;
  uint32_t i = getOffset(u,v);
//  if(data[i])
//    return data[i];
//  else
//    return data[i] = (v != 0) ? gcd_depth(v, u%v, depth+1,data) + depth : depth;
    return data[i] = gcd_depth(v, u%v, depth+1,data);
}

int main(int argc, char* argv[]) {
  uint32_t limit=5000000;//default
  uint32_t i,j;
  uint64_t sum;
  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%u",&limit); }

  uint32_t size  = (limit*(limit+1))/2;
  uint32_t *data = (uint32_t*) malloc(size*sizeof(uint32_t));

  printf("%u\n",size);
//  for(i=1,sum=0; i<=limit; ++i)
//    for(j=1; j<=i; ++j)
//      printf("%u %u %u\n",i,j,getOffset(i,j));


//  for(i=0; i<limit; ++i)
//    data[i] = 0;
/**
  for(i=1,sum=0; i<=limit; ++i)
    for(j=1; j<=i; ++j)
      sum += gcd_depth(i,j,0,data);
//  for(i=0; i<limit; ++i)
//    for(j=0; j<i; ++j)
/**/
  uint32_t t,u,v,k;
  for(i=2,sum=0; i<=limit; ++i) {
    for(j=1; j<i; ++j) {
      for(k=0,u=i,v=j; v; ++k) {
        t = u;
        u = v;
        v = t % v;
      }
      sum += k;
      printf("%u,",k);
    }
    printf("\n");
  }
/**/
/**
  for(i=0; i<limit; ++i) {
    for(j=0; j<=i; ++j)
      printf("%u,",data[getOffset(i+1,j+1)]);
    printf("\n");
  }
/**/
//  sum -= limit;
//  sum *= 2;
//  sum += limit;
//  sum += size;

  printf("%llu\n",sum);
  return 0;
}

/*
void expand_arr(uint32_t **arr, uint32_t *size) {
  *size *= 2;
  *arr   = realloc(*arr,*size * sizeof(uint32_t));
}

void expand_buf(char     **buf, uint32_t *size) {
  *size *= 2;
  *buf   = realloc(*buf,*size * sizeof(char));
}
*/
uint32_t print_help(char* str) {
  printf("  Usage: %s \n",str);
  printf("  Calculates the maximum sumation path from \n");
  printf("  top of pyramid to bottom read in from STDIN.\n");
  printf("  Incorrectly formatted input data may cause \n");
  printf("  unexpected behavior.\n");
  return 0;
}
