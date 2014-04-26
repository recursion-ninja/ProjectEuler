#include <gmp.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define MIN 2
/**
 * Calculate all
 * Use mergesort to remove duplicates
 */

void     uniqueMergeSort(mpz_t * const values, uint32_t * const count);
uint32_t print_help  (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t  maxBase = 100; /* default */
  uint32_t  maxExp  = 100; /* default */
  uint32_t  a,b,i;
  uint32_t  size,count;
  mpz_t    *values;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&maxBase); }
  if(argc > 2) {  sscanf(argv[2],"%u",&maxExp ); }
  if(maxBase < MIN || maxExp < MIN)
    return EXIT_FAILURE;

  count  = 0;
  size   = (maxBase-1)*(maxExp-1);
  values = malloc(size * sizeof *values);
  if(values==NULL)
    return EXIT_FAILURE;

  for(i=0; i<size; ++i)
    mpz_init(values[i]);

  for(a=MIN; a<=maxBase; ++a)
    for(b=MIN; b<=maxExp; ++b)
      mpz_ui_pow_ui(values[count++],a,b); /* Efficient integer exponentiation */

  uniqueMergeSort(values, &count); /* Sorts and removes duplicate entries */

  printf("%u\n",count);
  return EXIT_SUCCESS;
}

/*void  merge_sort(char  ** const arr, const uint32_t size, const uint32_t ascend) {*/
void  uniqueMergeSort(mpz_t * const list, uint32_t * const count) {
  uint32_t  a = *count / 2;
  uint32_t  b = *count - a;
  uint32_t  i,x,y;
   int      rel;
  mpz_t     *arrA,*arrB;
  if(*count < 2)
    return;

  arrA = malloc(a * sizeof *list);
  arrB = malloc(b * sizeof *list);

  for(i=0; i<a; ++i)
    mpz_init(arrA[i]);
  for(i=0; i<b; ++i)
    mpz_init(arrB[i]);

  for(i=0; i<a; ++i)
    mpz_set(arrA[i],list[i]);
  for(i=0; i<b; ++i)
    mpz_set(arrB[i],list[i+a]);

  /* values of a & b may change when removing dupicates */
  uniqueMergeSort(arrA,&a);
  uniqueMergeSort(arrB,&b);

  /* HERE */
  for(i=x=y=0; i<*count && x<a && y<b; ++i) {
    rel = mpz_cmp(arrA[x], arrB[y]);
    if(rel >  0)
      mpz_set(list[i], arrB[y++]);
    if(rel <  0)
      mpz_set(list[i], arrA[x++]);
    if(rel == 0) {
      mpz_set(list[i], arrA[x++]);
      ++y;
    }
  }
  while(i<*count && x<a)
    mpz_set(list[i++],arrA[x++]);
  while(i<*count && y<b)
    mpz_set(list[i++],arrB[y++]);

  *count = i;

  free(arrA);
  free(arrB);
}


uint32_t print_help(const char * const str) {
  printf("  Usage: %s <x> <y>\n",str);
  printf("  Finds size of the set, (sets don't have duplicates)\n");
  printf("  { a^b | 2 <= a <= x && 2<= b <= y ]\n");
  printf("  * x : a decimal number >= 2\n");
  printf("      : default = 100\n");
  printf("  * y : a decimal number >= 2\n");
  printf("      : default = 100\n");
  return 0;
}
