#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  RADIX_MIN =  2;
#define  RADIX_MAX = 62;

static const char *digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

char* getPermutationByIndex(const char * const set, uint64_t index);
void  print_help(const char * const str);

/**
 * factorial base
 */

int main(int argc, char* argv[]) {
  uint64_t index = 1000000; /* Default */
  uint32_t radix = 10;
  char     *permStr,*elements;

  if(argc > 1 &&!strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
    sscanf(argv[1],"%llu",&index);
  if(argc > 2)
    sscanf(argv[2],"%u"  ,&radix);

  elements = malloc((radix+1) * sizeof *elements);
  strncpy(elements,digits,radix);
  elements[radix] = '\0';

  permStr = getPermutationByIndex(elements, index-1); /* zero indexed */

  printf("%s\n",permStr);
  free(permStr);
  free(elements);
  return EXIT_SUCCESS;
}

char*    getPermutationByIndex(const char * const set, uint64_t index) {
  uint64_t i,j,size,factorial;
  char *elem,*out;
  size  = strlen(set);
  elem  = malloc((size+1) * sizeof *elem);
  out   = calloc( size+1,   sizeof *out );

  strncpy(elem,set,size);
  elem[size] = '\0';

  for(i=factorial=1; i<size; ++i)
    factorial *= i;

  for(i=0; size; ++i) {
    j      = index/factorial;
    out[i] = elem[j];
    index %= factorial;

    for(; j<size-1; ++j) /* shift elements over */
      elem[j] = elem[j+1];
    elem[j] = '\0';

    if(--size)
      factorial /= size;
  }
  free(elem);
  return out;
}

void print_help(const char * const str) {
  printf("  Usage: %s <index> <radix> \n",str);
  printf("  Prints the <index>th permutation of a numeric set of size <radix> \n");
  printf("  * index : a decimal number [0,<radix>!] \n");
  printf("          : default = 1000000\n");
  printf("  * radix : [2,62]\n");
  printf("          : default = 10\n");
}
