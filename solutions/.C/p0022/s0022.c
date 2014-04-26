#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  START_SIZE_STR 10
#define  START_SIZE_ARR 10

void     expand_arr(char *** const arr, uint32_t * const size);
void     expand_str(char  ** const str, uint32_t * const size);
void     merge_sort(char  ** const arr, const uint32_t size, const uint32_t ascend);
uint32_t print_help(char* str);

int main(int argc, char* argv[]) {
  uint32_t i,j,more,count,len;
  uint32_t size = START_SIZE_ARR;
  uint32_t asnd = 1; /* default */
  uint64_t sum;
  char   **list = malloc(size * sizeof(char*));
  char    *str;
  char     c;

  /* parse args */
  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&asnd); }

  /* read in data */
  for(count=0,more=1; more; ++count) {
    len = START_SIZE_STR;
    str = (char*) malloc(len * sizeof(char));
    for(i=0,c=1; c!=0;) {
      c = fgetc(stdin);
      switch(c) {
        case '\'':         /* fallthrough */
        case '"' : break;  /* do nothing  */
        case EOF : more=0; /* fallthrough */
        case '\f':         /* fallthrough */
        case '\n':         /* fallthrough */
        case '\t':         /* fallthrough */
        case '\v':         /* fallthrough */
        case ' ' :         /* fallthrough */
        case '|' :         /* fallthrough */
        case ',' : c=0;
        default  :
          if(i>=len)
            expand_str(&str,&len);
          str[i++] = c;
          break;
      }
    }
    if(count>=size)
      expand_arr(&list,&size);
    list[count] = str;
  }

  /* process data */
  size = count;
  list = (char**) realloc(list, size * sizeof(char*)); /* repack array */
  merge_sort(list,size,asnd);
  for(i=sum=0; i<size; ++i)
    for(j=0; list[i][j]!='\0'; ++j)
      sum += (i+1) *(list[i][j]-64);

  /* output result */
  printf("%llu\n",sum);
  return 0;
}

void expand_str(char  ** const str, uint32_t * const size) {
  *size *= 2;
  *str   = (char*)  realloc(*str, *size * sizeof(char));
}

void expand_arr(char *** const arr, uint32_t * const size) {
  *size *= 2;
  *arr   = (char**) realloc(*arr, *size * sizeof(char*));
}

void merge_sort(char  ** const arr, const uint32_t size, const uint32_t ascend) {
  uint32_t  a = size / 2;
  uint32_t  b = size - a;
  uint32_t  i,x,y;
  char      **arrA,**arrB;
  if(size < 2)
    return;

  arrA = (char**) malloc(a * sizeof(char*));
  arrB = (char**) malloc(b * sizeof(char*));
  for(i=0; i<a; ++i)
    arrA[i] = arr[i];
  for(i=0; i<b; ++i)
    arrB[i] = arr[i+a];

  merge_sort(arrA,a,ascend);
  merge_sort(arrB,b,ascend);

  for(i=x=y=0; i<size && x<a && y<b; ++i)
    arr[i] = ( (strcmp(arrA[x],arrB[y]) < 0) == ascend) ? arrA[x++] : arrB[y++];
  while(i<size && x<a)
    arr[i++] = arrA[x++];
  while(i<size && y<b)
    arr[i++] = arrB[y++];

  free(arrA);
  free(arrB);
}

uint32_t print_help(char* str) {
  printf("  Usage: %s <bool>\n",str);
  printf("  Reads in a list of strings from STDIN \n");
  printf("  conditionally enclosed by a pair of ['\"] \n");
  printf("  and delimited by one of [\\f\\n\\t\\v|, ].\n");
  printf("  List is sorted in <bool> order (true=ascending). \n");
  printf("  For each string in the ordered list, \n");
  printf("  the sum of its characters * the string's rank in the list. \n");
  printf("  And the sum of these values are calculated. \n");
  printf("  * bool : unsigned integer interpreted as a boolean value \n");
  printf("         : default 1 \n");
  return 0;
}
