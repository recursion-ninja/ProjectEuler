#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  START_SIZE 10
#define  ASCII_OFFSET 64

void readData(char *** const list, uint32_t * const count);
void freeList(char *** const list, uint32_t * const count);
int  comparator(const void* a, const void* b);
void print_help(char* str);

int main(int argc, char* argv[]) {
  uint32_t i,j,count=0;
  uint64_t sum;
  char   **list=NULL;

  /* parse args */
  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;

  readData(&list,&count);
  qsort(list,count,sizeof *list,comparator);

  for(i=sum=0; i<count; ++i)
    for(j=0; list[i][j]!='\0'; ++j)
      sum += (i+1) * (list[i][j] - ASCII_OFFSET);

  /* output result */
  printf("%llu\n",sum);
  freeList(&list,&count);
  return EXIT_SUCCESS;
}

void readData(char *** const list, uint32_t * const count) {
  uint32_t i,size,more,len;
  int      c;
  char    *str;

  if(*list!=NULL)
    freeList(list,count);

  *count = 0;
  *list = malloc((size=START_SIZE) * sizeof **list);

  /* read in data */
  more = 1;
  while(more) {
    str = malloc((len =START_SIZE) * sizeof *str);
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
            str = realloc(str, (len*=2) * sizeof *str);
          str[i++] = c;
          break;
      }
    }
    if(*count>=size)
      *list = realloc(*list, (size*=2) * sizeof *list);
    (*list)[(*count)++] = str;
  }
  *list = realloc(*list,*count * sizeof **list);
}

void freeList(char *** const list, uint32_t * const count) {
  uint32_t i;
  for(i=0; i<*count; ++i)
    free((*list)[i]);
  free(*list);
  *list = NULL;
  *count = 0;
}

int comparator(const void* a, const void* b) {
  return strcmp(*((const char **)a),*((const char **)b));
}

void print_help(char* str) {
  printf("  Usage: %s \n",str);
  printf("  Reads in a list of strings from STDIN \n");
  printf("  conditionally enclosed by a pair of ['\"] \n");
  printf("  and delimited by one of [\\f\\n\\t\\v|, ].\n");
  printf("  List is sorted in ascending order. \n");
  printf("  For each string in the ordered list, \n");
  printf("  the sum of its characters * the string's rank in the list. \n");
  printf("  And the sum of these values are calculated. \n");
}
