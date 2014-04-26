#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  MIN_LOWERCASE  65
#define  MAX_LOWERCASE  90
#define  MIN_UPPERCASE  97
#define  MAX_UPPERCASE 122
/**
 * 
 */
uint32_t wordSum(const char * const buf);
uint32_t letterValue(const char c);
int      isUpperCaseLetter(const char c);
int      isLowerCaseLetter(const char c);
int      readWord(char ** const buf, uint32_t * const size);
int      readData(char ** const buf, uint32_t * const size);
int      toEnquote(void);
int      toDelimeter(void);
int      isDelimeter(char c);
int      isEnquote(char c);
int      isTriangleNumber(const uint32_t n);
uint32_t inList(const uint32_t n, const uint32_t * const arr, const uint32_t count);
int      print_help(char* str);

int main(int argc, char* argv[]) {
  int finished;
  uint32_t size = 2;
  uint32_t count;
  char *buffer  = malloc(size * sizeof *buffer);

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]);

  for(count=finished=0; !finished;) {
    finished = readWord(&buffer,&size);
    if(isTriangleNumber(wordSum(buffer)))
      ++count;
  }
  printf("%u\n",count);
  free(buffer);
  return EXIT_SUCCESS;
}

uint32_t wordSum(const char * const buf) {
  int i;
  uint32_t sum;
  for(i=sum=0; buf[i]; ++i)
    sum += letterValue(buf[i]);
  return sum;
}

uint32_t letterValue(const char c) {
  return isLowerCaseLetter(c) ? c - MIN_LOWERCASE + 1
       : isUpperCaseLetter(c) ? c - MIN_UPPERCASE + 1
       : 0;
}

int isUpperCaseLetter(const char c) {
  return MIN_UPPERCASE <= c && c <= MAX_UPPERCASE;
}

int isLowerCaseLetter(const char c) {
  return MIN_LOWERCASE <= c && c <= MAX_LOWERCASE;
}

int readWord(char ** const buf, uint32_t * const size) {
  int reachedEOF = 0;
  (*buf)[0] = '\0';

  if(!reachedEOF)
    reachedEOF = toEnquote();

  if(!reachedEOF)
    reachedEOF = readData(buf,size);

  if(!reachedEOF)
    reachedEOF = toDelimeter();

  return reachedEOF;
}

int readData(char ** const buf, uint32_t * const size) {
  int c;
  uint32_t i;
  for(i=0; !isEnquote(c = getchar()) && c != EOF; ++i) {
    if(i >= *size)
      *buf = realloc(*buf, (*size *= 2) * sizeof **buf);
    (*buf)[i] = c;
  }
    if(i >= *size)
      *buf = realloc(*buf, (*size *= 2) * sizeof **buf);
  (*buf)[i] = '\0';
  return c == EOF;
}

int toEnquote(void) {
  int c;
  while(!isEnquote(c = getchar()) && c !=EOF);
  return c == EOF;
}

int toDelimeter(void) {
  int c;
  while(!isDelimeter(c = getchar()) && c !=EOF);
  return c == EOF;
}

int isDelimeter(char c) {
  switch(c) {
    case  ',':
    case  '|':
    case '\t':
    case '\n': return 1;
    default:   return 0;
  }
}

int isEnquote(char c) {
  switch(c) {
    case  '"':
    case '\'': return 1;
    default:   return 0;
  }
}


int isTriangleNumber(const uint32_t n) {
  static uint32_t *triangleNumbers = NULL;
  static uint32_t count=0;
  static uint32_t size=0;

  if(triangleNumbers==NULL)
    triangleNumbers = malloc((size=2) * sizeof *triangleNumbers);

  for(; count==0 || n > triangleNumbers[count-1]; ++count) {
    if(count >= size)
      triangleNumbers = realloc(triangleNumbers, (size*=2) * sizeof *triangleNumbers);
    triangleNumbers[count] = (count*(count+1))/2;
  }

  return inList(n,triangleNumbers,count);
}

uint32_t inList(const uint32_t n, const uint32_t * const arr, const uint32_t count) {
  uint64_t low,hi,mid;
  low = 0;
  hi = count;
  while(low < hi) { /* binary search */
    mid = low/2 + hi/2;
    if(arr[mid] == n)
      return  1;
    if(arr[mid] >  n)
      hi  = mid;
    else
      low = mid + 1;
  }
  return 0;
}

int print_help(char* str) {
  printf("  Usage: %s \n",str);
  printf("  Calculates the number of triangle words in input\n");
  printf("  read from STDIN as enquoted CSV data\n");
  return 0;
}
