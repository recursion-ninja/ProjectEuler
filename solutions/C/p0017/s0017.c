#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

char*    toBritishStr(uint64_t n);
void     getMagStr(uint32_t n, char ** const str, uint32_t * const count, uint32_t * const size);
void     strapp(char** const str, const char * const app, uint32_t * const count, uint32_t * const size);
void     expand_str(char ** const str, uint32_t * const size);
uint32_t countLetters(const char * const str);
uint32_t print_help(const char * const str);

/**
 *
 *
 */

int main(int argc, char* argv[]) {
  uint64_t limit=1000; /* default */
  uint64_t i,sum;
  char *str;
  if(argc > 1 &&  !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1 && (!strcmp(argv[1],"--print") || !strcmp(argv[1],"-p"))) {
    if(argc > 2) sscanf(argv[2],"%llu",&limit);
    printf("%s\n",toBritishStr(limit));
    return EXIT_SUCCESS;
  }
  if(argc > 1)
    sscanf(argv[1],"%llu",&limit);

  for(i=1,sum=0; i<=limit; ++i) {
    str  = toBritishStr(i);
    sum += countLetters(str);
    free(str);
  }
  printf("%llu\n",sum);
  return EXIT_SUCCESS;
}

static const char*    ones [] = {"one","two"   ,"three" ,"four","five","six","seven" ,"eight" ,"nine"};
static const char*    teens[] = {"ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"};
static const char*    tens [] = {"ten","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"};
static const uint32_t mags    = 7;
static const uint64_t mag  [] = { 1,      1000,  1000000,1000000000,1000000000000ULL,1000000000000000ULL,1000000000000000000ULL};
static const char*    denom[] = {""," thousand"," million"," billion"," trillion"," quadrillion"," quintillion" };
static const char*    hund    =  " hundred";
static const char*    hyphen  =  "-";
static const char*    and     =  " and ";

char* toBritishStr(uint64_t n) {
  uint32_t i,first,size=10,count=0;
  char     *out;
  out = (char*) malloc(size * sizeof(char));
  for(i=mags-1,first=1; i<mags; --i) {
    if(n>=mag[i]) {
      if(!first)
        strapp(&out,and,&count,&size);
      first=0;
      getMagStr(n/mag[i],&out,&count,&size);
      strapp(&out,denom[i],&count,&size);
      n%=mag[i];
    }
  }
  if(count>=size)
    expand_str(&out,&size);
  out[count++] = '\0';
  return out;
}

/* expects range [0,999] */
void getMagStr(uint32_t n, char ** const str, uint32_t * const count, uint32_t * const size) {
  uint32_t x,first=1;
  if(n>=100) {
    strapp(str,ones[n/100-1],count,size);
    strapp(str,hund,count,size);
    first=0;
    n%=100;
  }
  if(n==0)
    return;
  if(n<20) {
    if(!first)
      strapp(str,and,count,size);
    strapp(str, (n<10) ? ones[n-1] : teens[n-10] ,count,size);
    first=0;
  }
  else {
    if(!first)
      strapp(str,and,count,size);
    strapp(str,tens[n/10-1],count,size);
    x = n%10;
    if(!x)
      return;
    strapp(str,hyphen,count,size);
    strapp(str,ones[--x],count,size);
  }
}

void strapp(char** const str, const char * const app, uint32_t * const count, uint32_t * const size) {
/*void strapp(char** str, const char * const app, uint32_t* count, uint32_t* size) {*/
  uint32_t i;
  for(i=0; app[i]; ++i) {
    if(*count>=*size)
      expand_str(str,size);
    (*str)[(*count)++] = app[i];
  }
}

void expand_str(char** const str, uint32_t * const size) {
  *size *= 2;
  *str = (char*) realloc(*str, *size * sizeof(char));
}

uint32_t countLetters(const char * const str) {
  uint32_t i, letters;
  for(i=letters=0; str[i]!=0; ++i)
    if(isalpha(str[i]))
      ++letters;
  return letters;
}

uint32_t print_help(const char * const str) {
  printf("  Usage 1: %s <number>\n",str);
  printf("  Usage 2: %s --print <number>\n",str);
  printf("  1): Calculates the number of letters in the \n");
  printf("  british spelling of the numbers from 1 to <number> inclusively\n");
  printf("  2): Prints the british spelling of <number> to STDOUT\n");
  printf("  * number : default 1000\n");
  return 0;
}
