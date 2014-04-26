#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  ERROR 0xFFFFFFFF

char*    readToken(uint32_t * const die);
char*    intToRomanNumerals(uint32_t x);
void     expand(char ** const str, uint32_t * const size);
uint32_t romanNumeralsToInt(const char * const str);
uint32_t getValue(const char c);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t num,sum,end;
  char     *token,*str;

  if(argc > 1 && !strcmp(argv[1],"--help"))  { return print_help(argv[0]); }

  for(sum=0,end=0; !end && (token=readToken(&end))!=NULL; ) {
    num  = romanNumeralsToInt(token);
    if(num==ERROR) {
      printf("Invalid Numeral String\n");
      return 1337; /* invalid input */
    }
    str  = intToRomanNumerals(num);
    sum += strlen(token) - strlen(str);
    free(token);
    free(str);
  }
  printf("%u\n",sum);
  return 0;
}

char* readToken(uint32_t * const die) {
  uint32_t size=2,i,b;
  char* str = malloc(size * sizeof(char));
  char  c;
  for(i=0,b=1; b; ++i) {
    if(i >= size)
      expand(&str,&size);
    c = fgetc(stdin);
    switch(c) {
      case EOF : *die =1;
      case ' ' :
      case '\n': str[i] = '\0';
                 b=0;
                 break;
      default  : str[i] = c; break;
    }
  }
  return (i==0) ? NULL : str;
}

uint32_t romanNumeralsToInt(const char * const  str) {
  uint32_t total,i,a,b;
  uint32_t len = strlen(str);
  for(i=b=total=0,a=ERROR; str[i]!='\0' && i<len; ++i,b=a)
    if((a=getValue(str[i]))!=ERROR)
      total += (b<a) ? a-b-b : a;
  return a==ERROR ? ERROR : total;
}

char* intToRomanNumerals(uint32_t x) {
  uint32_t size=2,i=0;
  char* str = malloc(size * sizeof(char));

  for(i=0; x>0; ++i){
    if(i+1 >= size)
      expand(&str,&size);
         if(x >= 1000) { x -= 1000; str[i] = 'M';                 }
    else if(x >=  900) { x -=  900; str[i] = 'C'; str[++i] = 'M'; }
    else if(x >=  500) { x -=  500; str[i] = 'D';                 }
    else if(x >=  400) { x -=  400; str[i] = 'C'; str[++i] = 'D'; }
    else if(x >=  100) { x -=  100; str[i] = 'C';                 }
    else if(x >=   90) { x -=   90; str[i] = 'X'; str[++i] = 'C'; }
    else if(x >=   50) { x -=   50; str[i] = 'L';                 }
    else if(x >=   40) { x -=   40; str[i] = 'X'; str[++i] = 'L'; }
    else if(x >=   10) { x -=   10; str[i] = 'X';                 }
    else if(x >=    9) { x -=    9; str[i] = 'I'; str[++i] = 'X'; }
    else if(x >=    5) { x -=    5; str[i] = 'V';                 }
    else if(x >=    4) { x -=    4; str[i] = 'I'; str[++i] = 'V'; }
    else if(x >=    1) { x -=    1; str[i] = 'I';                 }
  }
  if(i>=size)
    expand(&str,&size);
  str[i] = '\0';
  return str;
}

uint32_t getValue(const char c) {
  switch(c) {
    case 'M': return 1000;
    case 'D': return 500;
    case 'C': return 100;
    case 'L': return 50;
    case 'X': return 10;
    case 'V': return 5;
    case 'I': return 1;
    default : return ERROR;
  }
}

void expand(char ** const str, uint32_t * const size) {
  *size *= 2;
  *str  = realloc(*str, *size * sizeof(char));
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s \n",str);
  printf("  Reads roman numeral integers from STDIN and \n");
  printf("  calculates the amount of characters saved \n");
  printf("  by representing the roman numeral integers in minimal character form\n");
  return 0;
}
