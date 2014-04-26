#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

uint64_t max(const uint64_t a, const uint64_t b, const uint64_t c, const uint64_t d);
uint32_t print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t rows=20,cols=20,size=4; /* default */
  uint32_t i,j,k,val;
  uint64_t p_max = 0;
  uint64_t p_hrznt,p_vertl,p_diag1,p_diag2,p_temp;
  char c;
  char buffer[32];
  uint32_t **data;

  if(argc  > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc  > 1) { sscanf(argv[1],"%u",&size); }
  if(argc  > 2) { sscanf(argv[2],"%u",&rows); }
  if(argc  > 3) { sscanf(argv[3],"%u",&cols); }
  data = (uint32_t**) malloc(rows * sizeof(uint32_t*));
  for(i=0; i< cols; ++i)
    data[i] = (uint32_t*) malloc(rows * sizeof(uint32_t));

  /* Read data from STDIN */
  for(i=0; i<rows; ++i) {
    for(j=0; j<cols; ++j) {
      for(k=0; (c = fgetc(stdin))!=EOF && c!=' ' && c!='\n'; ++k)
        buffer[k] = c;
      buffer[k] = '\0';
      sscanf(buffer,"%u",&val);
      data[i][j] = val;
    }
  }

  /* Process data */
  for(i=0; i<=rows-size; ++i) {
    for(j=0; j<cols; ++j) {
      p_hrznt = p_vertl = p_diag1 = p_diag2 = 1;
      /* horizontal */
      if(j+size <= cols)
        for(k=0; k<size; ++k)
          p_hrznt *= data[i  ][j+k];
      /* vertical */
      if(i+size <= rows)
        for(k=0; k<size; ++k)
          p_vertl *= data[i+k][j  ];
      /* diagonal `-. */
      if(i+size <= rows && j+size <= cols)
        for(k=0; k<size; ++k)
          p_diag1 *= data[i+k][j+k];
      /* diagonal ,-' */
      if(i+size <= rows && j-size+1 < cols)
        for(k=0; k<size; ++k)
          p_diag2 *= data[i+k][j-k];

      if((p_temp = max(p_hrznt,p_vertl,p_diag1,p_diag2)) > p_max)
        p_max = p_temp;
    }
  }

  printf("%llu\n",p_max);
  return 0;
}

uint64_t max(const uint64_t a, const uint64_t b, const uint64_t c, const uint64_t d) {
       if(a >= b && a >= c && a >= d)  return a;
  else if(b >= a && b >= c && b >= d)  return b;
  else if(c >= a && c >= b && c >= d)  return c;
  else /*(d >= a && d >= b && d >= c)*/return d;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the sum of all primes \n");
  printf("  which are less then the limit \n");
  printf("  * limit : a decimal number\n");
  printf("          : default = 2000000\n");
  return 0;
}

