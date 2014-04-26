#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
  uint32_t   rows;
  uint32_t   cols;
  uint32_t **data;
} grid;

grid*    makeGrid(const uint32_t rows, const uint32_t cols);
void     freeGrid(grid ** const x);
void     readData(grid *  const x);
uint64_t findMaxProduct(grid *  const x, const uint32_t size);
uint64_t max(const uint64_t a, const uint64_t b, const uint64_t c, const uint64_t d);
void     print_help(const char * const str); //TODO Update Method Text

int main(int argc, char* argv[]) {
  uint32_t rows=20,cols=20,size=4; /* default */
  grid *matrix;

  if(argc  > 1 &&!strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc  > 1)
    sscanf(argv[1],"%u",&size);
  if(argc  > 2)
    sscanf(argv[2],"%u",&rows);
  if(argc  > 3)
    sscanf(argv[3],"%u",&cols);

  matrix = makeGrid(rows,cols);
  readData(matrix);
  printf("%llu\n",findMaxProduct(matrix,size));
  freeGrid(&matrix);

  return EXIT_SUCCESS;
}

grid* makeGrid(const uint32_t rows, const uint32_t cols) {
  uint32_t i;
  grid *out = malloc(sizeof *out);
  out->rows = rows;
  out->cols = cols;
  out->data = malloc(rows * sizeof *(out->data));
  for(i=0; i<rows; ++i)
    out->data[i] = malloc(cols * sizeof **(out->data));
  return out;
}

void freeGrid(grid ** const x) {
  uint32_t i;
  for(i=0; i< (*x)->rows; ++i)
    free((*x)->data[i]);
  free((*x)->data);
  free(*x);
  *x = NULL;
}

void     readData(grid *  const x) {
  uint32_t i,j,k;
  int c;
  char buffer[11]; /* Safely holds UINT32_MAX with trailing null */

  /* Read data from STDIN */
  for(i=0; i<x->rows; ++i) {
    for(j=0; j<x->cols; ++j) {
      for(k=0; (c = fgetc(stdin))!=EOF && c!=' ' && c!='\n'; ++k)
        buffer[k] = c;
      buffer[k] = '\0';
      sscanf(buffer,"%u",&(x->data[i][j]));
    }
  }
}

uint64_t findMaxProduct(grid *  const x, const uint32_t size) {
  uint32_t i,j,k;
  uint64_t p_hrznt,p_vertl,p_diag1,p_diag2,p_tmp,p_max;
  for(i=p_max=0; i<=x->rows-size; ++i) {
    for(j=0; j<x->cols; ++j) {
      p_hrznt = p_vertl = p_diag1 = p_diag2 = 1;
      /* horizontal   */
      if(j+size <= x->cols)
        for(k=0; k<size; ++k)
          p_hrznt *= x->data[i  ][j+k];
      /* vertical     */
      if(i+size <= x->rows)
        for(k=0; k<size; ++k)
          p_vertl *= x->data[i+k][j  ];
      /* diagonal `-. */
      if(i+size <= x->rows && j+size <= x->cols)
        for(k=0; k<size; ++k)
          p_diag1 *= x->data[i+k][j+k];
      /* diagonal ,-' */
      if(i+size <= x->rows && j-size+1 < x->cols)
        for(k=0; k<size; ++k)
          p_diag2 *= x->data[i+k][j-k];

      if((p_tmp = max(p_hrznt,p_vertl,p_diag1,p_diag2)) > p_max)
        p_max = p_tmp;
    }
  }
  return p_max;
}

uint64_t max(const uint64_t a, const uint64_t b, const uint64_t c, const uint64_t d) {
       if(a >= b && a >= c && a >= d)  return a;
  else if(b >= a && b >= c && b >= d)  return b;
  else if(c >= a && c >= b && c >= d)  return c;
  else /*(d >= a && d >= b && d >= c)*/return d;
}

void print_help(const char * const str) {
  printf("  Usage: %s <length> <rows> <cols> \n",str);
  printf("  Reads tabular data from STDIN and\n");
  printf("  calculates the maximum product of <length> consecutive cells in the\n");
  printf("  horizontal, veritcal forward diagonal, or reverse diagonal direction\n");
  printf("  * legnth : a decimal number\n");
  printf("           : default = 4\n");
  printf("  * rows   : a decimal number\n");
  printf("           : default = 20\n");
  printf("  * cols   : a decimal number\n");
  printf("           : default = 20\n");
}
