#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * NOTES:
 * Recursively generate primitive triples,
 * Multiply primitive triples scalars
 * Sort triples per perimeter
 * Count unique perimeters
 */

typedef struct triple {
  uint64_t v[3];
} triple;
int      triplePerimeterCmp(const void * lhs, const void * rhs);
uint64_t perimeter(const triple * const t);
uint32_t getUniquePerimeterCount(triple ** const triples, const uint32_t count);
void     gatherPythagoreanTriplesByPerimeter(const uint32_t limit, triple *** const list, uint32_t * const count);
void     getTriplesByPerimeter(triple * t, const uint64_t limit, triple *** const list, uint32_t * const count, uint32_t * const size);
triple*  getRootTriple(void);
triple*  scalarMultiply(const uint64_t n, const triple * const t);
triple*  matrixMultiply(int64_t * m, const triple * const t);
void     addToList(triple * t, triple *** const list, uint32_t * const count, uint32_t * const size);
void     deleteList(triple *** const list, uint32_t * const count);
void     print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit = 1500000; /* default */
  uint32_t count;
  triple **triples = NULL;

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]), EXIT_SUCCESS; }
  if(argc > 1) {  sscanf(argv[1],"%u",&limit); }

  gatherPythagoreanTriplesByPerimeter(limit,&triples,&count);
  printf("%u\n",getUniquePerimeterCount(triples,count));
  deleteList(&triples,&count);

  return EXIT_SUCCESS;
}

int triplePerimeterCmp(const void * lhs, const void * rhs) {
  return perimeter(*(triple **)lhs)
       - perimeter(*(triple **)rhs);
}

uint64_t perimeter(const triple * const t) {
  int i;
  int64_t sum;
  for(i=sum=0; i<3; ++i)
    sum += t->v[i];
  return sum;
}

uint32_t getUniquePerimeterCount(triple ** const triples, const uint32_t count) {
  uint32_t i,uniquePerimeters;

  if(count < 2)
    return count;

  qsort(triples,count,sizeof *triples,triplePerimeterCmp);

  uniquePerimeters=0;
  /* Check non-end values for uniqueness */
  for(i=1; i<count-1; ++i)
    if(perimeter(triples[i-1]) != perimeter(triples[i  ])
    && perimeter(triples[i  ]) != perimeter(triples[i+1]))
      ++uniquePerimeters;

  /* Check end values for uniqueness */
  if(perimeter(triples[0]) != perimeter(triples[1]))
    ++uniquePerimeters;
  if(perimeter(triples[count-2]) != perimeter(triples[count-1]))
    ++uniquePerimeters;

  return uniquePerimeters;
}

void gatherPythagoreanTriplesByPerimeter(const uint32_t limit, triple *** const list, uint32_t * const count) {
  uint32_t size;

  if(*list != NULL)
    deleteList(list,count);

  size   = 2;
  *count = 0;
  *list  = malloc(size * sizeof **list);

  getTriplesByPerimeter(getRootTriple(),limit,list,count,&size);

  *list = realloc(*list, *count * sizeof **list);
}

void getTriplesByPerimeter(triple * t, const uint64_t limit, triple *** const list, uint32_t * const count, uint32_t * const size) {
  static const int64_t transformationMatrix[3][3][3] =
    {{{ 1,-2, 2}
     ,{ 2,-1, 2}
     ,{ 2,-2, 3}}
    ,{{ 1, 2, 2}
     ,{ 2, 1, 2}
     ,{ 2, 2, 3}}
    ,{{-1, 2, 2}
     ,{-2, 1, 2}
     ,{-2, 2, 3}}};

  uint32_t i;
  uint64_t perimeter_ = perimeter(t);

  if(perimeter_ > limit) {
    free(t);
    return;
  }

  addToList(t,list,count,size);
  for(i=2; i*perimeter_ <= limit; ++i)
    addToList(scalarMultiply(i,t),list,count,size);

  for(i=0; i<3; ++i)
    getTriplesByPerimeter(matrixMultiply((int64_t *)transformationMatrix[i],t)
                         ,limit,list,count,size);
}

triple* getRootTriple(void) {
  static const triple root = {.v={3,4,5}};
  int i;
  triple* result = malloc(sizeof(*result));
  for(i=0; i<3; ++i)
    result->v[i] = root.v[i];
  return result;
}

triple* scalarMultiply(const uint64_t n, const triple * const t) {
  int i;
  triple *result = malloc(sizeof *result);
  for(i=0; i<3; ++i)
    result->v[i] = t->v[i]*n;
  return result;
}

triple* matrixMultiply(int64_t * m , const triple * const t) {
  int i,j;
  int64_t sum;
  triple *result = malloc(sizeof *result);
  for(i=0; i<3; ++i) {
    for(j=sum=0; j<3; ++j)
      sum += t->v[j] * m[3*i+j];
    result->v[i] = sum;
  }
  return result;
}

void addToList(triple * t, triple *** const list, uint32_t * const count, uint32_t * const size) {
  if(*count >= *size)
    *list = realloc(*list, (*size*=2) * sizeof **list);
  (*list)[(*count)++] = t;
}

void deleteList(triple *** const list, uint32_t * const count) {
  uint32_t i;
  for(i=0; i < *count; ++i)
    free((*list)[i]);
  free(*list);
  *list  = NULL;
  *count = 0;
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculates the number of perimeters less then or equal to <limit>\n");
  printf("  for which only a single right triangle can be formed\n");
  printf("  * limit    : a decimal number exclusive bound\n");
  printf("             : default = 15000000\n");
}
