#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/**
 * NOTES:
 * Itterate through all possible points
 * Check for invalid points
 * Check point configrations that are right triangles
 * Don't count triangles twice
 */

uint32_t isRightTriangle(const uint32_t x1, const uint32_t y1, const uint32_t x2, const uint32_t y2, const uint32_t x3, const uint32_t y3);
uint32_t pointsAreUnique(const uint32_t x1, const uint32_t y1, const uint32_t x2, const uint32_t y2, const uint32_t x3, const uint32_t y3);
uint32_t areSamePoint   (const uint32_t x1, const uint32_t y1, const uint32_t x2, const uint32_t y2);
uint32_t distanceSquared(const uint32_t x1, const uint32_t y1, const uint32_t x2, const uint32_t y2);
void     orderLengths   (uint32_t * const a, uint32_t * const b, uint32_t * const c);
void     swap           (uint32_t * const a, uint32_t * const b);
void     print_help     (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t limit = 50; /* Default */
  uint32_t x1,y1,x2,y2;
  uint32_t rightTriangles=0;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;
  if(argc > 1)
     sscanf(argv[1],"%u",&limit);

  for(x1=0; x1<=limit; ++x1)
    for(y1=0; y1<=limit; ++y1)
      for(x2=0; x2<=limit; ++x2)
        for(y2=0; y2<=limit; ++y2)
          if(x2-x1 < y2-y1) /* don't count triangles twice! */
            if(isRightTriangle(x1,y1,x2,y2,0,0))
               ++rightTriangles;

  printf("%u\n",rightTriangles);
  return EXIT_SUCCESS;
}

uint32_t isRightTriangle(const uint32_t x1, const uint32_t y1, const uint32_t x2, const uint32_t y2, const uint32_t x3, const uint32_t y3) {
  uint32_t lengthSquared_A, lengthSquared_B, lengthSquared_C;
  if(!pointsAreUnique(x1,y1,x2,y2,x3,y3))
    return 0;
  lengthSquared_A = distanceSquared(x1,y1,x2,y2);
  lengthSquared_B = distanceSquared(x2,y2,x3,y3);
  lengthSquared_C = distancesquared(x1,y1,x3,y3);
  orderLengths(&lengthSquared_A,&lengthSquared_B,&lengthSquared_C);
  return lengthSquared_A + lengthSquared_B == lengthSquared_C;
}

uint32_t pointsAreUnique(const uint32_t x1, const uint32_t y1, const uint32_t x2, const uint32_t y2, const uint32_t x3, const uint32_t y3) {
  return !(areSamePoint(x1,y1,x2,y2) || areSamePoint(x2,y2,x3,y3) || areSamePoint(x1,y1,x3,y3));
}

uint32_t areSamePoint(const uint32_t x1, const uint32_t y1, const uint32_t x2, const uint32_t y2) {
  return x1==x2 && y1==y2;
}

uint32_t distanceSquared(const uint32_t x1, const uint32_t y1, const uint32_t x2, const uint32_t y2) {
  return (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1);
}

void orderLengths(uint32_t * const a, uint32_t * const b, uint32_t * const c) {
  if(*a > *b) swap(a,b);
  if(*a > *c) swap(a,c);
  if(*b > *c) swap(b,c);
}

void swap(uint32_t * const a, uint32_t * const b) {
  static uint32_t t;
   t = *a;
  *a = *b;
  *b =  t;
}


void print_help(const char * const str) {
  printf("  Usage: %s <limit> \n",str);
  printf("  Calculate the quantity of unique right triangles\n");
  printf("  formed by points O, P, Q such that:\n");
  printf("  O = (0,0), P = (a,b), Q = (c,d) where a,b,c,d are integers\n");
  printf("  and a,b,c,d are in the range [0, <limit>]\n");
  printf("  * limit  : a decimal number\n");
  printf("           : default = 50\n");
}
