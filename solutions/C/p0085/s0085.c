#include <stdio.h>
#include <stdint.h>
#include <string.h>

/**
 * Reduce nxm sub-rectangle problem to closed form n*m*(n+1)*(m+1)
 * Fix the height=1, use binary serach to determine the width with least error
 * Progress by incrementing fixed heights until width is greater then height
 * This means we have crossed the line height=width, and all subsequent values are reflections
 * Store & update values with current least error to target
 */

uint64_t  getClosestWidth(const uint64_t height, const uint64_t target);
uint64_t  getError(const uint64_t height, const uint64_t width, const uint64_t target);
uint32_t  isLeastError(const uint64_t height, const uint64_t width, const uint64_t target);
uint64_t  subRectangleCount(const uint64_t height, const uint64_t width);
uint32_t  print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint64_t x,y,e,dx,dy,de;
  uint64_t target     = 2000000; /* default */

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { sscanf(argv[1],"%llu",&target); }

  dx=dy=de=x= UINT64_MAX;

  for(y=1; y<x; ++y) {
    x = getClosestWidth(y,target);
    e = getError(x,y,target);
    if(e < de)
      dx=x, dy=y, de=e;
  }

  printf("(%llu,%llu) %llu : %llu = %llu +/- %llu \n",dx,dy,dx*dy,subRectangleCount(dx,dy),target,getError(dx,dy,target));
  return 0;
}

uint64_t getClosestWidth(const uint64_t height, const uint64_t target) {
  uint64_t hi,mid,low;
  low = hi = 1;
  while(subRectangleCount(height,hi) < target)
    low = hi, hi *= 2;

  while(low < hi) {
    mid = low/2 + hi/2;
    if(isLeastError(height,mid,target))
      return mid;
    if(subRectangleCount(height,mid) > target)
      hi  = mid;
    else
      low = mid + 1;
  }
  return 0;
}

uint64_t getError(const uint64_t height, const uint64_t width, const uint64_t target) {
  int64_t t = ((int64_t)subRectangleCount(height,width)) - ((int64_t)target);
  return t < 0 ? -t : t;
}

uint32_t isLeastError(const uint64_t height, const uint64_t width, const uint64_t target) {
  uint64_t a,b,c;
  a = getError(height,width-1,target);
  b = getError(height,width-0,target);
  c = getError(height,width+1,target);
  return a >= b && b <= c;
}

uint64_t subRectangleCount(const uint64_t height, const uint64_t width) {
  return ( height * width * (height + 1) * (width + 1) )/4;
}

uint32_t  print_help(const char * const str) {
  printf("  Usage: %s <target>\n",str);
  printf("  Calculates the dimensions of the rectangle\n");
  printf("  whose sub rectangle count is closest to <target>\n");
  printf("  * target   : a decimal number exclusive bound\n");
  printf("             : default = 2000000\n");
  return 0;
}
