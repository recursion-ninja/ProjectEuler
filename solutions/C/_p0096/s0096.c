#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define  BLOCK_SIZE 3

uint32_t  getGridSolutionSigniture(const uint32_t * const grid);
uint32_t  getGridWidth(void);
uint32_t  getGridHeight(void);
uint32_t  getGridSize(void);
uint32_t  getGridBlockSize(void);
uint32_t* getNewGrid(void);
uint32_t  getGridOffset(const uint32_t x, const uint32_t y);
uint32_t  readInGridData(uint32_t * const grid);
void      clearGrid(uint32_t * const grid);
uint32_t  readInNextDataToken(uint32_t * const moreFile);
void      readToEndOfLine(uint32_t * const moreFile);
uint32_t  isEndOfLine(int c);
uint32_t  isFullGrid(const uint32_t * const grid);
uint32_t  isValidGrid(const uint32_t * const grid);
uint32_t  isValidRow(const uint32_t * const grid, const uint32_t x);
uint32_t  isValidCol(const uint32_t * const grid, const uint32_t x);
uint32_t  isValidLine(const uint32_t * const grid, const uint32_t x, const uint32_t vertical);
uint32_t  isValidSquare(const uint32_t * const grid, const uint32_t x);
uint8_t*  getNewCheckArr(void);
uint32_t  checkArrIsValid(const uint8_t * const checkArr);
/* Printing */
void      printGrid(const uint32_t * const grid);
uint32_t  shouldOutputColSeperator(const uint32_t x);
uint32_t  shouldOutputRowSeperator(const uint32_t x);
uint32_t  shouldOutputSeperator(const uint32_t x);
void      outputColSeperator(void);
void      outputRowSeperator(void);
void      print_help(const char * const str);

int main(int argc, char* argv[]) {
  uint32_t outputSolution = 0;
  uint32_t i,sum;
  uint32_t *grid;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;

  outputSolution = argc > 1 && !strcmp(argv[1],"--solve");
  grid = getNewGrid();

  for(i=sum=0; readInGridData(grid); ++i) {
//    solveSudokuGrid(grid);
    sum += getGridSolutionSigniture(grid);
    printf("Grid %u is valid: %s\n",i+1,isValidGrid(grid)?"Yes":"No");
    if(outputSolution)
      printf("Grid %u Solution:\n",i+1),
      printGrid(grid);
  }

  printf("%u\n",sum);
  return EXIT_SUCCESS;
}

uint32_t getGridSolutionSigniture(const uint32_t * const grid) {
  return grid[0]*100 + grid[1]*10 + grid[2];
}

uint32_t getGridWidth(void) {
  return BLOCK_SIZE * BLOCK_SIZE;
}

uint32_t getGridHeight(void) {
  return BLOCK_SIZE * BLOCK_SIZE;
}

uint32_t getGridSize(void) {
  return getGridWidth() * getGridHeight();
}

uint32_t getGridBlockSize(void) {
  return BLOCK_SIZE;
}

uint32_t* getNewGrid(void) {
  return malloc(getGridSize() * sizeof (uint32_t*));
}

uint32_t getGridOffset(const uint32_t x, const uint32_t y) {
  return x*getGridWidth() + y;
}

uint32_t readInGridData(uint32_t * const grid) {
  uint32_t i,j,moreFile;
  clearGrid(grid);
  moreFile=1;
  for(i=0; moreFile && i<getGridHeight(); ++i)
    for(j=0; moreFile && j<getGridWidth(); ++j)
      grid[getGridOffset(i,j)] = readInNextDataToken(&moreFile);
  return moreFile;
}

void clearGrid(uint32_t * const grid) {
  memset(grid,0,getGridSize());
}

uint32_t readInNextDataToken(uint32_t * const moreFile) {
  int c;

  c = getchar();
  *moreFile = c != EOF;

  if(!(*moreFile))
    return 0;

  if(isdigit(c))
    return c - '0';

  if(!isEndOfLine(c))
    readToEndOfLine(moreFile);

  return readInNextDataToken(moreFile);
}

void readToEndOfLine(uint32_t * const moreFile) {
  int c,isEOL;
  for(isEOL=0; !isEOL && *moreFile;)
    if((isEOL = isEndOfLine(c=getchar())))
      *moreFile = c != EOF;
}

uint32_t isEndOfLine(int c) {
  switch(c) {
    case EOF :
    case '\n':
    case '\r': return 1;
    default  : return 0;
  }
}

uint32_t  isFullGrid(const uint32_t * const grid) {
  uint32_t i;
  for(i=0; i<getGridSize(); ++i)
    if(grid[i] < 1 || 9 < grid[i])
      return 0;
  return 1;
}

uint32_t isValidGrid(const uint32_t * const grid) {
  uint32_t i,isValid;
  isValid = isFullGrid(grid);

  for(i=0; isValid && i<getGridHeight(); ++i)
    isValid = isValid && isValidRow(grid,i);

  for(i=0; isValid && i<getGridWidth(); ++i)
    isValid = isValid && isValidCol(grid,i);

  for(i=0; isValid && i<getGridWidth(); ++i)
    isValid = isValid && isValidSquare(grid,i);

  return isValid;
}

uint32_t  isValidRow(const uint32_t * const grid, const uint32_t x) {
  return isValidLine(grid,x,0);
}

uint32_t  isValidCol(const uint32_t * const grid, const uint32_t x) {
  return isValidLine(grid,x,1);
}

uint32_t  isValidLine(const uint32_t * const grid, const uint32_t x, const uint32_t vertical) {
  static uint8_t *checkArr = NULL;
  uint32_t i;
  if(checkArr == NULL)
    checkArr = getNewCheckArr();
  memset(checkArr,0,getGridWidth()+1);

  for(i=0; i<getGridWidth(); ++i)
    vertical
    ? (checkArr[ grid[getGridOffset(i,x)] ])++
    : (checkArr[ grid[getGridOffset(x,i)] ])++;

  return checkArrIsValid(checkArr);
}

uint32_t  isValidSquare(const uint32_t * const grid, const uint32_t x) {
  static uint8_t *checkArr = NULL;
  uint32_t i,j,horzOffset,vertOffset;

  if(checkArr == NULL)
    checkArr = getNewCheckArr();
  memset(checkArr,0,getGridWidth()+1);

  vertOffset = (x/3) * getGridBlockSize();
  horzOffset = (x%3) * getGridBlockSize();

  for(i=0; i<getGridBlockSize(); ++i)
    for(j=0; j<getGridBlockSize(); ++j)
      (checkArr[ grid[getGridOffset(vertOffset+i,horzOffset+j)] ] )++;

  return checkArrIsValid(checkArr);
}

uint8_t* getNewCheckArr(void) {
  return malloc((getGridWidth()+1) * sizeof (uint8_t));
}

uint32_t checkArrIsValid(const uint8_t * const checkArr) {
  uint32_t i;
  if(checkArr[0])
    return 0;

  for(i=1; i<getGridWidth()+1; ++i)
    if(checkArr[i]!=1)
      return 0;
  return 1;
}

void printGrid(const uint32_t * const grid) {
  uint32_t i,j;
  for(i=0; i<getGridHeight(); ++i) {
    for(j=0; j<getGridWidth(); ++j) {
      printf(" %u",grid[getGridOffset(i,j)]);
      if(shouldOutputColSeperator(j))
        printf(" "), outputColSeperator();
    }
    printf("\n");
    if(shouldOutputRowSeperator(i))
      outputRowSeperator();
  }
}

uint32_t shouldOutputColSeperator(const uint32_t x) {
  return shouldOutputSeperator(x);
}

uint32_t shouldOutputRowSeperator(const uint32_t x) {
  return shouldOutputSeperator(x);
}

uint32_t shouldOutputSeperator(const uint32_t x) {
  return 0 < x && x < getGridWidth()-1 && ((x+1) % (getGridBlockSize())) == 0;
}

void outputColSeperator(void) {
  printf("|");
}

void outputRowSeperator(void) {
  uint32_t i;
  for(i=0; i<getGridWidth(); ++i) {
    printf("--");
    if(shouldOutputColSeperator(i))
      printf("-"), outputColSeperator();
  }
  printf("\n");
}

void print_help(const char * const str) {
  printf("  Usage: %s <limit> <divisors>\n",str);
  printf("  Calculates the sum of all numbers which are\n");
  printf("  divisible by one or more of the divisors\n");
  printf("  and less then the limit\n");
  printf("  * limit    : a decimal number exclusive bound\n");
  printf("             : default = 1000\n");
  printf("  * divisors : a decimal number csv list (no spaces)\n");
  printf("             : default = 3,5\n");
}
