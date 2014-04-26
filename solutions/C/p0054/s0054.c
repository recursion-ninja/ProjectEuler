#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef enum { TIE=0,PLAYER_ONE=1,PLAYER_TWO=2 } winner;
/* typedef enum { HEART=0x00,DIAMOND=0x40,SPADE=0x80,CLUB=0xC0 } suit; */
/* typedef enum { 2,3,4,5,6,7,8,9,10,11,12,13,14 } rank; */
#define HEART   0x00
#define DIAMOND 0x40
#define SPADE   0x80
#define CLUB    0xC0

#define  HAND_SIZE 5
#define  SUIT_MASK 0xC0
#define  RANK_MASK 0X3F

uint32_t getNextSetOfHands(uint8_t * const p1, uint8_t * const p2);
uint32_t readInHand       (uint8_t * const hand);
uint8_t  readNextCard     (uint32_t * const endOfFile);
uint8_t  getCharSuit      (const char c);
uint8_t  getCharRank      (const char c);


uint32_t compareHands   (uint8_t * const handA, uint8_t * const handB);
uint32_t tieBreaker     (uint8_t * const handA, uint8_t * const handB);
uint32_t getHandValue   (uint8_t * const hand);
void     orderHand      (uint8_t * const hand);
uint8_t  getRank        (const uint8_t card);
uint8_t  getSuit        (const uint8_t card);
void     swap           (uint8_t * const hand, const uint32_t x, const uint32_t y);
void     printHand      (const uint8_t * const hand);
uint8_t  isStraightFlush(uint8_t * const hand);
uint8_t  isFullHouse    (uint8_t * const hand);
uint8_t  isFlush        (uint8_t * const hand);
uint8_t  isStraight     (uint8_t * const hand);
uint8_t  isTwoPair      (uint8_t * const hand);
uint8_t  isXOfKind      (uint8_t * const hand, const uint32_t x);
void     organizeXOfKind(uint8_t * const hand, const uint8_t value);
void     print_help     (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t player1Wins,player2Wins,hands;
  uint8_t  *player1,*player2;
  winner   result;

  if(argc > 1 && !strcmp(argv[1],"--help"))
    return print_help(argv[0]), EXIT_SUCCESS;

  player1 = malloc(HAND_SIZE * sizeof *player1);
  player2 = malloc(HAND_SIZE * sizeof *player2);
  player1Wins = player2Wins = 0;

  for(hands=0; getNextSetOfHands(player1,player2); ++hands)
    if((result = compareHands(player1,player2)) != TIE)
      result==PLAYER_ONE ? ++player1Wins : ++player2Wins;

  printf("Player 1 wins: %6u\n",player1Wins);
  printf("Player 2 wins: %6u\n",player2Wins);
  printf("Ties:          %6u\n",hands-(player1Wins+player2Wins));

  return EXIT_SUCCESS;
}

uint32_t getNextSetOfHands(uint8_t * const p1, uint8_t * const p2) {
  return readInHand(p1) && readInHand(p2);
}

uint32_t readInHand(uint8_t * const hand) {
  uint32_t i,isEndOfFile = 0;
  for(i=0; !isEndOfFile && i<HAND_SIZE; ++i)
    hand[i] = readNextCard(&isEndOfFile);
  return !isEndOfFile;
}

uint8_t readNextCard(uint32_t * const endOfFile) {
  uint32_t haveCardValue,haveSuitValue,haveRankValue;
  uint8_t  cardValue = 0;
  int  c;
  haveCardValue = haveSuitValue = haveRankValue = 0;
  *endOfFile = 0;
  while(!(*endOfFile) && !haveCardValue && (c=getchar())) {
    switch(c) {
      case EOF : *endOfFile = 1; break;
      /* Ignore WhiteSpace */
      case ' ' :
      case '\n':
      case '\t':
      case '\v': break;
      /* Parse Suit Tokens */
      case 'H' :
      case 'D' :
      case 'S' :
      case 'C' : if(!haveSuitValue) cardValue |= getCharSuit(c); haveSuitValue = 1; break;
      /* Parse Rank Tokens */
      case '2' :
      case '3' :
      case '4' :
      case '5' :
      case '6' :
      case '7' :
      case '8' :
      case '9' :
      case 'T' :
      case 'J' :
      case 'Q' :
      case 'K' :
      case 'A' : if(!haveRankValue) cardValue |= getCharRank(c); haveRankValue = 1; break;
      default: break;
    }
    haveCardValue = haveSuitValue && haveRankValue;
  }
  return cardValue;
}

uint8_t getCharSuit(const char c) {
  switch(c) {
    case 'H': return HEART;
    case 'D': return DIAMOND;
    case 'S': return SPADE;
    case 'C': return CLUB;
  }
  return 0;
}

uint8_t getCharRank(const char c) {
  switch(c) {
    case '2' :
    case '3' :
    case '4' :
    case '5' :
    case '6' :
    case '7' :
    case '8' :
    case '9' : return c-'0';
    case 'T' : return 10;
    case 'J' : return 11;
    case 'Q' : return 12;
    case 'K' : return 13;
    case 'A' : return 14;
  }
  return 0;
}

winner compareHands(uint8_t * const handA, uint8_t * const handB) {
  uint32_t valA = getHandValue(handA);
  uint32_t valB = getHandValue(handB);
  if(valA == valB)
    return tieBreaker(handA,handB);
  else
    return valA > valB ? PLAYER_ONE : PLAYER_TWO;
}

winner tieBreaker(uint8_t * const handA, uint8_t * const handB) {
  uint32_t i;
  for(i=0; i<HAND_SIZE; ++i)
    if(getRank(handA[i]) != getRank(handB[i]))
      return getRank(handA[i]) > getRank(handB[i]) ? PLAYER_ONE : PLAYER_TWO;
  return TIE;
}

uint32_t getHandValue(uint8_t * const hand) {
  orderHand(hand);
  if(isStraightFlush(hand)) return 8;
  if(isXOfKind(hand,4)    ) return 7;
  if(isFullHouse(hand)    ) return 6;
  if(isFlush(hand)        ) return 5;
  if(isStraight(hand)     ) return 4;
  if(isXOfKind(hand,3)    ) return 3;
  if(isTwoPair(hand)      ) return 2;
  if(isXOfKind(hand,2)    ) return 1;
  return 0;
}

void orderHand(uint8_t * const hand) {
  /* Selection Sort is optimal for small sets */
  uint32_t i,j,k;
  for(i=0; i<HAND_SIZE; swap(hand,i++,k))
    for(k=j=i; j<HAND_SIZE; ++j)
      if(getRank(hand[j]) > getRank(hand[k]))
        k=j;
}

uint8_t  getRank(const uint8_t card) {
  return card & RANK_MASK;
}

uint8_t  getSuit(const uint8_t card) {
  return card & SUIT_MASK;
}

void     swap(uint8_t * const hand, const uint32_t x, const uint32_t y) {
  uint8_t temp;
  temp    = hand[x];
  hand[x] = hand[y];
  hand[y] = temp;
}

void printHand(const uint8_t * const hand) {
  uint32_t i,c;
  for(i=0; i<HAND_SIZE; ++i) {
    switch(getSuit(hand[i])) {
      case HEART  : c='H'; break;
      case DIAMOND: c='D'; break;
      case SPADE  : c='S'; break;
      case CLUB   : c='C'; break;
      default     : c='X'; break;
    }
    printf("%c%u,",c,getRank(hand[i]));
  }
  printf("\n");
}

uint8_t  isStraightFlush(uint8_t * const hand) {
  return isFlush(hand) && isStraight(hand);
}

uint8_t  isFullHouse    (uint8_t * const hand) {
 return isXOfKind(hand,3) && getRank(hand[HAND_SIZE-1]) == getRank(hand[HAND_SIZE-2]);
}

uint8_t  isFlush        (uint8_t * const hand) {
  uint32_t i,match;
  for(i=match=1; match && i<HAND_SIZE; ++i)
    match = getSuit(hand[i-1]) == getSuit(hand[i]);
  return match;
}
uint8_t  isStraight     (uint8_t * const hand) {
  uint32_t i,descending;
  for(i=descending=1; descending && i<HAND_SIZE; ++i)
    descending = getRank(hand[i-1]) == getRank(hand[i])+1;
  return descending;
}

uint8_t  isTwoPair      (uint8_t * const hand) {
  uint32_t i,j,x;
  if(!isXOfKind(hand,2))
    return 0;
  for(i=2,x=0; !x && i<HAND_SIZE; ++i)
    for(j=i+1; !x && j<HAND_SIZE; ++j)
      if(getRank(hand[i])==getRank(hand[j]))
        x = i;
  if(!x)
    return 0;
  if(x != 2)
    swap(hand,2,HAND_SIZE-1);
  return getRank(hand[0]);
}

uint8_t isXOfKind(uint8_t * const hand,const uint32_t x) {
  uint32_t i,j,match;
  for(i=0; i<=HAND_SIZE-x; ++i) {
    for(j=0,match=1; match && j<x-1; ++j)
      match = getRank(hand[i+j]) == getRank(hand[i+j+1]);
    if(match)
      return organizeXOfKind(hand,getRank(hand[i])), getRank(hand[0]);
  }
  return 0x00;
}

void organizeXOfKind(uint8_t * const hand, const uint8_t value) {
  uint32_t i,j,k;
  for(i=k=0; i<HAND_SIZE; ++i)
    if(getRank(hand[i]) == value)
      swap(hand,i,k++);
  for(i=k; i<HAND_SIZE; swap(hand,i++,k))
    for(j=i+1,k=i; j<HAND_SIZE; ++j)
      if((hand[j] & RANK_MASK) > (hand[k] & RANK_MASK))
        k=j;
}

void print_help(const char * const str) {
  printf("  Usage: %s \n",str);
  printf("  Reads poker hands from STDIN and tallies wins for the players\n");
}
