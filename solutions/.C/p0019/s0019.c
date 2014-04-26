#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define  DAYS     7
#define  MONTHS   12
#define  DATE_MAX 31
#define  DATE_MIN 1
#define  YEAR_MAX 65535

#define  SUN  0
#define  MON  1
#define  TUE  2
#define  WED  3
#define  THU  4
#define  FRI  5
#define  SAT  6

#define  JAN  0
#define  FEB  1
#define  MAR  2
#define  APR  3
#define  MAY  4
#define  JUN  5
#define  JUL  6
#define  AUG  7
#define  SEP  8
#define  OCT  9
#define  NOV 10
#define  DEC 11

static const char* days  [] = {"sun","mon","tue","wed","thu","fri","sat"};
static const char* months[] = {"jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"};

typedef struct {
  uint32_t month;
  uint32_t date;
  uint32_t year;
  uint32_t day;
} date;

void     date_set   (date * const inDate, const uint32_t month, const uint32_t date, const uint32_t year, const uint32_t day);
int32_t  date_cmp   (date * const lhs, date * const rhs);
void     date_inc   (date * const inDate);
void     date_dec   (date * const inDate);
uint32_t parse_day  (char * const str);
uint32_t parse_month(char * const str);
uint32_t print_help (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t t_date = 1;   /* default */
  uint32_t t_day  = SUN; /* default */
  uint32_t s_month= JAN,s_date= 1,s_year=1901; /* default */
  uint32_t e_month= DEC,e_date=31,e_year=2000; /* default */
  uint32_t count  = 0;
  date     changer,start,target;

  /* Parse arguments */
  if(argc > 1 &&!strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) { t_day   = parse_day(  argv[1]); }
  if(argc > 2) { sscanf(argv[2],"%u",&t_date );  }
  if(argc > 3) { s_month = parse_month(argv[3]); }
  if(argc > 4) { sscanf(argv[4],"%u",&s_date );  }
  if(argc > 5) { sscanf(argv[5],"%u",&s_year );  }
  if(argc > 6) { e_month = parse_month(argv[6]); }
  if(argc > 7) { sscanf(argv[7],"%u",&e_date );  }
  if(argc > 8) { sscanf(argv[8],"%u",&e_year );  }

  /* Check for valid parameters */
  if(t_day   >= DAYS
  || t_date  >  DATE_MAX || DATE_MIN > t_date
  || s_month >= MONTHS
  || s_date  >  DATE_MAX || DATE_MIN > s_date
  || s_year  >  YEAR_MAX
  || e_month >= MONTHS
  || e_date  >  DATE_MAX || DATE_MIN > e_date
  || e_year  >  YEAR_MAX) { return print_help(argv[0]); }

  /* Initialize dates */
  date_set(&changer,      0,     1,  1900,MON); /* IS A MONDAY */
  date_set(&start  ,s_month,s_date,s_year,SUN);
  date_set(&target ,e_month,e_date,e_year,SUN);

  /* Normalize to start date */
  while(date_cmp(&changer,&start) < 0)
    date_inc(&changer);
  while(date_cmp(&changer,&start) > 0)
    date_dec(&changer);
  /* Calculate date/day matches */
  while(date_cmp(&changer,&target)!=0) {
    if(changer.day == t_day && changer.date == t_date)
      count++;
    /* allows for inversions; start date > end date */
    date_cmp(&start,&target)<0 ? date_inc(&changer) : date_dec(&changer);
  }
  /* Inclusively check last day */
  if(changer.day == t_day && changer.date == t_date)
    count++;
  printf("%u\n",count);
  return 0;
}

void date_set(date * const inDate, const uint32_t month, const uint32_t date, const uint32_t year, const uint32_t day) {
  inDate->month = month;
  inDate->date  = date ;
  inDate->year  = year ;
  inDate->day   = day  ;
}

int32_t date_cmp(date * const lhs, date * const rhs) {
  return (lhs->year == rhs->year && lhs->month == rhs->month  && lhs->date == rhs->date)  ?  0 :
         (lhs->year <  rhs->year || (
          lhs->year == rhs->year && lhs->month <  rhs->month) || (
          lhs->year == rhs->year && lhs->month == rhs->month  && lhs->date <  rhs->date)) ? -1 : 1;
}

void date_inc(date * const inDate) {
  uint32_t x;
  inDate->day += 1;
  inDate->day %= 7;
  switch(inDate->month) {
    case FEB:
      x = ( ( !(inDate->year %4) && inDate->year%100 ) || !(inDate->year%400) ) ? 29 : 28 ;
      if(inDate->date == x) {
        (inDate->month)++;
         inDate->date=1;
      }
      else
        (inDate->date)++;
     break;
    case APR:
    case JUN:
    case SEP:
    case NOV:
      if(inDate->date == 30) {
        (inDate->month)++;
         inDate->date=1;
      }
      else
        (inDate->date)++;
     break;
    case JAN:
    case MAR:
    case MAY:
    case JUL:
    case AUG:
    case OCT:
      if(inDate->date == 31) {
        (inDate->month)++;
         inDate->date=1;
      }
      else
        (inDate->date)++;
     break;
    case DEC:
      if(inDate->date == 31) {
        (inDate->month)= 0;
        (inDate->year )++;
         inDate->date=1;
      }
      else
        (inDate->date)++;
     break;
  }
}

void date_dec(date * const inDate) {
  inDate->day = inDate->day==0 ? 6 : inDate->day -1;

  switch(inDate->month) {
    case MAR: /* -> February */
      if(inDate->date == 1) {
        (inDate->month)--;
         inDate->date=( (!(inDate->year %4) && !(inDate->year%100)) || !(inDate->year%400) ) ? 29 : 28 ;
      }
      else
        (inDate->date)--;
     break;
    case MAY: /* -> April     */
    case JUL: /* -> June      */
    case OCT: /* -> September */
    case DEC: /* -> November  */
      if(inDate->date == 1) {
        (inDate->month)--;
         inDate->date=30;
      }
      else
        (inDate->date)--;
     break;
    case FEB: /* -> Janurary */
    case APR: /* -> March    */
    case JUN: /* -> May      */
    case AUG: /* -> July     */
    case SEP: /* -> August   */
    case NOV: /* -> October  */
      if(inDate->date == 1) {
        (inDate->month)--;
         inDate->date=31;
      }
      else
        (inDate->date)--;
     break;
    case JAN: /* -> December */
      if(inDate->date  == 1) {
        (inDate->month)= 11;
        (inDate->year )--;
         inDate->date=31;
      }
      else
        (inDate->date)--;
     break;
  }
}

uint32_t parse_day(char * const str) {
  uint32_t i,len,isnum,val;
  char *ptr;
  for(len=isnum=1,ptr=str; *ptr; ++ptr,++len) {
    isnum = isnum && isdigit(*ptr);
    *ptr = tolower(*ptr);
  }
  if(isnum && sscanf(str,"%u",&val))
    return val;
  for(i=0; i<DAYS; ++i)
    if(!strcmp(str,days[i]))
      return i;
  return ~0;
}

uint32_t parse_month(char * const str) {
  uint32_t i,len,isnum,val;
  char *ptr;
  for(len=isnum=1,ptr=str; *ptr; ++ptr,++len) {
    isnum = isnum && isdigit(*ptr);
    *ptr = tolower(*ptr);
  }
  if(isnum && sscanf(str,"%u",&val))
    return val;
  for(i=0; i<MONTHS; ++i)
    if(!strcmp(str,months[i]))
      return i;
  return ~0;
}

uint32_t print_help(char const * const str) {
  printf("  Usage: %s <day> <date1> <month1> <date2> <year1> <month2> <date3> <year2> \n",str);
  printf("  Calculates the number of <day>s which fall on a <date1> of the month \n");
  printf("  from the starting date of <month1> <date2> <year1> \n");
  printf("  to the ending date of <month2> <date3> <year2>, inclusively. \n");
  printf("  DATE  VALUES [1,31] \n");
  printf("  YEAR  VALUES [0,65535] \n");
  printf("  DAY   VALUES [sun,mon,tue,wed,thu,fri,sat] \n");
  printf("               [  0,  1,  2,  3,  4,  5,  6] \n");
  printf("  MONTH VALUES [jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec] \n");
  printf("               [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11] \n");
  printf("  DEFAULTS \n");
  printf("  * day    : sun\n");
  printf("  * date1  : 1\n");
  printf("  * month1 : jan\n");
  printf("  * date2  : 1\n");
  printf("  * year1  : 1901\n");
  printf("  * month2 : dec\n");
  printf("  * date3  : 31\n");
  printf("  * year2  : 2000\n");
  return 0;
}
