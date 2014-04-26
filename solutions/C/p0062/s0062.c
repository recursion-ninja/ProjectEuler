#include <gmp.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#define  START_SIZE     5
#define  BASE          10

/**
 * NOTES:
 * DO NOT trust mpz_sizeinbase value to correctly return number of digits (read the docs)
 *
 * DO NOT use the following naive aproach:
 * Getting all permutations for each cube takes factorial time
 * Runtime : O(n*m!)
 * foreach(i in 5 to 1000000) //arbitrarily large upper bound
 *   foreach(perm in getAllPermutations(i*i*i))
 *     if(isCubic(perm))
 *       ++count;
 *     if(count==target)
 *       break;
 *
 * Better Runtime: O(n*m)
 * n = cbrt(min)
 * m = number of cubes with number of digits as n
 */

typedef struct {
  uint16_t  size;      /* memory allocated for roots set */
  uint16_t  count;     /* number of roots matching signiture */
  uint64_t *roots;     /* 32 bit representation of cubic roots */
  uint8_t  *signiture; /* Signiture of permutation */
  char     *minimum;   /* String representation of smallest cubic permutation for signiture */
} permset;

permset* newPermSet  (const uint8_t * const sig, const char * const str, const uint64_t num);
void     delPermSet  (permset    * const set);
void     addPermSet  (permset    * const powerset, const uint64_t elem);
void     addPowerSet (permset  *** const powerset, permset * const set, uint32_t * const count, uint32_t * const size);
void     setSigniture(const char * const str, uint8_t * const sig);
uint32_t print_help  (const char * const str);

int main(int argc, char* argv[]) {
  uint32_t target=5; /* Default */
  uint32_t found,match,size=10,count,setLen,strLen;
  uint64_t i,j,k,l;
  char     *currNumStr;
  uint8_t  *sig        = (uint8_t* ) malloc(BASE * sizeof(uint8_t ));
  permset  **powerset  = (permset**) malloc(  10 * sizeof(permset*));
  mpz_t    num;
  mpz_t    base;

  mpz_init(num);
  mpz_init(base);

  if(argc > 1 && !strcmp(argv[1],"--help")) { return print_help(argv[0]); }
  if(argc > 1) {  sscanf(argv[1],"%u",&target); }

  /* for each length */
  for(i=1,setLen=1; i<UINT64_MAX; ++setLen) {
    currNumStr = (char*) malloc((setLen+2)*sizeof(char));
    /* gather all cubes of that length */
    for(count=0; 1; ++i) {
      mpz_set_ui(base,i);
      mpz_mul(num,base,base);
      mpz_mul(num,num ,base);
      gmp_sprintf(currNumStr,"%Zd",num);
      strLen = strlen(currNumStr); /* get 'len' value AFTER string converstion */
      if(strLen > setLen)
        break;
      setSigniture(currNumStr, sig);
      for(k=match=0; !match && k<count; ++k)
        for(l=0, match=1; match && l<BASE; ++l)
          match = (powerset[k]->signiture)[l] == sig[l];
      if(!match)
        addPowerSet(&powerset,newPermSet(sig,currNumStr,i),&count,&size);
      else
        addPermSet(powerset[--k],i);
    }
    free(currNumStr);

    /* check if a permutation set exist of target cardinality */
    for(j=found=0; !found && j<count; ++j)
      if(powerset[j]->count == target)
        found = 1;
    k = --j;
    /* clear data if not found */
    if(!found)
      for(j=0; j<count; ++j)
        delPermSet(powerset[j]);
    else
      break; /* found the minimum */
  }
  printf("{ ");
  for(i=0; i<powerset[k]->count; ++i)
    printf("%s%llu",i==0 ? "" : ", ", powerset[k]->roots[i]);
  printf(" }\n%llu^3 %s\n",(powerset[k]->roots)[0],powerset[k]->minimum);
  return 0;
}

permset* newPermSet(const uint8_t * const sig, const char * const str, const uint64_t num) {
  permset *set    = (permset*) malloc(sizeof(permset));
  uint8_t *cpy_sig= (uint8_t*) malloc(BASE * sizeof(uint8_t));
  char    *cpy_str= (char*)    malloc((strlen(str)+1) * sizeof(char));
  strcpy(cpy_str,str);
  memcpy(cpy_sig,sig,BASE*sizeof(uint8_t));
  set->size      = START_SIZE;
  set->count     = 1;
  set->roots     = (uint64_t*)  malloc(set->size * sizeof(uint64_t));
  set->signiture = cpy_sig;
  set->minimum   = cpy_str;
 (set->roots)[0] = num;
  return set;
}

void delPermSet(permset * const set) {
  free(set->roots);
  free(set->signiture);
  free(set->minimum);
  free(set);
}

void addPowerSet(permset *** const powerset, permset * const set, uint32_t * const count, uint32_t * const size) {
  if(*count  >= *size) {
    *size    *= 2;
    *powerset = (permset**) realloc(*powerset, *size * sizeof(permset*));
  }
  (*powerset)[(*count)++] = set;
}

void addPermSet(permset * const set, const uint64_t elem) {
  if(set->count >= set->size) {
     set->size  *= 2;
     set->roots  = (uint64_t*) realloc(set->roots, set->size * sizeof(uint64_t));
  }
  (set->roots)[(set->count)++] = elem;
}

/* Expects str to be a numeric string */
/* Expects sig to be a 10 element byte array */
void setSigniture(const char * const str, uint8_t * const sig) {
  uint32_t i;
  memset(sig,0,BASE);
  for(i=0; str[i]; ++i)
    sig[ str[i]-48 ]++;
}

uint32_t print_help(const char * const str) {
  printf("  Usage: %s <perms> \n",str);
  printf("  Finds the smallest cube for which exactly <perms> of it's permutations are also a cube, \n");
  printf("  * perms : a decimal number \n");
  printf("          : default = 5\n");
  return 0;
}
