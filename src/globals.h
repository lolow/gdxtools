/* globals.h
 * Global #defines, typedefs, and (gasp!) data for gdxrrw
 * $Id: globals.h 52395 2015-06-09 04:33:03Z sdirkse $
 */

#if ! defined(_GDXRRW_GLOBALS_H_)
#define       _GDXRRW_GLOBALS_H_

#if defined(_WIN32)
# include <windows.h>
#else
# include <unistd.h>
# include <sys/wait.h>
#endif

#define INVENT_SET_TEXT_DEFAULT NA_LOGICAL
#define LINELEN 1024
#define MAX_STRING 128
#if defined(__linux__) && defined(__x86_64)
/* long story: GLIBC hacked up memcpy on my Fedora 15 machine
 * so GLIBC 2.14 is required to use the gdxrrw.so.  That is not acceptable.
 */
# define MEMCPY memmove
#else
# define MEMCPY memcpy
#endif

typedef int valIndex_t[GMS_MAX_INDEX_DIM]; /* data indices from sparse 'val' */
typedef char shortStringBuf_t[GMS_SSSIZE];
typedef void (GDX_CALLCONV *gdxGetLoadPath_t) (char *s);
typedef enum dType {
  set = GMS_DT_SET,
  parameter = GMS_DT_PAR,
  variable = GMS_DT_VAR,
  equation = GMS_DT_EQU,
  alias = GMS_DT_ALIAS
} dType_t;
typedef enum dForm {
  unKnown=0,
  full,
  sparse
} dForm_t;
typedef enum dField {
  level = GMS_VAL_LEVEL,
  marginal = GMS_VAL_MARGINAL,
  lower = GMS_VAL_LOWER,
  upper = GMS_VAL_UPPER,
  scale = GMS_VAL_SCALE,
  all = GMS_VAL_MAX,
  max = GMS_VAL_MAX + 1
} dField_t;
typedef struct rSpec {          /* rgdx read specifier */
  char name[1024];
  dForm_t dForm;
  dField_t dField;
  int withField;
  int compress;
  int ts;
  int te;
  int withUel;
  int dim;
  SEXP filterUel;
} rSpec_t;
typedef struct wSpec {
  char name[1024];
  shortStringBuf_t aliasFor;
  dForm_t dForm;
  dType_t dType;
  int withDim;                  /* input list had 'dim' element */
  int withVal;                  /* input list had 'val' element */
  int withTs;                   /* input list had 'ts' element */
  int withUel;                  /* input list had 'uel' element */
  int dim;                      /* as read from input list element 'dim' */
  int symDim;                   /* consistent with GDX or GAMS idea of symbol dim */
  int typeCode;                 /* to send to GDX */
} wSpec_t;
typedef unsigned long long int uint64_t;
typedef union d64 {
  double x;
  uint64_t u64;
} d64_t;
typedef enum filterType {             /* filter types */
  unset = 0,
  identity,
  integer
} filterType_t;
typedef enum domainType {
  none = 0,
  relaxed,
  regular
} domainType_t;
typedef struct hpFilter {       /* high-performance filter */
  int *idx;                     /* actual data: uel indices to select */
  int n;                        /* number of elements in filter */
  int isOrdered;                /* is the data ordered? */
  int prevPos;                  /* previous position of successful search */
  filterType_t fType;
} hpFilter_t;
typedef struct xpFilter {       /* xtreme-performance filter */
  int *idx;                     /* actual data: uel indices to select */
  int n;                        /* number of elements in filter */
  int prevPos;                  /* previous position of successful search */
  filterType_t fType;
  domainType_t domType;         /* what type of domain info was the source? */
} xpFilter_t;

GDX_FUNCPTR(gdxGetLoadPath);


/* ********** functions in gdxrrw.c **************** */
SEXP
gdxInfoExt (SEXP args);


/* ********** functions in gdxrrw.c **************** */
SEXP
wgdxExt (SEXP args);
SEXP
igdxExt (SEXP args);
SEXP
gamsExt (SEXP args);


/* ********** functions in rgdx.c *********************** */
SEXP
rgdxExt (SEXP args);


/* ********** functions in wgdx.c *********************** */
SEXP
wgdxExt (SEXP args);


/* ********** functions in utils.c ********************** */
char *
CHAR2ShortStr (const char *from, shortStringBuf_t to);
void
checkFileExtension (shortStringBuf_t fileName);
void
checkStringLength (const char *str);
void
compressData (int symDim, int mRows, SEXP uni, int nUni, xpFilter_t filterList[],
              SEXP spVals, SEXP uels);
void
createElementMatrix (SEXP compVal, SEXP textElement, SEXP compTe,
                     SEXP compUels, int symDim, int nRec);
void
mkHPFilter (SEXP uFilter, hpFilter_t *hpf);
void
mkXPFilter (int symIdx, Rboolean useDomInfo, xpFilter_t filterList[],
            SEXP outDomains, int *domInfoCode);
void
prepHPFilter (int symDim, hpFilter_t filterList[]);
int
findInHPFilter (int symDim, const int inUels[], hpFilter_t filterList[],
                int outIdx[]);
int
findInXPFilter (int symDim, const int inUels[], xpFilter_t filterList[],
                int outIdx[]);
void
xpFilterToUels (int symDim, xpFilter_t filterList[], SEXP uni, SEXP uels);
void
getDomainNames (int symIdx, Rboolean useDomInfo,
                SEXP outDomains, int *domInfoCode);
char *
getGlobalString (const char *globName, shortStringBuf_t result);
int
getNonDefaultElemCount (gdxHandle_t h, int symIdx,
                        int symType, int symSubType, dField_t dField);
int
getNonZeroElements (gdxHandle_t h, int symIdx, dField_t dField);
Rboolean
getInventSetText (Rboolean defVal);
Rboolean
exp2Boolean (SEXP exp);
int
isCompress (void);
void
loadGDX (void);
void
makeStrVec (SEXP outExp, SEXP inExp);
void
sparseToFull (SEXP spVal, SEXP fullVal, SEXP uelLists,
              int symType, int symSubType, dField_t dField, int nRec, int symDimX);
void
getDefRecEqu (int subType, double defRec[]);
void
getDefRecVar (int subType, double defRec[]);
double
getDefValEqu (int subType, dField_t dField);
double
getDefValVar (int subType, dField_t dField);
double
getDefVal (int symType, int subType, dField_t dField);
void
addDomInfo (const char *symName, SEXP domExp, SEXP domInfoExp);
void
showLibSearchPath (void);
int
getEnvVar (const char *evName, shortStringBuf_t evVal);

/* ****** global variables ****** */
#if defined(_GDXRRW_MAIN_)

#define _GDXRRW_EXTERN_
gdxHandle_t gdxHandle = (gdxHandle_t) 0;
int gamsoIsUnset = 0;

#else

#define _GDXRRW_EXTERN_ extern
extern gdxHandle_t gdxHandle;
extern int gamsoIsUnset;
extern char ID[GMS_SSSIZE];

#endif  /* defined(_GDXRRW_MAIN_) */

_GDXRRW_EXTERN_ int globalGams;
_GDXRRW_EXTERN_ int gamsoIsUnset;

#endif /* ! defined(_GDXRRW_GLOBALS_H_) */
