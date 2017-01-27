/* utils.c
 * utility functions for gdxrrw package
 * $Id: utils.c 57609 2016-06-10 02:02:59Z sdirkse $
 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "gdxcc.h"
#include "gclgms.h"
#include "globals.h"

/* just to shut up some warnings on Linux */
typedef int (*compareFunc_t) (const void *, const void *);

/* checkForDuplicates
 * checks the input array of strings for duplicates,
 * throwing an error is there are any
 */
static void
checkForDuplicates (SEXP strExp)
{
  int k;
  shortStringBuf_t *elements;
  int nRec;

  /* store all the strings in temporary storage */
  nRec = length(strExp);

  elements = malloc(nRec * sizeof(*elements));
  for (k = 0; k < nRec; k++) {
    strcpy(elements[k], CHAR(STRING_ELT(strExp, k)));
  }

  /* sort the strings */
  qsort (elements, nRec, sizeof(*elements), (compareFunc_t)strcmp);

  /* check for duplicates */
  for (k = 0;  k < nRec - 1;  k++) {
    if (0 == strcmp(elements[k], elements[k+1])) {
      Rprintf ("Input UEL filter has duplicate entry of '%s'\n",
               elements[k]);
      error ("UEL filters must not contain duplicates.");
    }
  }
  free (elements);
} /* checkForDuplicates */

/* ------------------ start of globally available functions --------------- */

/* CHAR2ShortStr
 * copy the input C-style string to a shortString buffer
 * return the output buffer, nor NULL if there is no input
 */
char *
CHAR2ShortStr (const char *from, shortStringBuf_t to)
{
  size_t n;

  if (NULL == from)
    return NULL;
  n = strlen(from);
  if (n >= sizeof(shortStringBuf_t)) {
    n = sizeof(shortStringBuf_t);
    strncpy(to, from, n);
    to[n-1] = '\0';
  }
  else
    strcpy(to, from);
  return to;
} /* CHAR2ShortStr */

/* checkFileExtension: checks the file extension of the gdx file.
 * If fileName does not have '.gdx' then we add it here
*/
void
checkFileExtension (shortStringBuf_t fileName)
{
  char *fileExt;

  fileExt = strrchr (fileName, '.');
  if (NULL == fileExt) {
    if (strlen(fileName) < sizeof(shortStringBuf_t)-4) {
      fileName = strcat(fileName, ".gdx");
    }
    else {
      error ("Input file name '%s' is too long\n", fileName);
    }
  }
  else if (0 != strcasecmp(".gdx", fileExt)) {
    error ("Input file extension '%s' is not valid: input must be a GDX file\n",
           fileExt);
  }
  return;
} /* checkFileExtension */

/* checkStringLength
 * raise an exception if the input str is too long to be a short-string
 */
void
checkStringLength (const char *str)
{
  int i;

  i = (int) strlen (str);
#if 0
  if (0 == i) {
    error ("Cannot access empty field. Please try again");
  }
#endif
  if (i >= sizeof(shortStringBuf_t)) {
    error("The data entered is too long: len=%d exceeds limit=%d.",
          i, (int)sizeof(shortStringBuf_t)-1);
  }
} /* checkStringLength */

/* compressData: compress the $vals data (in sparse form)
 * and also the associated domains.
 *     in: symDim - symbol dimension
 *     in: mRows - nonzeros in symbol / rows in $vals
 *     in: uni, nUni - universe of uels and its size/count
 *     in: xpFilter_t filterList[]
 * in/out: spVals - $vals to output
 *    out: uelList - $uels to output
*/
void
compressData (int symDim, int mRows, SEXP uni, int nUni, xpFilter_t filterList[],
              SEXP spVals, SEXP uelList)
{
  int n;                  /* initial cardinality for this index pos */
  int nn;                 /* compressed cardinality */
  int nMax;
  int iDim, i, k, stop, nTmp;
  int *mask;
  double *v;                    /* index values in spVals */
  SEXP uelVec;                  /* elements of uelList */

  n = 0;                        /* shut up warnings */
  nMax = 0;
  for (iDim = 0;  iDim < symDim;  iDim++) {
    xpFilter_t *xpf = filterList + iDim;
    switch (xpf->fType) {
    case unset:
      error ("internal error: xpFilter type unset");
      break;
    case identity:
      /* assume index values in this dimension are in [1..nUni] */
      n = nUni;
      break;
    case integer:
      /* assume index values in this dimension are in [1..xpf->n] */
      n = xpf->n;
      break;
    default:
      error ("internal error: unknown hpFilter type");
    } /* end switch */
    if (n > nMax)
      nMax = n;
  } /* loop over index positions */

  mask = malloc(nMax*sizeof(*mask));
  v =  REAL(spVals);
  for (iDim = 0;  iDim < symDim;  iDim++) {
    nn = 0;
    xpFilter_t *xpf = filterList + iDim;
    switch (xpf->fType) {
    case unset:
      error ("internal error: xpFilter type unset");
      break;
    case identity:
      n = nUni;
      /* record and count used index values */
      /* (void) memset (mask, 0, nMax*sizeof(*mask)); */
      (void) memset (mask, 0, n*sizeof(*mask));
      for (k = mRows*iDim, stop = mRows*(iDim+1);  k < stop;  k++) {
        i = (int)v[k];          /* one-based */
        if ((i <= 0) || (i > n))
          error ("bogus index i found in compressData: iDim=%d n=%d i=%d",
                 iDim, n, i);
        i--;
        if (mask[i] == 0) {
          mask[i] = 1;
          nn++;
        }
      } /* loop over index values in this position */
      PROTECT(uelVec = allocVector(STRSXP, nn));
      SET_VECTOR_ELT(uelList, iDim, uelVec);
      UNPROTECT(1);
      /* loop over found index values in mask, computing new index values */
      for (nTmp = 0, i = 0;  i < n;  i++) {
        if (mask[i]) {
          SET_STRING_ELT(uelVec, nTmp, STRING_ELT(uni, i));
          nTmp++;
          mask[i] = nTmp;
        }
      }
      /*  update index values to compressed ordering */
      for (k = mRows*iDim, stop = mRows*(iDim+1);  k < stop;  k++) {
        i = (int)v[k] - 1;
        v[k] = mask[i];
      } /* loop over index values in this position */
      break;
    case integer:
      n = xpf->n;
      /* record and count used index values */
      /* (void) memset (mask, 0, nMax*sizeof(*mask)); */
      (void) memset (mask, 0, n*sizeof(*mask));
      for (k = mRows*iDim, stop = mRows*(iDim+1);  k < stop;  k++) {
        i = (int)v[k];          /* one-based */
        if ((i <= 0) || (i > n))
          error ("bogus index i found in compressData: iDim=%d n=%d i=%d",
                 iDim, n, i);
        i--;
        if (mask[i] == 0) {
          mask[i] = 1;
          nn++;
        }
      } /* loop over index values in this position */
      PROTECT(uelVec = allocVector(STRSXP, nn));
      SET_VECTOR_ELT(uelList, iDim, uelVec);
      UNPROTECT(1);
      /* loop over found index values in mask,
       * computing new index values and new output uels */
      for (nTmp = 0, i = 0;  i < n;  i++) {
        if (mask[i]) {
          SET_STRING_ELT(uelVec, nTmp, STRING_ELT(uni, xpf->idx[i]-1));
          nTmp++;
          mask[i] = nTmp;
        }
      }
      /*  update index values to compressed ordering */
      for (k = mRows*iDim, stop = mRows*(iDim+1);  k < stop;  k++) {
        i = (int)v[k] - 1;
        v[k] = mask[i];
      } /* loop over index values in this position */


      break;
    default:
      error ("internal error: unknown hpFilter type");
    } /* end switch */
  } /* loop over index positions */

  free(mask);
  return;
} /* compressData */

/* createElementMatrix: what does this do?
 * create text element Matrix from sparse data and element vector
 */
void
createElementMatrix (SEXP compVal, SEXP textElement, SEXP compTe,
                     SEXP compUels, int symDim, int nRec)
{
  int i, j, iRec, index, totNumber;
  double *p;

  /* Step 1: loop over full matrix and set every value as empty string */
  for (j = 0; j < length(compTe); j++) {
    SET_STRING_ELT(compTe, j, mkChar(""));
  }

  /* Step 2: loop over each row of sparse matrix and populate full matrix */
  p = REAL(compVal);

  for (iRec = 0; iRec < nRec; iRec++) {
    index = 0;
    totNumber = 1;
    for (i = 0;  i < symDim;  i++) {
      index = index + ((int)p[iRec + nRec*i] - 1)*totNumber;
      totNumber = (totNumber)*length(VECTOR_ELT(compUels, i));
    }
    SET_STRING_ELT(compTe, index, duplicate(STRING_ELT(textElement, iRec)) );
  }

  return;
} /* createElementMatrix */

/* mkHPFilter: construct an integer filter from the user-supplied string uels
 * ufilter: user-supplied filter - $uels[k]
 * hpf: high-performance filter for internal use
 */
void
mkHPFilter (SEXP uFilter, hpFilter_t *hpf)
{
  int k, n;
  int *idx;
  const char *uelString;
  int isOrdered = 1;
  int dummy, uelInt, lastUelInt;
  int allFound = 1;    /* all strings from the filter found in GDX? */
  int found;

  hpf->fType = integer;
  hpf->n = n = length(uFilter);
  /* Rprintf ("  mkHPFilter: n = %d\n", n); */
  hpf->prevPos = 0;
  hpf->idx = idx =  malloc(n * sizeof(*idx));
  if (NULL == idx)
    error ("memory exhaustion error: could not allocate index for hpFilter");
  for (lastUelInt = 0, k = 0;  k < n;  k++) {
    uelString = CHAR(STRING_ELT(uFilter, k));
    found = gdxUMFindUEL (gdxHandle, uelString, &uelInt, &dummy);
    /* Rprintf ("       k = %2d:  %s  %d  %d\n", k, uelString, uelInt, dummy); */
    if (! found) {                /* not found */
      allFound = 0;
      isOrdered = allFound; /* for now insist all are found to be ordered */
    }
    else if (isOrdered) {
      if (uelInt > lastUelInt)
        lastUelInt = uelInt;
      else
        isOrdered = 0;
    }
    idx[k] = uelInt;
  }
  hpf->isOrdered = isOrdered;
  if (! isOrdered) {
    checkForDuplicates (uFilter);
  }
  /* Rprintf ("  mkHPFilter: isOrdered = %d  allFound = %d\n", isOrdered, allFound); */
  return;
} /* mkHPFilter */

/* mkXPFilters: construct XPfilter from what?
 * symIdx: of symbol to construct filter for
 * xpf: high-performance filter for internal use
 */
void
mkXPFilter (int symIdx, Rboolean useDomInfo, xpFilter_t filterList[],
            SEXP outDomains, int *domInfoCode)
{
  int rc, iRec, nRecs, changeIdx;
  int kSym, kDim, kType;        /* for loop over index sets */
  int iDim, symDim, symType, symNNZ, symUser;
  int *idx;
  shortStringBuf_t symName, kName, symText;
  gdxStrIndex_t domNames;
  gdxStrIndexPtrs_t domPtrs;
  gdxUelIndex_t symDoms;
  gdxUelIndex_t uels;
  gdxValues_t values;
  xpFilter_t *xpf;

  GDXSTRINDEXPTRS_INIT (domNames, domPtrs);
  /* gdxSymbolGetDomainX is/was buggy and might not write the domain names
   * set to '*' to work around that */
  for (iDim = 0;  iDim < GLOBAL_MAX_INDEX_DIM;  iDim++)
    strcpy (domPtrs[iDim], "*");

  *domInfoCode = 0;             /* NA/unused */
  rc = gdxSymbolInfo (gdxHandle, symIdx, symName, &symDim, &symType);
  if (! rc)
    error ("bad return from gdxSymbolInfo in mkXPFilter");
  if (symDim <= 0)
    return;                     /* skip scalars */
  switch (symType) {
  case GMS_DT_PAR:
  case GMS_DT_SET:
  case GMS_DT_VAR:
  case GMS_DT_EQU:
    if (useDomInfo) {
      rc = gdxSymbolGetDomainX (gdxHandle, symIdx, domPtrs);
      *domInfoCode = rc;
    }
    else
      rc = 1;                   /* no domain info available */
    switch (rc) {
    case 1:                   /* NA: no domain info */
      for (iDim = 0;  iDim < symDim;  iDim++) {
        xpf = filterList + iDim;
        xpf->domType = none;
        xpf->fType = identity;
      } /* end loop over dims */
      if (R_NilValue != outDomains) {
        for (iDim = 0;  iDim < symDim;  iDim++) {
          SET_STRING_ELT(outDomains, iDim, mkChar("*"));
        }
      }
      break;
    case 2:                   /* relaxed domain info */
      for (iDim = 0;  iDim < symDim;  iDim++) {
        xpf = filterList + iDim;
        SET_STRING_ELT(outDomains, iDim, mkChar(domPtrs[iDim]));
        if (0 == strcmp(domPtrs[iDim],"*")) { /* not available: use the universe */
          xpf->domType = none;
          xpf->fType = identity;
          continue;
        }
        rc = gdxFindSymbol (gdxHandle, domPtrs[iDim], &kSym);
        if (! rc) {             /* not available: use the universe */
          xpf->domType = none;
          xpf->fType = identity;
          continue;
        }
        /* now we have the nice case: set symbol is in GDX */
        rc = gdxSymbolInfo (gdxHandle, kSym, kName, &kDim, &kType);
        if (! rc)
          error ("bad return from gdxSymbolInfo in mkXPFilter");
        if (0 != strcmp(domPtrs[iDim],kName))
          error ("bad domain lookup: %s <> %s", domPtrs[iDim], kName);
        if (GMS_DT_ALIAS == kType) {
          gdxSymbolInfoX (gdxHandle, kSym, &symNNZ, &symUser, symText);
          kSym = symUser;
          rc = gdxSymbolInfo (gdxHandle, kSym, kName, &kDim, &kType);
          if (! rc)
            error ("bad return from gdxSymbolInfo in mkXPFilter");
        }
        if ((1 != kDim) || (GMS_DT_SET != kType))
          error ("non-domain-set data from gdxSymbolInfo in mkXPFilter");
        xpf->domType = relaxed;
        xpf->fType = integer;
        xpf->prevPos = 0;
        gdxDataReadRawStart (gdxHandle, kSym, &nRecs);
        xpf->n = nRecs;
        xpf->idx = idx =  malloc(nRecs * sizeof(*idx));
        for (iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          idx[iRec] = uels[0];
        } /* loop over GDX records */
        if (!gdxDataReadDone (gdxHandle)) {
          error ("Could not gdxDataReadDone");
        }
      } /* end loop over dims */
      break;
    case 3:                   /* full domain info */
      rc = gdxSymbolGetDomain (gdxHandle, symIdx, symDoms);
      if (! rc)
        error ("error calling gdxSymbolGetDomain");
      for (iDim = 0;  iDim < symDim;  iDim++) {
        SET_STRING_ELT(outDomains, iDim, mkChar(domPtrs[iDim]));
        kSym = symDoms[iDim];
        rc = gdxSymbolInfo (gdxHandle, kSym, kName, &kDim, &kType);
        if (! rc)
          error ("bad return from gdxSymbolInfo in mkXPFilter");
        if (GMS_DT_ALIAS == kType) {
          gdxSymbolInfoX (gdxHandle, kSym, &symNNZ, &symUser, symText);
          kSym = symUser;
          rc = gdxSymbolInfo (gdxHandle, kSym, kName, &kDim, &kType);
          if (! rc)
            error ("bad return from gdxSymbolInfo in mkXPFilter");
        }
        if ((1 != kDim) || (GMS_DT_SET != kType))
          error ("non-domain-set data from gdxSymbolInfo in mkXPFilter");
        xpf = filterList + iDim;
        xpf->domType = regular;
        xpf->fType = integer;
        xpf->prevPos = 0;
        gdxDataReadRawStart (gdxHandle, kSym, &nRecs);
        xpf->n = nRecs;
        xpf->idx = idx =  malloc(nRecs * sizeof(*idx));
        for (iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          idx[iRec] = uels[0];
        } /* loop over GDX records */
        if (!gdxDataReadDone (gdxHandle)) {
          error ("Could not gdxDataReadDone");
        }
      } /* loop over domain sets */
      break;
    case 0:                   /* bad input */
    default:
      error ("unexpected return (%d) from gdxSymbolGetDomainX", rc);
    } /* switch */
    break;
  default:
    error ("mkXPFilter: symbol %s has type %d (%s): unimplemented\n",
           symName, symType, gmsGdxTypeText[symType]);
  } /* switch (symType) */

  return;
} /* mkXPFilter */

/* prepHPFilter: prep/check a high-performance filter prior to use
 * This is not initializing data, just initializing prevPos
 * and perhaps some debugging-type checks on consistency
 */
void
prepHPFilter (int symDim, hpFilter_t filterList[])
{
  int iDim;
  hpFilter_t *hpf;

  if (NULL == filterList)
    error ("internal error: NULL hpFilter");
  for (iDim = 0;  iDim < symDim;  iDim++) {
    hpf = filterList + iDim;
    switch (hpf->fType) {
    case unset:
      error ("internal error: hpFilter type unset");
      break;
    case identity:
      error ("internal error: just sanity checking");
      break;
    case integer:
      if (hpf->n <= 0)
        error ("internal error: integer hpFilter must be nonempty"); /* really? */
      hpf->prevPos = 0;
      break;
    default:
      error ("internal error: unknown hpFilter type");
    }
  } /* loop over symbol dimensions */
} /* prepHPFilter */

/* findInHPFilter: search for inUels in filterList,
 * storing 1 + the index where found in outIdx[k], i.e. index in [1,filterList[k].n]
 * as a side effect, updates previous search info in filterList
 * return:
 *   1     if found,
 *   0     otherwise
 */
int
findInHPFilter (int symDim, const int inUels[], hpFilter_t filterList[],
                int outIdx[])
{
  int iDim, k, targetUel, found, isOrdered;
  const int *idx;
  hpFilter_t *hpf;

  if (NULL == filterList)
    error ("internal error: NULL hpFilter");
  for (iDim = 0;  iDim < symDim;  iDim++) {
    hpf = filterList + iDim;
    switch (hpf->fType) {
    case unset:
      error ("internal error: hpFilter type unset");
      break;
    case identity:
      outIdx[iDim] = inUels[iDim];
      break;
    case integer:
      idx = hpf->idx;
      isOrdered = hpf->isOrdered;
      targetUel = inUels[iDim];
      found = 0;
      if (isOrdered) {
        /* search starting with previous found position */
        for (k = hpf->prevPos;  k < hpf->n;  k++) {
          if (idx[k] == targetUel) {
            hpf->prevPos = k;
            outIdx[iDim] = k + 1;
            found = 1;
            break;
          }
          else if (idx[k] > targetUel)
            break;
        } /* loop over filter elements */
        if (! found) {          /* search from the beginning */
          for (k = 0;  k < hpf->n;  k++) {
            if (idx[k] == targetUel) {
              hpf->prevPos = k;
              outIdx[iDim] = k + 1;
              found = 1;
              break;
            }
            else if (idx[k] > targetUel)
              break;
          } /* loop over filter elements */
        }
      } /* if (isOrdered) */
      else {
        for (k = 0;  k < hpf->n;  k++) {
          if (idx[k] == targetUel) {
            hpf->prevPos = k;
            outIdx[iDim] = k + 1;
            found = 1;
            break;
          }
        } /* loop over filter elements */
      } /* if (isOrdered) .. else .. */
      if (! found)
        return 0;
      break;
    default:
      error ("internal error: unknown hpFilter type");
    }
  } /* loop over symbol dimensions */
  return 1;                     /* found */
} /* findInHPFilter */

/* findInXPFilter: search for inUels in filterList,
 * storing 1 + the index where found in outIdx[k], i.e. index in [1,filterList[k].n]
 * as a side effect, updates previous search info in filterList
 * return:
 *   0     if found,
 *   iDim  otherwise, where iDim in [1..symDim] is the leftmost bad index pos
 * similar to findInHPFilter, but XPFilter assumes
 * the filters and the inputs are ordered
 */
int
findInXPFilter (int symDim, const int inUels[], xpFilter_t filterList[],
                int outIdx[])
{
  int iDim, k, targetUel, stop;
  const int *idx;
  xpFilter_t *xpf;
  int reset = 0;                /* the left-most first moving index
                                 * resets all searches to the right */

  if (NULL == filterList)
    error ("internal error: NULL xpFilter");
  for (iDim = 0;  iDim < symDim;  iDim++) {
    xpf = filterList + iDim;
    switch (xpf->fType) {
    case unset:
      error ("internal error: xpFilter type unset");
      break;
    case identity:
      outIdx[iDim] = inUels[iDim];
      if (reset) {
        xpf->prevPos = inUels[iDim];
      }
      else {
        if      (xpf->prevPos < inUels[iDim]) {
          /* advanced in this index position */
          reset = 1;
          xpf->prevPos = inUels[iDim];
        }
        else if (xpf->prevPos == inUels[iDim])
          ;                     /* no change */
        else {
          /* moving back in this index position */
          return iDim + 1;
        }
      }
      break;
    case integer:
      idx = xpf->idx;
      targetUel = inUels[iDim];
      if (! reset) {
        /* optimize for the easy & common case: a repeat */
        if (idx[xpf->prevPos] == targetUel) {
          outIdx[iDim] = xpf->prevPos + 1;
          break;                /* from switch */
        }
        reset = 1;
        /* search starting one past the previous found position */
        for (k = xpf->prevPos + 1, stop = xpf->n;  k < stop;  k++) {
          if (idx[k] == targetUel) {
            xpf->prevPos = k;
            outIdx[iDim] = k + 1;
            break;              /* from for loop */
          }
          else if (idx[k] > targetUel) {
            return iDim + 1;
          }
        } /* loop over filter elements */
        if (k < stop)
          break;                /* from switch stmt */
        return iDim + 1;        /* not found??  bad! */
      }
      else {
        /* search was reset, must start from the beginning */
        ;
        for (k = 0, stop = xpf->n;  k < stop;  k++) {
          if (idx[k] == targetUel) {
            xpf->prevPos = k;
            outIdx[iDim] = k + 1;
            break;              /* from for loop */
          }
          else if (idx[k] > targetUel)
            return iDim + 1;
        } /* loop over filter elements */
        if (k < stop)
          break;                /* from switch stmt */
        return iDim + 1;        /* not found??  bad! */
      }
      break;
    default:
      error ("internal error: unknown hpFilter type");
    }
  } /* loop over symbol dimensions */
  return 0;                     /* found */
} /* findInXPFilter */

/* xpFilterToUels
 * create output uels (i.e. $uels) for an xpFilter
 */
void
xpFilterToUels (int symDim, xpFilter_t filterList[], SEXP uni, SEXP uels)
{
  int UELUserMapping;
  int iDim, n, k, iUEL;
  shortStringBuf_t uelName;
  SEXP s;
  xpFilter_t *xpf;

  for (iDim = 0;  iDim < symDim;  iDim++) {
    xpf = filterList + iDim;
    switch (xpf->fType) {
    case identity:
      SET_VECTOR_ELT(uels, iDim, uni);
      break;
    case integer:
      n = xpf->n;
      PROTECT(s = allocVector(STRSXP, n));
      for (k = 0;  k < n;  k++) {
        iUEL = xpf->idx[k];
        if (!gdxUMUelGet (gdxHandle, iUEL, uelName, &UELUserMapping)) {
          error("xpFilterToUels: could not gdxUMUelGet");
        }
        SET_STRING_ELT(s, k, mkChar(uelName));
      } /* loop over filter elements */
      SET_VECTOR_ELT(uels, iDim, s);
      UNPROTECT(1);
      break;
    default:
      error ("internal error creating output uels");
    }
  } /* loop over indices */
  return;
} /* xpFilterToUels */

/* getDomainNames: get domain names for symbol symIdx */
void
getDomainNames (int symIdx, Rboolean useDomInfo,
                SEXP outDomains, int *domInfoCode)
{
  gdxStrIndex_t domNames;
  gdxStrIndexPtrs_t domPtrs;
  shortStringBuf_t symName;
  int iDim, symDim, symType;
  int rc;

  GDXSTRINDEXPTRS_INIT (domNames, domPtrs);
  /* gdxSymbolGetDomainX is/was buggy and might not write the domain names
   * set to '*' to work around that */
  for (iDim = 0;  iDim < GLOBAL_MAX_INDEX_DIM;  iDim++)
    strcpy (domPtrs[iDim], "*");
  *domInfoCode = 0;             /* NA/unused */
  
  rc = gdxSymbolInfo (gdxHandle, symIdx, symName, &symDim, &symType);
  if (! rc)
    error ("bad return from gdxSymbolInfo in getDomainNames");
  if (symDim <= 0)
    return;                     /* skip scalars */

  switch (symType) {
  case GMS_DT_PAR:
  case GMS_DT_SET:
  case GMS_DT_VAR:
  case GMS_DT_EQU:
    if (useDomInfo) {
      *domInfoCode = rc = gdxSymbolGetDomainX (gdxHandle, symIdx, domPtrs);
    }
    else
      rc = 1;                   /* no domain info available */
    switch (rc) {
    case 1:                     /* nothing available */
      for (iDim = 0;  iDim < symDim;  iDim++) {
        SET_STRING_ELT(outDomains, iDim, mkChar("*"));
      }
      break;
    case 2:                   /* relaxed domain info */
    case 3:                   /* full domain info */
      for (iDim = 0;  iDim < symDim;  iDim++) {
        SET_STRING_ELT(outDomains, iDim, mkChar(domPtrs[iDim]));
      } /* end loop over dims */
      break;
    case 0:                   /* bad input */
    default:
      error ("unexpected return (%d) from gdxSymbolGetDomainX", rc);
    } /* switch */
    break;
  default:
    error ("mkXPFilter: symbol %s has type %d (%s): unimplemented\n",
           symName, symType, gmsGdxTypeText[symType]);
  } /* switch (symType) */

  return;
} /* getDomainNames */

/* This method will read variable "gamso" from R workspace */
char *getGlobalString (const char *globName, shortStringBuf_t result)
{
  SEXP gamso, lstNames, tmp;
  char *res;
  int k, infields, found;

  *result = '\0';
  res = NULL;
  if (gamsoIsUnset)
    return res;

  gamso = findVar (install("gamso"), R_GlobalEnv);

  if (gamso == NULL || TYPEOF(gamso) != VECSXP) {
    gamsoIsUnset = 1;
    globalGams = 0;
    return res;
  }

  if (globalGams == 1) {
    lstNames = getAttrib (gamso, R_NamesSymbol);
    infields = length(gamso);
    /* Checking if field data is for "globName" */
    for (found = 0, k = 0;  k < infields;  k++) {
      if (strcmp(globName, CHAR(STRING_ELT(lstNames, k))) == 0) {
        found = 1;
        break;
      }
    }

    if (found) {
      tmp = VECTOR_ELT(gamso, k);
      if (TYPEOF(tmp) == STRSXP) {
        checkStringLength (CHAR(STRING_ELT(tmp, 0)));
        res = CHAR2ShortStr (CHAR(STRING_ELT(tmp, 0)), result);
      }
      else {
        warning("To change default behavior of %s, please enter it as string.\n", globName);
        Rprintf("You entered it as %d.\n", TYPEOF(tmp));
        return NULL;
      }
    }
  }
  return res;
} /* getGlobalString */

/* getNonDefaultElemCount
 * return count of non-default elements for the specified field
 * of the symbol referenced by symIdx
 */
int
getNonDefaultElemCount (gdxHandle_t h, int symIdx,
                        int symType, int symSubType, dField_t dField)
{
  double defVal;                /* default value - may be nonzero */
  int nRecs, changeIdx, i, cnt;
  gdxUelIndex_t uels;
  gdxValues_t values;

  defVal = 0;
  switch (symType) {
  case GMS_DT_VAR:
    defVal = getDefValVar (symSubType, dField);
    break;
  case GMS_DT_EQU:
    defVal = getDefValEqu (symSubType, dField);
    break;
  } /* end switch */

  gdxDataReadRawStart (h, symIdx, &nRecs);
  if (all == dField) {
    return nRecs;
  }
  for (cnt = 0, i = 0;  i < nRecs;  i++) {
    gdxDataReadRaw (h, uels, values, &changeIdx);
    if (values[dField] != defVal) {
      cnt++;
    }
  }
  return cnt;
} /* getNonDefaultElemCount */

/* getNonZeroElements
 * return nonzero count for the specified field of a variable or equation
 */
int
getNonZeroElements (gdxHandle_t h, int symIdx, dField_t dField)
{
  int nRecs, changeIdx, i, cnt;
  gdxUelIndex_t uels;
  gdxValues_t values;

  gdxDataReadRawStart (h, symIdx, &nRecs);
  if (all == dField) {
    return nRecs;
  }
  for (cnt = 0, i = 0;  i < nRecs;  i++) {
    gdxDataReadRaw (h, uels, values, &changeIdx);
    if (values[dField] != 0) {
      cnt++;
    }
  }
  return cnt;
} /* getNonZeroElements */

/* get option gdx.inventSetText */
Rboolean
getInventSetText (Rboolean defVal)
{
  SEXP o = GetOption1(install("gdx.inventSetText"));

  if (R_NilValue == o)
    return defVal;
  if (LGLSXP == TYPEOF(o))
    return LOGICAL(o)[0];
  return asLogical(o);
} /* getInventSetText */

/* interpret an expression (probably an input arg) as a logical/boolean */
Rboolean
exp2Boolean (SEXP exp)
{
  const char *s;

  switch (TYPEOF(exp)) {
  case LGLSXP:
    return LOGICAL(exp)[0];
    break;
  case INTSXP:
    return INTEGER(exp)[0];
    break;
  case REALSXP:
    if (0.0 == REAL(exp)[0])
      return FALSE;
    else
      return TRUE;
    break;
  case STRSXP:
    s = CHAR(STRING_ELT(exp, 0));
    if ('\0' == s[1])
      switch (s[0]) {
      case 'T':
      case 't':
      case 'Y':
      case 'y':
      case '1':
        return TRUE;
      case 'F':
      case 'f':
      case 'N':
      case 'n':
      case '0':
        return FALSE;
      default:
        return NA_LOGICAL;
      }
    if (0 == strcmp("TRUE",s)) return TRUE;
    if (0 == strcmp("True",s)) return TRUE;
    if (0 == strcmp("true",s)) return TRUE;
    if (0 == strcmp("YES",s)) return TRUE;
    if (0 == strcmp("Yes",s)) return TRUE;
    if (0 == strcmp("yes",s)) return TRUE;

    if (0 == strcmp("FALSE",s)) return FALSE;
    if (0 == strcmp("False",s)) return FALSE;
    if (0 == strcmp("false",s)) return FALSE;
    if (0 == strcmp("NO",s)) return FALSE;
    if (0 == strcmp("No",s)) return FALSE;
    if (0 == strcmp("no",s)) return FALSE;

    return NA_LOGICAL;
    break;
  }
  return NA_LOGICAL;
} /* exp2Boolean */

/* this method for global input "compress" */
int isCompress (void)
{
  SEXP gamso, tmp, lstName;
  Rboolean logical = NA_LOGICAL;
  char *str;
  int i, infields;
  int compress = 0;
  int found = 0;
  shortStringBuf_t fName;

  str = NULL;
  gamso = findVar( install("gamso"), R_GlobalEnv );

  if (gamso == NULL || TYPEOF(gamso) == NILSXP  ||  TYPEOF(gamso) == SYMSXP) {
    globalGams = 0;
    return 0;
  }

  /*   if (TYPEOF(gamso) != VECSXP  && globalGams)
       {
       warning("To change default behavior, please enter 'gamso' as list.\n" );
       Rprintf("You entered it as %d.\n", TYPEOF(gamso) );
       globalGams = 0;
       return 0;
       } */

  else if (TYPEOF(gamso) == VECSXP && globalGams == 1) {
    lstName = getAttrib(gamso, R_NamesSymbol);
    i=0;
    infields = length(gamso);
    /* Checking if field data is for "name" */
    for (i = 0; i < infields; i++) {
      if (strcasecmp("compress", CHAR(STRING_ELT(lstName, i))) == 0) {
        found = 1;
        break;
      }
    }

    if (found == 1 && globalGams) {
      tmp = VECTOR_ELT(gamso, i);
      if (TYPEOF(tmp) == STRSXP) {
        str = CHAR2ShortStr (CHAR(STRING_ELT(tmp, 0)), fName);
        if (NULL != str) {
          if (strcasecmp(fName,"true") == 0)
            compress = 1;
          else if (strcmp(fName,"false") == 0)
            compress = 0;
          else {
            /* else warning message */
            warning ("To change default behavior of 'compress', please enter it as 'true' or 'false'\n" );
            Rprintf ("You entered it as %s.\n", str);
          }
        }
      } /* TYPEOF=STRSXP */
      else if (TYPEOF(tmp) == LGLSXP) {
        logical = LOGICAL(tmp)[0];
        if (logical == TRUE)
          compress = 1;
        else
          compress = 0;
      }
      else {
        warning ("To change default behavior of 'compress', please enter it as either a string or a logical.\n");
        Rprintf ("You entered it with TYPEOF('compress') = %d.\n", TYPEOF(tmp));
        return 0;
      }
    }
  }
  return compress;
} /* isCompress */

/* loadGDX: load the GDX API, if not already loaded,
 * and raise an exception on failure
 */
void
loadGDX (void)
{
  shortStringBuf_t sysDir, msg;
  int rc;

  if (gdxLibraryLoaded())
    return;                     /* already loaded */

  /* try R_GAMS_SYSDIR */
  rc = getEnvVar ("R_GAMS_SYSDIR", sysDir);
  if ((0 == rc) && (strlen(sysDir) > 0)) {
    rc = gdxGetReadyD (sysDir, msg, sizeof(msg));
  }
  else
    sysDir[0] = '\0';
  if (gdxLibraryLoaded())
    return;                     /* nailed it */

  rc = gdxGetReady (msg, sizeof(msg));
  if (0 == rc) {
    error ("Error loading the GDX API:"
             " use igdx() to diagnose and solve the problem\n");
  }
  return;
} /* loadGDX */

/* makeStrVec
 * converts the input vector of ints or reals into strings
 * both inExp and outExp are assumed to be allocated on input
 */
void
makeStrVec (SEXP outExp, SEXP inExp)
{
  int  len;
  char buf[256];
  double *doubleData;
  int *intData;
  int k;

  len = length(inExp);
  if (TYPEOF(inExp) == REALSXP) {
    doubleData = REAL(inExp);
    for (k = 0; k < len; k++) {
      sprintf(buf, "%g", doubleData[k]);
      SET_STRING_ELT(outExp, k, mkChar(buf));
    }
  }
  else if (TYPEOF(inExp) == INTSXP) {
    intData = INTEGER(inExp);
    for (k = 0; k < len; k++) {
      sprintf(buf, "%i", intData[k]);
      SET_STRING_ELT(outExp, k, mkChar(buf));
    }
  }
  else if (TYPEOF(inExp) == STRSXP) {
    for (k = 0; k < len; k++) {
      SET_STRING_ELT(outExp, k, duplicate(STRING_ELT(inExp, k)));
    }
  }

  checkForDuplicates (outExp);

  return;
} /* makeStrVec */

/* sparseToFull: from input data in sparse form, create output data in full form
 * spVal: input .val matrix in sparse form
 * fullVal: output .val matrix in full form
 * uelLists: .uels for symbol
 * N.B.: R stores matrices column-wise, i.e. left index moving fastest
 */
void
sparseToFull (SEXP spVal, SEXP fullVal, SEXP uelLists,
              int symType, int symSubType, dField_t dField, int nRec, int symDimX)
{
  int k, iRec;
  int fullLen;             /* length of output matrix fullVal */
  int fullCard;            /* cardinality of fully allocated matrix */
  int card[GLOBAL_MAX_INDEX_DIM];
  double defVal;           /* default value - may be nonzero */
  double *p, *pFull, *tFull;
  int index;
  int symDim = symDimX;
  int ii;
  dField_t iField;

  pFull = REAL(fullVal);
  fullLen = length(fullVal);
  p = REAL(spVal);

  switch (symType) {
  case GMS_DT_SET:
    /* step 1: initialize full matrix */
    (void) memset (pFull, 0, fullLen * sizeof(*pFull));
    /* step 2: loop over each row/nonzero of sparse matrix to populate full matrix */
    fullCard = 1;
    for (k = 0;  k < symDim;  k++) {
      card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
      fullCard *= card[k];
    }
    if (fullCard != fullLen)
      error ("sparseToFull: unexpected inputs:  fullCard=%d  fullLen=%d",
             fullCard, fullLen);

    for (iRec = 0;  iRec < nRec;  iRec++) {
      ii = iRec + nRec*(symDim-1);
      for (index = p[ii]-1, k = symDim-2;  k >= 0;  k--) {
        ii -= nRec;
        index = (index * card[k]) + p[ii] - 1;
      }
      pFull[index] = 1;
    } /* end loop over nonzeros */
    break;
  case GMS_DT_PAR:
    /* step 1: initialize full matrix */
    (void) memset (pFull, 0, fullLen * sizeof(*pFull));
    /* step 2: loop over each row/nonzero of sparse matrix to populate full matrix */
    fullCard = 1;
    for (k = 0;  k < symDim;  k++) {
      card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
      fullCard *= card[k];
    }
    if (fullCard != fullLen)
      error ("sparseToFull: unexpected inputs:  fullCard=%d  fullLen=%d",
             fullCard, fullLen);

    for (iRec = 0;  iRec < nRec;  iRec++) {
      ii = iRec + nRec*(symDim-1);
      for (index = p[ii]-1, k = symDim-2;  k >= 0;  k--) {
        ii -= nRec;
        index = (index * card[k]) + p[ii] - 1;
      }
      pFull[index] = p[iRec + nRec*symDim];
    } /* end loop over nonzeros */
    break;
  case GMS_DT_VAR:
    if (all == dField) {
      double defRec[GMS_VAL_MAX];

      symDim--;
      fullCard = 1;
      for (k = 0;  k < symDim;  k++) {
        card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
        fullCard *= card[k];
      }
      if ((fullCard * 5) != fullLen)
        error ("sparseToFull: unexpected inputs:  fullCard*5=%d  fullLen=%d",
               fullCard*5, fullLen);

      /* step 1: initialize full matrix to the defaults */
      getDefRecVar (symSubType, defRec);
      for (tFull = pFull, iField = level;  iField <= scale;  iField++) {
        if (0 == defRec[iField])
          (void) memset (tFull, 0, fullCard * sizeof(*pFull));
        else {
          for (k = 0;  k < fullCard;  k++)
            tFull[k] = defRec[iField];
        }
        tFull += fullCard;
      }
      /* step 2: loop over each record of the variable to plug in non-defaults */
      for (iRec = 0;  iRec < nRec;  iRec++) {
        ii = iRec + nRec*symDim;
        for (index = p[ii]-1, k = symDim-1;  k >= 0;  k--) {
          ii -= nRec;
          index = (index * card[k]) + p[ii] - 1;
        }
        pFull[index] = p[iRec + nRec*symDimX];
      } /* end loop over nonzeros */
    }
    else {                      /* all != dField */
      /* step 1: initialize full matrix */
      defVal = getDefValVar (symSubType, dField);
      if (0 == defVal) {
        (void) memset (pFull, 0, fullLen * sizeof(*pFull));
      }
      else {
        for (k = 0;  k < fullLen;  k++)
          pFull[k] = defVal;
      }
      /* step 2: loop over each row/nonzero of sparse matrix to populate full matrix */
      fullCard = 1;
      for (k = 0;  k < symDim;  k++) {
        card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
        fullCard *= card[k];
      }
      if (fullCard != fullLen)
        error ("sparseToFull: unexpected inputs:  fullCard=%d  fullLen=%d",
               fullCard, fullLen);

      for (iRec = 0;  iRec < nRec;  iRec++) {
        ii = iRec + nRec*(symDim-1);
        for (index = p[ii]-1, k = symDim-2;  k >= 0;  k--) {
          ii -= nRec;
          index = (index * card[k]) + p[ii] - 1;
        }
        pFull[index] = p[iRec + nRec*symDim];
      } /* end loop over nonzeros */
    } /* if all == dField .. else .. */
    break;
  case GMS_DT_EQU:
    if (all == dField) {
      double defRec[GMS_VAL_MAX];

      symDim--;
      fullCard = 1;
      for (k = 0;  k < symDim;  k++) {
        card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
        fullCard *= card[k];
      }
      if ((fullCard * 5) != fullLen)
        error ("sparseToFull: unexpected inputs:  fullCard*5=%d  fullLen=%d",
               fullCard*5, fullLen);

      /* step 1: initialize full matrix to the defaults */
      getDefRecEqu (symSubType, defRec);
      for (tFull = pFull, iField = level;  iField <= scale;  iField++) {
        if (0 == defRec[iField])
          (void) memset (tFull, 0, fullCard * sizeof(*pFull));
        else {
          for (k = 0;  k < fullCard;  k++)
            tFull[k] = defRec[iField];
        }
        tFull += fullCard;
      }
      /* step 2: loop over each record of the equation to plug in non-defaults */
      for (iRec = 0;  iRec < nRec;  iRec++) {
        ii = iRec + nRec*symDim;
        for (index = p[ii]-1, k = symDim-1;  k >= 0;  k--) {
          ii -= nRec;
          index = (index * card[k]) + p[ii] - 1;
        }
        pFull[index] = p[iRec + nRec*symDimX];
      } /* end loop over nonzeros */
      /* error  ("not yet implemented YY"); */
    }
    else {                      /* all != dField */
      /* step 1: initialize full matrix */
      defVal = getDefValEqu (symSubType, dField);
      if (0 == defVal) {
        (void) memset (pFull, 0, fullLen * sizeof(*pFull));
      }
      else {
        for (k = 0;  k < fullLen;  k++)
          pFull[k] = defVal;
      }
      /* step 2: loop over each row/nonzero of sparse matrix to populate full matrix */
      fullCard = 1;
      for (k = 0;  k < symDim;  k++) {
        card[k] = length(VECTOR_ELT(uelLists, k)); /* number of elements in dim k */
        fullCard *= card[k];
      }
      if (fullCard != fullLen)
        error ("sparseToFull: unexpected inputs:  fullCard=%d  fullLen=%d",
               fullCard, fullLen);

      for (iRec = 0;  iRec < nRec;  iRec++) {
        ii = iRec + nRec*(symDim-1);
        for (index = p[ii]-1, k = symDim-2;  k >= 0;  k--) {
          ii -= nRec;
          index = (index * card[k]) + p[ii] - 1;
        }
        pFull[index] = p[iRec + nRec*symDim];
      } /* end loop over nonzeros */
    } /* if all == dField .. else .. */
    break;
  default:
    error("Unrecognized type of symbol found.");
  } /* end switch */

  return;
} /* sparseToFull */

/* getDefRecEqu: return the default record for an equation of type subType
 * Treat unrecognized subType like GMS_EQUTYPE_N, that seems to be the default
 */
void
getDefRecEqu (int subType, double defRec[])
{
  (void) memset (defRec, 0, GMS_VAL_MAX * sizeof(double));
  defRec[scale] = 1;
  switch (subType) {
  case GMS_EQUTYPE_E:
  case GMS_EQUTYPE_X:
    break;
  case GMS_EQUTYPE_G:
    defRec[upper] = R_PosInf;
    break;
  case GMS_EQUTYPE_L:
    defRec[lower] = R_NegInf;
    break;
  case GMS_EQUTYPE_C:
    defRec[upper] = R_PosInf;
    break;
  case GMS_EQUTYPE_N:
  default:
    defRec[upper] = R_PosInf;
    defRec[lower] = R_NegInf;
  } /* switch subType */
  return;
} /* getDefRecEqu */

/* getDefRecVar: return the default record for a variable of type subType */
void
getDefRecVar (int subType, double defRec[])
{
  (void) memset (defRec, 0, GMS_VAL_MAX * sizeof(double));
  defRec[scale] = 1;
  switch (subType) {
  case GMS_VARTYPE_BINARY:
    defRec[upper] = 1;
    break;
  case GMS_VARTYPE_INTEGER:
    defRec[upper] = 100;
    break;
  case GMS_VARTYPE_POSITIVE:
  case GMS_VARTYPE_SOS1:
  case GMS_VARTYPE_SOS2:
    defRec[upper] = R_PosInf;
    break;
  case GMS_VARTYPE_NEGATIVE:
    defRec[lower] = R_NegInf;
    break;
  case GMS_VARTYPE_FREE:
    defRec[lower] = R_NegInf;
    defRec[upper] = R_PosInf;
    break;
  case GMS_VARTYPE_SEMICONT:
    defRec[lower] = 1;
    defRec[upper] = R_PosInf;
    break;
  case GMS_VARTYPE_SEMIINT:
    defRec[lower] = 1;
    defRec[upper] = 100;
    break;
  } /* switch subType */
  return;
} /* getDefRecVar */

/* getDefValEqu: return the default value for field dField of an equation
 * of type subType */
double
getDefValEqu (int subType, dField_t dField)
{
  if (all == dField)
    error ("dField = all passed to getDefValEqu: internal error");

  if (scale == dField)
    return 1;

  switch (subType) {
  case GMS_EQUTYPE_E:
  case GMS_EQUTYPE_X:
    /* all 0 */
    break;
  case GMS_EQUTYPE_G:
  case GMS_EQUTYPE_C:
    if (upper == dField)
      return R_PosInf;
    break;
  case GMS_EQUTYPE_L:
    if (lower == dField)
      return R_NegInf;
    break;
  case GMS_EQUTYPE_N:
    if (upper == dField)
      return R_PosInf;
    if (lower == dField)
      return R_NegInf;
    break;
  } /* switch subType */
  return 0;
} /* getDefValEqu */

/* getDefValVar: return the default value for field dField of a variable
 * of type subType */
double
getDefValVar (int subType, dField_t dField)
{
  if (all == dField)
    error ("dField = all passed to getDefValVar: internal error");

  if (scale == dField)
    return 1;

  switch (subType) {
  case GMS_VARTYPE_BINARY:
    if (upper == dField)
      return 1;
    break;
  case GMS_VARTYPE_INTEGER:
    if (upper == dField)
      return 100;
    break;
  case GMS_VARTYPE_POSITIVE:
  case GMS_VARTYPE_SOS1:
  case GMS_VARTYPE_SOS2:
    if (upper == dField)
      return R_PosInf;
    break;
  case GMS_VARTYPE_NEGATIVE:
    if (lower == dField)
      return R_NegInf;
    break;
  case GMS_VARTYPE_FREE:
    if (lower == dField)
      return R_NegInf;
    if (upper == dField)
      return R_PosInf;
    break;
  case GMS_VARTYPE_SEMICONT:
    if (lower == dField)
      return 1;
    if (upper == dField)
      return R_PosInf;
    break;
  case GMS_VARTYPE_SEMIINT:
    if (lower == dField)
      return 1;
    if (upper == dField)
      return 100;
    break;
  } /* switch subType */
  return 0;
} /* getDefValVar */

/* getDefVal: return the default value consistent with the given
 *   symType (e.g. GMS_DT_VAR),
 *   subType (e.g. GMS_VARTYPE_BINARY), and
 *   dField  (e.g. lower)
 */
double
getDefVal (int symType, int subType, dField_t dField)
{
  double defVal = 0;

  switch (symType) {
  case GMS_DT_SET:    /* just choose something not to match the val */
    defVal = -1;
    break;
  case GMS_DT_VAR:
    defVal = getDefValVar (subType, dField);
    break;
  case GMS_DT_EQU:
    defVal = getDefValEqu (subType, dField);
    break;
  } /* end switch */
  return defVal;
} /* getDefVal */

/* addDomInfo: add relaxed domain info for a symbol to a GDX file
 */
void
addDomInfo (const char *symName, SEXP domExp, SEXP domInfoExp)
{
  int rc, k, nDoms, symDim, symType, symIdx;
  char dummy[GMS_SSSIZE];
  const char *domName;
  const char *tmpName;
  gdxStrIndex_t domNames;
  gdxStrIndexPtrs_t domPtrs;

  if (domInfoExp) {
    /* verified it's a string prior to this */
    tmpName = CHAR(STRING_ELT(domInfoExp, 0));
    /* if domInfo is specified, it must indicate the domain info is good */
    if ((0 == strcmp (tmpName, "compressed")) ||
        (0 == strcmp (tmpName, "filtered")) ||
        (0 == strcmp (tmpName, "full")) ||
        (0 == strcmp (tmpName, "relaxed"))
        )
      ;                         /* no return */
    else
      /* any other value implies domExp is sketchy, don't use it */
      return;
  }

  if (domExp) {
    // Rprintf ("DEBUG: found domains for symbol %s\n", symName);
  }
  else {
    return;                /* nothing to do if no domains available */
  }
  GDXSTRINDEXPTRS_INIT (domNames, domPtrs);
  rc = gdxFindSymbol (gdxHandle, symName, &symIdx);
  if (! rc)
    error ("Error writing domain info for symbol '%s':"
           " not found in GDX", symName);
  rc = gdxSymbolInfo (gdxHandle, symIdx, dummy, &symDim, &symType);
  if (! rc)
    error ("Error writing domain info for symbol '%s':"
           " gdxSymbolInfo call failed", symName);

  /* just in case, set a solid default */
  for (k = 0;  k < GLOBAL_MAX_INDEX_DIM;  k++)
    strcpy (domPtrs[k], "*");

  nDoms = length(domExp);
  if (nDoms > symDim)
    nDoms = symDim;
  for (k = 0;  k < nDoms;  k++) {
    domName = CHAR(STRING_ELT(domExp, k));
    // Rprintf ("  %s\n", domName);
    strcpy (domPtrs[k], domName);
  }
  rc = gdxSymbolSetDomainX (gdxHandle, symIdx, (const char **) domPtrs);
  return;
} /* addDomInfo */

/* show the platform-dependent shared library search path */
void
showLibSearchPath (void)
{
#if defined(_WIN32)
  const char evName[] = "PATH";
  char *s;
  size_t len;

  len = GetEnvironmentVariable (evName, NULL, 0);
  if (0 == len)
    Rprintf ("%s is not set!\n", evName);
  else {
    s = (char *) malloc(len);
    if (NULL == s)
      Rprintf ("%s could not be read: malloc failure!\n", evName);
    else {
      (void) GetEnvironmentVariable (evName, s, len);
      Rprintf ("%s = %s\n", evName, s);
      free(s);
    }
  }

#else
# if defined(__APPLE__)
  const char evName[] = "DYLD_LIBRARY_PATH";
# else
  const char evName[] = "LD_LIBRARY_PATH";
# endif
  char *s;

  s = getenv (evName);
  if (NULL == s)
    Rprintf ("%s is not set!\n", evName);
  else
    Rprintf ("%s = %s\n", evName, s);
#endif
} /* showLibSearchPath */

/* get value of environment variable evName
 * return 0 if evName was set, ~0 otherwise
 */
int
getEnvVar (const char *evName, shortStringBuf_t evVal)
{
#if defined(_WIN32)
  char *s;
  size_t len;

  evVal[0] = '\0';
  len = GetEnvironmentVariable (evName, NULL, 0);
  if (0 == len)
    return 1;
  else {
    s = (char *) malloc(len);
    if (NULL == s)
      return 1;
    else {
      (void) GetEnvironmentVariable (evName, s, len);
      (void) CHAR2ShortStr (s, evVal);
      free(s);
    }
  }

#else
  char *s;

  evVal[0] = '\0';
  s = getenv (evName);
  if (NULL == s)
    return 1;                   /* not set */
  else
    (void) CHAR2ShortStr (s, evVal);
#endif

  return 0;
} /* getEnvVar */
