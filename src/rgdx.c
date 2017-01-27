/* rgdx.c
 * code for gdxrrw::rgdx
 * $Id: rgdx.c 51955 2015-04-30 14:56:55Z sdirkse $
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

/* checkRgdxList: checks the input request list for valid data
 * and updates the read specifier
 */
static void
checkRgdxList (const SEXP lst, rSpec_t *rSpec, int *protectCnt)
{
  SEXP lstNames, tmpUel;
  SEXP bufferUel;
  SEXP compressExp = NULL;      /* from input requestList */
  SEXP dimExp = NULL;           /* from input requestList */
  SEXP fieldExp = NULL;         /* from input requestList */
  SEXP formExp = NULL;          /* from input requestList */
  SEXP nameExp = NULL;          /* from input requestList */
  SEXP teExp = NULL;            /* from input requestList */
  SEXP tsExp = NULL;            /* from input requestList */
  SEXP uelsExp = NULL;          /* from input requestList */
  int i, j;
  int nElements;                /* number of elements in lst */
  const char *tmpName;
  const char *elmtName;         /* list element name */
  Rboolean compress = NA_LOGICAL;

  nElements = length(lst);
  /* check maximum number of elements */
  if (nElements < 1 || nElements > 8) {
    error("Incorrect number of elements in input list argument.");
  }

  lstNames = getAttrib(lst, R_NamesSymbol);

  if (lstNames == R_NilValue) {
    Rprintf("Input list must be named\n");
    Rprintf("Valid names are: 'name', 'dim', 'uels', 'form', 'compress', 'field', 'te', 'ts'.\n");
    error("Please try again with named input list.");
  }

  /* first, check that all names are recognized, reject o/w
   * in the process, store the symbol for direct access later
   */
  for (i = 0;  i < nElements;  i++) {
    elmtName = CHAR(STRING_ELT(lstNames, i));
    if      (strcmp("compress", elmtName) == 0) {
      compressExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("dim", elmtName) == 0) {
      dimExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("field", elmtName) == 0) {
      fieldExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("form", elmtName) == 0) {
      formExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("name", elmtName) == 0) {
      nameExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("te", elmtName) == 0) {
      teExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("ts", elmtName) == 0) {
      tsExp = VECTOR_ELT(lst, i);
    }
    else if (strcmp("uels", elmtName) == 0) {
      uelsExp = VECTOR_ELT(lst, i);
    }
    else {
      Rprintf ("Input list elements for rgdx must be according to this specification:\n");
      Rprintf ("'name', 'dim', 'uels', 'form', 'compress', 'field', 'te', 'ts'.\n");
      error("Incorrect type of rgdx input list element '%s' specified.",
            elmtName);
    }
  }

  /* now process the fields provided */

  if (compressExp && (R_NilValue != compressExp)) {
    if (TYPEOF(compressExp) == STRSXP) {
      tmpName = CHAR(STRING_ELT(compressExp, 0));
      if (0 == strcasecmp("true", tmpName)) {
        rSpec->compress = 1;
      }
      else if (0 == strcasecmp("false", tmpName)) {
        rSpec->compress = 0;
      }
      else {
        error("Input list element 'compress' must be either 'true' or 'false'.");
      }
    }
    else if (TYPEOF(compressExp) == LGLSXP) {
      compress = LOGICAL(compressExp)[0];
      if (compress == TRUE) {
        rSpec->compress = 1;
      }
      else {
        rSpec->compress = 0;
      }
    }
    else {
      Rprintf ("List element 'compress' must be either string or logical - found %d instead\n",
               TYPEOF(compressExp));
      error("Input list element 'compress' must be either string or logical");
    }
  } /* if compressExp */

  if (dimExp && (R_NilValue != dimExp)) {
    if (INTSXP == TYPEOF(dimExp)) {
      if (length(dimExp) != 1) {
        error ("Optional input list element 'dim' must have only one element.");
      }
      if (INTEGER(dimExp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.");
      }
      rSpec->dim = INTEGER(dimExp)[0];
    }
    else if (REALSXP == TYPEOF(dimExp)) {
      if (length(dimExp) != 1) {
        error ("Optional input list element 'dim' must have only one element.");
      }
      if (REAL(dimExp)[0] < 0) {
        error("Negative value is not allowed as valid input for 'dim'.");
      }
      rSpec->dim = (int) REAL(dimExp)[0];
      if (REAL(dimExp)[0] != rSpec->dim) {
        error("Non-integer value is not allowed as valid input for 'dim'.");
      }
    }
    else {
      Rprintf ("List element 'dim' must be numeric - found %d instead\n",
               TYPEOF(dimExp));
      error ("Optional input list element 'dim' must be numeric.");
    }
  } /* dimExp */

  if (fieldExp && (R_NilValue != fieldExp)) {
    char fieldErrorMsg[] = "Input list element 'field' must be in"
      " ['l','m','lo','up','s','all'].";
    if (TYPEOF(fieldExp) != STRSXP ) {
      Rprintf ("List element 'field' must be a string - found %d instead\n",
               TYPEOF(fieldExp));
      error("Input list element 'field' must be string");
    }
    tmpName = CHAR(STRING_ELT(fieldExp, 0));
    if (strlen(tmpName) == 0) {
      error(fieldErrorMsg);
    }
    rSpec->withField = 1;
    if      (0 == strcasecmp("l", tmpName)) {
      rSpec->dField = level;
    }
    else if (0 == strcasecmp("m", tmpName)) {
      rSpec->dField = marginal;
    }
    else if (0 == strcasecmp("lo", tmpName)) {
      rSpec->dField = lower;
    }
    else if (0 == strcasecmp("up", tmpName)) {
      rSpec->dField = upper;
    }
    else if (0 == strcasecmp("s", tmpName)) {
      rSpec->dField = scale;
    }
    else if (0 == strcasecmp("all", tmpName)) {
      rSpec->dField = all;
    }
    else {
      error(fieldErrorMsg);
     }
  } /* if fieldExp */

  if (formExp && (R_NilValue != formExp)) {
    if (STRSXP != TYPEOF(formExp)) {
      Rprintf ("List element 'form' must be a string - found %d instead\n",
               TYPEOF(formExp));
      error ("Input list element 'form' must be string");
    }
    tmpName = CHAR(STRING_ELT(formExp, 0));
    if (strcasecmp("full", tmpName) == 0) {
      rSpec->dForm = full;
    }
    else if (strcasecmp("sparse", tmpName) == 0) {
      rSpec->dForm = sparse;
    }
    else {
      error("Input list element 'form' must be either 'full' or 'sparse'.");
    }
  } /* formExp */

  if (NULL == nameExp)
    error ("Required list element 'name' is missing. Please try again." );
  if (TYPEOF(nameExp) != STRSXP) {
    Rprintf ("List element 'name' must be a string - found %d instead\n",
             TYPEOF(nameExp));
    error("Input list element 'name' must be string.");
  }
  tmpName = CHAR(STRING_ELT(nameExp, 0));
  checkStringLength (tmpName);
  strcpy (rSpec->name, tmpName);

  if (teExp && (R_NilValue != teExp)) {
    if (TYPEOF(teExp) == STRSXP ) {
      tmpName = CHAR(STRING_ELT(teExp, 0));
      if (strlen(tmpName) == 0) {
        error("Input list element 'te' must be either 'true' or 'false'.");
      }
      if (0 == strcasecmp("true", tmpName)) {
        rSpec->te = 1;
      }
      else if (0 == strcasecmp("false", tmpName)) {
        rSpec->te = 0;
      }
      else {
        error("Input list element 'te' must be either 'true' or 'false'.");
      }
    }
    else if (TYPEOF(teExp) == LGLSXP) {
      if (LOGICAL(teExp)[0] == TRUE) {
        rSpec->te = 1;
      }
    }
    else {
      error("Input list element 'te' must be either string or logical"
            " - found %d instead", TYPEOF(teExp));
    }
  } /* teExp */

  if (tsExp && (R_NilValue != tsExp)) {
    if (TYPEOF(tsExp) == STRSXP ) {
      tmpName = CHAR(STRING_ELT(tsExp, 0));
      if (0 == strcasecmp("true", tmpName)) {
        rSpec->ts = 1;
      }
      else if (0 == strcasecmp("false", tmpName)) {
        rSpec->ts = 0;
      }
      else {
        error("Input list element 'ts' must be either 'true' or 'false'.");
      }
    }
    else if (TYPEOF(tsExp) == LGLSXP) {
      if (LOGICAL(tsExp)[0] == TRUE) {
        rSpec->ts = 1;
      }
    }
    else {
      Rprintf ("List element 'ts' must be either string or logical"
               " - found %d instead\n", TYPEOF(tsExp));
      error("Input list element 'ts' must be either string or logical");
    }
  } /* tsExp */

  if (uelsExp && (R_NilValue != uelsExp)) {
    if (TYPEOF(uelsExp) != VECSXP) {
      error("List element 'uels' must be a list.");
    }
    else {
      PROTECT(rSpec->filterUel = allocVector(VECSXP, length(uelsExp)));
      ++*protectCnt;
      for (j = 0; j < length(uelsExp); j++) {
        tmpUel = VECTOR_ELT(uelsExp, j);
        if (tmpUel == R_NilValue) {
          error("Empty Uel is not allowed");
        }
        else {
          PROTECT(bufferUel = allocVector(STRSXP, length(tmpUel)));
          /* Convert to output */
          makeStrVec (bufferUel, tmpUel);
          SET_VECTOR_ELT(rSpec->filterUel, j, bufferUel);
          UNPROTECT(1);         /* bufferUel */
        }
      }
      rSpec->withUel = 1;
    }
  } /* uelsExp */
} /* checkRgdxList */


/* aliasReturn: construct and return an expression representing an alias
 * You should do UNPROTECT(1) after calling this and immediately before return
 */
SEXP aliasReturn (const char *symName, const char *aliasFor)
{
  SEXP outListNames, outList, outName, outType, outFor;

  PROTECT(outList = allocVector(VECSXP, 3));
  PROTECT(outListNames = allocVector(STRSXP, 3));

  SET_STRING_ELT(outListNames, 0, mkChar("name"));
  PROTECT(outName = allocVector(STRSXP, 1));
  SET_STRING_ELT(outName, 0, mkChar(symName));
  SET_VECTOR_ELT(outList     , 0, outName);
  UNPROTECT(1);

  SET_STRING_ELT(outListNames, 1, mkChar("type"));
  PROTECT(outType = allocVector(STRSXP, 1));
  SET_STRING_ELT(outType, 0, mkChar("alias"));
  SET_VECTOR_ELT(outList     , 1, outType);
  UNPROTECT(1);

  SET_STRING_ELT(outListNames, 2, mkChar("aliasFor"));
  PROTECT(outFor = allocVector(STRSXP, 1));
  SET_STRING_ELT(outFor, 0, mkChar(aliasFor));
  SET_VECTOR_ELT(outList     , 2, outFor);
  UNPROTECT(1);

  setAttrib(outList, R_NamesSymbol, outListNames);
  UNPROTECT(1);                 /* outListNames */
  return outList;
} /* aliasReturn */

/* rgdx: gateway function for reading gdx, called from R via .External
 * first argument <- gdx file name
 * second argument <- requestList containing several elements
 * that make up a read specifier, e.g. symbol name, dim, form, etc
 * third argument <- squeeze specifier
 * fourth argument <- useDomInfo specifier
 * fifth argument <- followAlias specifier
 * ------------------------------------------------------------------ */
SEXP rgdxExt (SEXP args)
{
  const char *funcName = "rgdx";
  SEXP fileName, requestList, squeezeExp, udi, followAliasExp, universe;
  SEXP targs;
  Rboolean inventSetText = NA_LOGICAL;
#if 0
  Rboolean domainNames = FALSE; /* dataframe names() <--> domain info in GDX */
#endif
  SEXP outName = R_NilValue,
    outType = R_NilValue,
    outDim = R_NilValue,
    outValSp = R_NilValue,      /* output .val, sparse form */
    outValFull = R_NilValue,    /* output .val, full form */
    outForm = R_NilValue,
    outUels = R_NilValue,
    outField = R_NilValue,
    outTs = R_NilValue,
    outTeSp = R_NilValue,       /* output .te, sparse form */
    outTeFull = R_NilValue,     /* output .te, full form */
    outDomains = R_NilValue,    /* output $domains */
    outDomInfo = R_NilValue;    /* output $domInfo */
  int domInfoCode;
  int reuseFilter = 0;          /* reuse input filter for outUels */
  SEXP outListNames, outList, dimVect, dimNames;
  SEXP tmpExp;
  hpFilter_t hpFilter[GMS_MAX_INDEX_DIM];
  xpFilter_t xpFilter[GMS_MAX_INDEX_DIM];
  int outIdx[GMS_MAX_INDEX_DIM];
  FILE    *fin;
  rSpec_t *rSpec;
  gdxUelIndex_t uels;
  gdxValues_t values;
  gdxSVals_t sVals;
  d64_t d64;
  double dt, posInf, negInf;
  shortStringBuf_t msgBuf;
  shortStringBuf_t uelName;
  shortStringBuf_t gdxFileName;
  int symIdx, symDim, symType, symNNZ, symUser = 0, typeCode = 0;
  int symDimX;                  /* allow for additional dim on var/equ with field='all' */
  SEXP fieldUels = R_NilValue; /* UELS for addition dimension for field */
  int iDim;
  int rc, findrc, errNum, nUEL, iUEL;
  int mrows = 0;                /* NNZ count, i.e. number of rows in
                                 * $val when form='sparse' */
  int nCols;                    /* number of cols in $val when form='sparse' */
  int kk, iRec, nRecs, index, changeIdx, kRec;
  int rgdxAlloc;                /* PROTECT count: undo this many on exit */
  int UELUserMapping, highestMappedUEL;
  int foundTuple;
  int arglen, matched = -1;
  double *p, *dimVal;
  char buf[3*sizeof(shortStringBuf_t)];
  char strippedID[GMS_SSSIZE];
  char symName[GMS_SSSIZE];
  char symText[GMS_SSSIZE], msg[GMS_SSSIZE], stringEle[GMS_SSSIZE];
  char domInfoSrc[16] = "unknown";
  char *types[] = {"set", "parameter", "variable", "equation"};
  char *forms[] = {"full", "sparse"};
  char *fields[] = {"l", "m", "lo", "up", "s", "all"};
  int elementIndex, IDum, totalElement;
  int withList = 0;
  int outElements = 0;    /* count of elements in outList */
  int iElement;           /* index into outList, outListNames */
  int nnz;         /* symbol cardinality, i.e. nonzero count */
  int nnzMax;      /* maximum possible nnz for this symbol */
  Rboolean squeezeDef = NA_LOGICAL; /* squeeze out default records */
  Rboolean useDomInfo = NA_LOGICAL;
  Rboolean followAlias = NA_LOGICAL;

  /* setting initial values */
  rgdxAlloc = 0;

  /* first arg is function name - ignore it */
  arglen = length(args);

  /* ----------------- Check proper number of inputs and outputs ------------
   * Function should follow specification of
   * rgdx ('gdxFileName', requestList = NULL, squeeze = TRUE, useDomInfo=TRUE,
   *       followAlias=TRUE)
   * ------------------------------------------------------------------------ */
  if (6 != arglen) {
    error ("usage: %s(gdxName, requestList = NULL, squeeze = TRUE,"
           " useDomInfo = TRUE, followAlias = TRUE) - incorrect arg count",
           funcName);
  }
  targs = CDR(args);
  fileName       = CAR(targs);  targs = CDR(targs);
  requestList    = CAR(targs);  targs = CDR(targs);
  squeezeExp     = CAR(targs);  targs = CDR(targs);
  udi            = CAR(targs);  targs = CDR(targs);
  followAliasExp = CAR(targs);  targs = CDR(targs);
  if (TYPEOF(fileName) != STRSXP) {
    error ("usage: %s(gdxName, requestList = NULL) - gdxName must be a string", funcName);
  }
  if (TYPEOF(requestList) == NILSXP)
    withList = 0;
  else {
    withList = 1;
    if (TYPEOF(requestList) != VECSXP) {
      error ("usage: %s(gdxName, requestList, squeeze, useDomInfo) - requestList must be a list", funcName);
    }
  }

  (void) CHAR2ShortStr (CHAR(STRING_ELT(fileName, 0)), gdxFileName);

  if (! withList) {
    if (0 == strcmp("?", gdxFileName)) {
      int n = (int)strlen (ID);
      memcpy (strippedID, ID+1, n-2);
      strippedID[n-2] = '\0';
      Rprintf ("R-file source info: %s\n", strippedID);
      return R_NilValue;
    } /* if audit run */
  } /* if one arg, of character type */

  squeezeDef = exp2Boolean (squeezeExp);
  if (NA_LOGICAL == squeezeDef) {
    error ("usage: %s(gdxName, requestList, squeeze = TRUE, useDomInfo = TRUE)\n    squeeze argument could not be interpreted as logical", funcName);
  }
  useDomInfo = exp2Boolean (udi);
  if (NA_LOGICAL == useDomInfo) {
    error ("usage: %s(gdxName, requestList, squeeze = TRUE, useDomInfo = TRUE)\n    useDomInfo argument could not be interpreted as logical", funcName);
  }
  followAlias = exp2Boolean (followAliasExp);
  if (NA_LOGICAL == followAlias) {
    error ("usage: %s(gdxName, requestList, squeeze = TRUE, useDomInfo = TRUE, followAlias = TRUE)\n    followAlias argument could not be interpreted as logical", funcName);
  }

  /* ------------------- check if the GDX file exists --------------- */
  checkFileExtension (gdxFileName);
  fin = fopen (gdxFileName, "r");
  if (fin==NULL) {
    error ("GDX file '%s' not found", gdxFileName);
  }
  fclose(fin);
  /*-------------------- Checking data for input list ------------*/
  /* Setting default values */
  rSpec = malloc(sizeof(*rSpec));
  memset (rSpec, 0, sizeof(*rSpec));
  rSpec->dForm = sparse;
  rSpec->dField = level;
  rSpec->dim = -1;              /* negative indicates NA */

  memset (hpFilter, 0, sizeof(hpFilter));
  memset (xpFilter, 0, sizeof(xpFilter));

  if (withList) {
    checkRgdxList (requestList, rSpec, &rgdxAlloc);
    if (rSpec->compress && rSpec->withUel) {
      error("Compression is not allowed with input UELs");
    }
  }

  loadGDX();
  rc = gdxCreate (&gdxHandle, msgBuf, sizeof(msgBuf));
  if (0 == rc)
    error ("Error creating GDX object: %s", msgBuf);
  rc = gdxOpenRead (gdxHandle, gdxFileName, &errNum);
  if (errNum || 0 == rc) {
    error("Could not open gdx file with gdxOpenRead");
  }

  gdxGetSpecialValues (gdxHandle, sVals);
  d64.u64 = 0x7fffffffffffffff; /* positive QNaN, mantissa all on */
  sVals[GMS_SVIDX_UNDEF] = d64.x;
  sVals[GMS_SVIDX_NA] = NA_REAL;
  dt = 0.0;
  posInf =  1 / dt;
  negInf = -1 / dt;
  sVals[GMS_SVIDX_EPS] = 0;
  sVals[GMS_SVIDX_PINF] = posInf;
  sVals[GMS_SVIDX_MINF] = negInf;
  gdxSetSpecialValues (gdxHandle, sVals);

  /* read symbol name only if input list is present */
  if (withList) {
    /* start searching for symbol */
    rc = gdxFindSymbol (gdxHandle, rSpec->name, &symIdx);
    if (! rc) {
      sprintf (buf, "GDX file %s contains no symbol named '%s'",
               gdxFileName,
               rSpec->name );
      error (buf);
    }
    gdxSymbolInfo (gdxHandle, symIdx, symName, &symDim, &symType);
    gdxSymbolInfoX (gdxHandle, symIdx, &symNNZ, &symUser, symText);
    /* symNNZ aka nRecs: count of nonzeros/records in symbol */

    switch (symType) {
    case GMS_DT_SET:
      if (rSpec->withField)
        error("Bad read specifier for set symbol '%s': 'field' not allowed.",
              rSpec->name);
      break;
    case GMS_DT_PAR:
      if (rSpec->withField)
        error("Bad read specifier for parameter symbol '%s': 'field' not allowed.",
              rSpec->name);
      break;
    case GMS_DT_VAR:
      if (rSpec->compress) {
        error("Compression is not allowed when reading variables");
      }
      typeCode = gmsFixVarType (symUser);
      if (typeCode < 0) {
        error ("Variable symbol '%s' has no associated type (e.g. free, binary)",
               rSpec->name);
      }
      break;
    case GMS_DT_EQU:
      if (rSpec->compress) {
        error("Compression is not allowed when reading equations");
      }
      typeCode = gmsFixEquType (symUser);
      if (typeCode < 0) {
        error ("Equation symbol '%s' has no associated type (e.g. =E=, =G=)",
               rSpec->name);
      }
      break;
    case GMS_DT_ALIAS:          /* follow link to actual set */
      if (followAlias) {
        symIdx = symUser;
        gdxSymbolInfo (gdxHandle, symIdx, symName, &symDim, &symType);
        gdxSymbolInfoX (gdxHandle, symIdx, &symNNZ, &symUser, symText);
      }
      else {
        char aliasFor[GMS_SSSIZE];

        gdxSymbolInfo (gdxHandle, symUser, aliasFor, &symDim, &symType);
        outList = aliasReturn (symName, aliasFor);
        free(rSpec);
        errNum = gdxClose (gdxHandle);
        if (errNum != 0) {
          error("Errors detected when closing gdx file");
        }
        (void) gdxFree (&gdxHandle);
        UNPROTECT(rgdxAlloc+1); /* 1 from aliasReturn call */
        return outList;
      }
      break;
    default:
      sprintf(buf, "GDX symbol %s (index=%d, symDim=%d, symType=%d)"
              " is not recognized as set, parameter, variable, or equation",
              rSpec->name, symIdx, symDim, symType);
      error(buf);
    } /* end switch */

    if (rSpec->te && symType != GMS_DT_SET) {
      error("Text elements only exist for sets and symbol '%s' is not a set.",
            rSpec->name);
    }
    if (rSpec->dim >= 0) {
      /* check that symbol dim agrees with expected dim */
      if (rSpec->dim != symDim) {
        sprintf(buf, "Symbol %s has dimension %d but you specifed dim=%d",
                rSpec->name, symDim, rSpec->dim);
        error(buf);
      }
    }
  } /* if (withList) */

  /* Get UEL universe from GDX file */
  (void) gdxUMUelInfo (gdxHandle, &nUEL, &highestMappedUEL);
  PROTECT(universe = allocVector(STRSXP, nUEL));
  rgdxAlloc++;
  for (iUEL = 1;  iUEL <= nUEL;  iUEL++) {
    if (!gdxUMUelGet (gdxHandle, iUEL, uelName, &UELUserMapping)) {
      error("Could not gdxUMUelGet");
    }
    SET_STRING_ELT(universe, iUEL-1, mkChar(uelName));
  }

  /* check relevant options */
  if (rSpec->te) {              /* if we read set text */
    SEXP o = GetOption1(install("gdx.inventSetText"));
    if (R_NilValue != o) {
      if (LGLSXP == TYPEOF(o)) {
        inventSetText = LOGICAL(o)[0];
      }
      else if (asLogical(o))
        inventSetText = TRUE;
      else
        inventSetText = FALSE;
    }
  }

#if 0
  {
    /* assume domainNames is set to the right default */
    SEXP o = GetOption1(install("gdx.domainNames"));
    if (R_NilValue != o) {
      if (LGLSXP == TYPEOF(o)) {
        if (NA_LOGICAL != LOGICAL(o)[0])
          domainNames = LOGICAL(o)[0];
      }
      else
        domainNames = asLogical(o);
    }
  }
#endif

  outElements = 6;   /* outList has at least 6 elements, maybe more */
  if (withList) { /* aa */
    /* Checking dimension of input uel and parameter in GDX file.
     * If they are not equal then error. */
    if (rSpec->withUel && length(rSpec->filterUel) != symDim) {
      error("Dimension of UEL filter entered does not match with symbol in GDX");
    }
    /* initialize hpFilter to use a universe filter for each dimension */
    for (iDim = 0;  iDim < symDim;  iDim++) {
      hpFilter[iDim].fType = identity;
    }

    nCols = symDim + 1;         /* usual index cols + data col */
    symDimX = symDim;
    switch (symType) {
    case GMS_DT_SET:
      nCols = symDim;           /* no data col */
      break;
    case GMS_DT_VAR:
    case GMS_DT_EQU:
      if (all == rSpec->dField) { /* additional 'field' col */
        nCols++;
        symDimX++;
        PROTECT(fieldUels = allocVector(STRSXP, GMS_VAL_MAX));
        rgdxAlloc++;
        SET_STRING_ELT(fieldUels, GMS_VAL_LEVEL   , mkChar(fields[GMS_VAL_LEVEL   ]));
        SET_STRING_ELT(fieldUels, GMS_VAL_MARGINAL, mkChar(fields[GMS_VAL_MARGINAL]));
        SET_STRING_ELT(fieldUels, GMS_VAL_LOWER   , mkChar(fields[GMS_VAL_LOWER   ]));
        SET_STRING_ELT(fieldUels, GMS_VAL_UPPER   , mkChar(fields[GMS_VAL_UPPER   ]));
        SET_STRING_ELT(fieldUels, GMS_VAL_SCALE   , mkChar(fields[GMS_VAL_SCALE   ]));
      }
      break;
    } /* end switch */

    /* we will have domain info returned for all symbols */
    PROTECT(outDomains = allocVector(STRSXP, symDimX));
    rgdxAlloc++;
    PROTECT(outDomInfo = allocVector(STRSXP, 1));
    rgdxAlloc++;
    domInfoCode = 0;

    outTeSp = R_NilValue;
    nnz = 0;
    if (rSpec->withUel) {
      /* here we check the cardinality of the symbol we are reading,
       * i.e. the number of nonzeros, i.e. the number of elements that match
       * in uel filter.  Given this value,
       * we can create a 2D double matrix for sparse format.
       */
      /* create integer filters */
      for (iDim = 0;  iDim < symDim;  iDim++) {
        mkHPFilter (VECTOR_ELT(rSpec->filterUel, iDim), hpFilter + iDim);
      }
      for (nnzMax = 1, iDim = 0;  iDim < symDim;  iDim++) {
        nnzMax *=  length(VECTOR_ELT(rSpec->filterUel, iDim));
      }

      (void) strcpy (domInfoSrc, "filtered");
#if 1
      getDomainNames (symIdx, useDomInfo, outDomains, &domInfoCode);
#else
      /* set domain names to "_user": cannot conflict with real set names */
      for (iDim = 0;  iDim < symDim;  iDim++) {
        SET_STRING_ELT(outDomains, iDim, mkChar("_user"));
      }
#endif
      reuseFilter = 1;
      if (symDimX > symDim) {
        reuseFilter = 0;
        SET_STRING_ELT(outDomains, iDim, mkChar("_field"));
      }

      /* count records that match the filter and won't be squeezed out */
      prepHPFilter (symDim, hpFilter);
      gdxDataReadRawStart (gdxHandle, symIdx, &nRecs);
      switch (symType) {
      case GMS_DT_SET:
        for (nnz = 0, iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          /* no squeeze for a set */
          foundTuple = findInHPFilter (symDim, uels, hpFilter, outIdx);
          if (foundTuple) {
            nnz++;
            if (nnz >= nnzMax) {
              break;
            }
          }
        } /* loop over gdx records */
        break;
      case GMS_DT_PAR:
        for (nnz = 0, iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          if (squeezeDef && (0 == values[GMS_VAL_LEVEL]))
            continue;
          foundTuple = findInHPFilter (symDim, uels, hpFilter, outIdx);
          if (foundTuple) {
            nnz++;
            if (nnz >= nnzMax) {
              break;
            }
          }
        } /* loop over gdx records */
        break;
      case GMS_DT_VAR:
      case GMS_DT_EQU:
        if (all != rSpec->dField) {
          double defVal = getDefVal (symType, typeCode, rSpec->dField);

          for (nnz = 0, iRec = 0;  iRec < nRecs;  iRec++) {
            gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
            if (squeezeDef && (defVal == values[rSpec->dField]))
              continue;
            foundTuple = findInHPFilter (symDim, uels, hpFilter, outIdx);
            if (foundTuple) {
              nnz++;
              if (nnz >= nnzMax) {
                break;
              }
            }
          } /* loop over GDX records */
        }
        else {                /* all == rSpec->dField */
          for (kRec = 0, iRec = 0;  iRec < nRecs;  iRec++) {
            gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
            /* for now assume no filtering when field==all */
            foundTuple = findInHPFilter (symDim, uels, hpFilter, outIdx);
            if (foundTuple) {
              nnz++;
              if (nnz >= nnzMax) {
                break;
              }
            }
          } /* loop over GDX records */
        }   /* if (all != field) .. else ..  */
        break;
      default:
        error("Unrecognized type of symbol found.");
      } /* end switch(symType) */
      if (!gdxDataReadDone (gdxHandle)) {
        error ("Could not gdxDataReadDone");
      }

      mrows = nnz;
      if ((symType == GMS_DT_VAR || symType == GMS_DT_EQU))
        if (all == rSpec->dField)
          mrows *= 5;           /* l,m,lo,up,scale */

      /* Allocating memory for 2D sparse matrix */
      PROTECT(outValSp = allocMatrix(REALSXP, mrows, nCols));
      rgdxAlloc++;
      p = REAL(outValSp);

      if (rSpec->te) { /* read set elements with their text, using filter */
        PROTECT(outTeSp = allocVector(STRSXP, nnz));
        rgdxAlloc++;
        gdxDataReadRawStart (gdxHandle, symIdx, &nRecs);
        prepHPFilter (symDim, hpFilter);
        for (matched = 0, iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          foundTuple = findInHPFilter (symDim, uels, hpFilter, outIdx);
          if (foundTuple) {
            for (iDim = 0;  iDim < symDim;  iDim++) {
              p[matched + iDim*nnz] = outIdx[iDim];
            }

            index = matched + symDim * nnz;

            if (values[GMS_VAL_LEVEL]) {
              elementIndex = (int) values[GMS_VAL_LEVEL];
              gdxGetElemText(gdxHandle, elementIndex, msg, &IDum);
              SET_STRING_ELT(outTeSp, matched, mkChar(msg));
            }
            else {
              if (NA_LOGICAL == inventSetText)
                SET_STRING_ELT(outTeSp, matched, R_NaString);
              else if (FALSE == inventSetText) /* make it "" */
                SET_STRING_ELT(outTeSp, matched, R_BlankString);
              else {
                stringEle[0] = '\0';
                for (iDim = 0;  iDim < symDim;  iDim++) {
                  strcat(stringEle, CHAR(STRING_ELT(universe, uels[iDim]-1)));
                  if (iDim != symDim-1)
                    strcat(stringEle, ".");
                }
                SET_STRING_ELT(outTeSp, matched, mkChar(stringEle));
              }
            }
            matched++;
          }
          if (matched == nnz) {
            break;
          }
        }  /* loop over GDX records */
        if (!gdxDataReadDone (gdxHandle)) {
          error ("Could not gdxDataReadDone");
        }
      } /* if rSpec->te */
      else {
        prepHPFilter (symDim, hpFilter);
        gdxDataReadRawStart (gdxHandle, symIdx, &nRecs);
        switch (symType) {
        case GMS_DT_SET:
          /* at some point, put the rSpec->te stuff in here instead of above this */
          if (rSpec->te) {
            error ("filtered set read: rSpec->te already handled above");
          }
          for (matched = 0, iRec = 0;  iRec < nRecs;  iRec++) {
            gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
            /* no squeeze for a set */
            foundTuple = findInHPFilter (symDim, uels, hpFilter, outIdx);
            if (foundTuple) {
              for (iDim = 0;  iDim < symDim;  iDim++) {
                p[matched + iDim*nnz] = outIdx[iDim];
              }
              matched++;
              if (matched == nnz)
                break;
            } /* if foundTuple */
          } /* loop over GDX records */
          break;
        case GMS_DT_PAR:
          for (matched = 0, iRec = 0;  iRec < nRecs;  iRec++) {
            gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
            if (squeezeDef && (0 == values[GMS_VAL_LEVEL]))
              continue;
            foundTuple = findInHPFilter (symDim, uels, hpFilter, outIdx);
            if (foundTuple) {
              for (iDim = 0;  iDim < symDim;  iDim++) {
                p[matched + iDim*nnz] = outIdx[iDim];
              }
              index = matched + symDim * nnz;
              p[index] = values[GMS_VAL_LEVEL];
              matched++;
              if (matched == nnz)
                break;
            } /* if foundTuple */
          } /* loop over GDX records */
          break;
        case GMS_DT_VAR:
        case GMS_DT_EQU:
          if (all != rSpec->dField) {
            double defVal = getDefVal (symType, typeCode, rSpec->dField);

            for (matched = 0, iRec = 0;  iRec < nRecs;  iRec++) {
              gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
              if (squeezeDef && (defVal == values[rSpec->dField]))
                continue;
              foundTuple = findInHPFilter (symDim, uels, hpFilter, outIdx);
              if (foundTuple) {
                for (iDim = 0;  iDim < symDim;  iDim++) {
                  p[matched + iDim*nnz] = outIdx[iDim];
                }
                index = matched + symDim * nnz;
                p[index] = values[rSpec->dField];
                matched++;
                if (matched == nnz)
                  break;
              } /* if foundTuple */
            } /* loop over GDX records */
          }
          else {                /* all == rSpec->dField */
            for (matched = 0, kRec = 0, iRec = 0;  iRec < nRecs;  iRec++) {
              gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
              /* for now assume no filtering when field==all */
              foundTuple = findInHPFilter (symDim, uels, hpFilter, outIdx);
              if (foundTuple) {
                for (index = kRec, kk = 0;  kk < symDim;  kk++) {
                  p[index+GMS_VAL_LEVEL   ] = outIdx[kk];
                  p[index+GMS_VAL_MARGINAL] = outIdx[kk];
                  p[index+GMS_VAL_LOWER   ] = outIdx[kk];
                  p[index+GMS_VAL_UPPER   ] = outIdx[kk];
                  p[index+GMS_VAL_SCALE   ] = outIdx[kk];
                  index += mrows;
                }
                p[index+GMS_VAL_LEVEL   ] = 1 + GMS_VAL_LEVEL;
                p[index+GMS_VAL_MARGINAL] = 1 + GMS_VAL_MARGINAL;
                p[index+GMS_VAL_LOWER   ] = 1 + GMS_VAL_LOWER;
                p[index+GMS_VAL_UPPER   ] = 1 + GMS_VAL_UPPER;
                p[index+GMS_VAL_SCALE   ] = 1 + GMS_VAL_SCALE;
                index += mrows;
                p[index+GMS_VAL_LEVEL   ] = values[GMS_VAL_LEVEL];
                p[index+GMS_VAL_MARGINAL] = values[GMS_VAL_MARGINAL];
                p[index+GMS_VAL_LOWER   ] = values[GMS_VAL_LOWER];
                p[index+GMS_VAL_UPPER   ] = values[GMS_VAL_UPPER];
                p[index+GMS_VAL_SCALE   ] = values[GMS_VAL_SCALE];
                kRec += GMS_VAL_MAX;
                matched++;
                if (matched == nnz)
                  break;
              } /* if foundTuple */
            } /* loop over GDX records */
          }   /* if (all != field) .. else ..  */
          break;
        default:
          error("Unrecognized type of symbol found.");
        } /* end switch(symType) */
        if (!gdxDataReadDone (gdxHandle)) {
          error ("Could not gdxDataReadDone");
        }
        if (matched != nnz)
          error ("mismatch after filtered read: matched = %d  nnz = %d", matched, nnz);
      } /* if (te) .. else .. */
    }   /* if withUel */
    else {
      /* read without user UEL filter: use domain info to filter if possible */
      reuseFilter = 0;
      mrows = symNNZ;
      /*  check for non zero elements for variable and equation */
      if ((symType == GMS_DT_VAR || symType == GMS_DT_EQU)) {
        if (all == rSpec->dField) {
          mrows *= 5;           /* l,m,lo,up,scale */
        }
        else if (squeezeDef) { /* potentially squeeze some out */
          mrows = getNonDefaultElemCount(gdxHandle, symIdx, symType, typeCode,
                                         rSpec->dField);
        }
      }
      /* Create 2D sparse R array */
      PROTECT(outValSp = allocMatrix(REALSXP, mrows, nCols));
      rgdxAlloc++;
      p = REAL(outValSp);

      mkXPFilter (symIdx, useDomInfo, xpFilter, outDomains, &domInfoCode);
      switch (domInfoCode) {
      case 0:
        (void) strcpy (domInfoSrc, "NA");
        break;
      case 1:
        (void) strcpy (domInfoSrc, "none");
        break;
      case 2:
        (void) strcpy (domInfoSrc, "relaxed");
        break;
      case 3:
        (void) strcpy (domInfoSrc, "full");
        break;
      }
      if (symDimX > symDim)
        SET_STRING_ELT(outDomains, iDim, mkChar("_field"));

      kRec = 0;                 /* shut up warnings */
      gdxDataReadRawStart (gdxHandle, symIdx, &nRecs);
      switch (symType) {
      case GMS_DT_SET:
        if (rSpec->te) {
          PROTECT(outTeSp = allocVector(STRSXP, mrows));
          rgdxAlloc++;
        }
        for (iRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          findrc = findInXPFilter (symDim, uels, xpFilter, outIdx);
          if (findrc) {
            error ("DEBUG 00: findrc = %d is unhandled", findrc);
          }
          for (kk = 0;  kk < symDim;  kk++) {
            p[iRec + kk*mrows] = outIdx[kk]; /* from the xpFilter */
          }
          if (rSpec->te) {
            if (values[GMS_VAL_LEVEL]) {
              elementIndex = (int) values[GMS_VAL_LEVEL];
              gdxGetElemText(gdxHandle, elementIndex, msg, &IDum);
              SET_STRING_ELT(outTeSp, iRec, mkChar(msg));
            }
            else {
              if (NA_LOGICAL == inventSetText)
                SET_STRING_ELT(outTeSp, iRec, R_NaString);
              else if (FALSE == inventSetText) /* make it "" */
                SET_STRING_ELT(outTeSp, iRec, R_BlankString);
              else {
                stringEle[0] = '\0';
                for (kk = 0;  kk < symDim;  kk++) {
                  strcat(stringEle, CHAR(STRING_ELT(universe, uels[kk]-1)));
                  if (kk != symDim-1)
                    strcat(stringEle, ".");
                }
                SET_STRING_ELT(outTeSp, iRec, mkChar(stringEle));
              } /* inventSetText is true */
            }
          } /* if returning set text */
        } /* loop over GDX records */
        kRec = nRecs;
        break;
      case GMS_DT_PAR:
        for (iRec = 0, kRec = 0;  iRec < nRecs;  iRec++) {
          gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
          findrc = findInXPFilter (symDim, uels, xpFilter, outIdx);
          if (findrc) {
            error ("DEBUG 00: findrc = %d is unhandled", findrc);
          }
          if ((! squeezeDef) ||
              (0 != values[GMS_VAL_LEVEL])) {
            /* store the value */
            for (index = kRec, kk = 0;  kk < symDim;  kk++) {
              p[index] = outIdx[kk]; /* from the xpFilter */
              index += mrows;
            }
            p[index] = values[GMS_VAL_LEVEL];
            kRec++;
          } /* end if (no squeeze || val != 0) */
        } /* loop over GDX records */
        break;
      case GMS_DT_VAR:
      case GMS_DT_EQU:
        if (all != rSpec->dField) {
          double defVal = 0;

          if (GMS_DT_VAR == symType)
            defVal = getDefValVar (typeCode, rSpec->dField);
          else {
            defVal = getDefValEqu (typeCode, rSpec->dField);
          }
          for (iRec = 0, kRec = 0;  iRec < nRecs;  iRec++) {
            gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
            findrc = findInXPFilter (symDim, uels, xpFilter, outIdx);
            if (findrc) {
              error ("DEBUG 00: findrc = %d is unhandled", findrc);
            }
            if ((! squeezeDef) ||
                (defVal != values[rSpec->dField])) {
              /* store the value */
              for (index = kRec, kk = 0;  kk < symDim;  kk++) {
                p[index] = outIdx[kk]; /* from the xpFilter */
                index += mrows;
              }
              p[index] = values[rSpec->dField];
              kRec++;
            } /* end if (no squeeze || val != default) */
          } /* loop over GDX records */
        }
        else {
          for (iRec = 0, kRec = 0;  iRec < nRecs;  iRec++) {
            gdxDataReadRaw (gdxHandle, uels, values, &changeIdx);
            findrc = findInXPFilter (symDim, uels, xpFilter, outIdx);
            if (findrc) {
              error ("DEBUG 00: findrc = %d is unhandled", findrc);
            }
            for (index = kRec, kk = 0;  kk < symDim;  kk++) {
              p[index+GMS_VAL_LEVEL   ] = outIdx[kk];
              p[index+GMS_VAL_MARGINAL] = outIdx[kk];
              p[index+GMS_VAL_LOWER   ] = outIdx[kk];
              p[index+GMS_VAL_UPPER   ] = outIdx[kk];
              p[index+GMS_VAL_SCALE   ] = outIdx[kk];
              index += mrows;
            }
            p[index+GMS_VAL_LEVEL   ] = 1 + GMS_VAL_LEVEL;
            p[index+GMS_VAL_MARGINAL] = 1 + GMS_VAL_MARGINAL;
            p[index+GMS_VAL_LOWER   ] = 1 + GMS_VAL_LOWER;
            p[index+GMS_VAL_UPPER   ] = 1 + GMS_VAL_UPPER;
            p[index+GMS_VAL_SCALE   ] = 1 + GMS_VAL_SCALE;
            index += mrows;
            p[index+GMS_VAL_LEVEL   ] = values[GMS_VAL_LEVEL];
            p[index+GMS_VAL_MARGINAL] = values[GMS_VAL_MARGINAL];
            p[index+GMS_VAL_LOWER   ] = values[GMS_VAL_LOWER];
            p[index+GMS_VAL_UPPER   ] = values[GMS_VAL_UPPER];
            p[index+GMS_VAL_SCALE   ] = values[GMS_VAL_SCALE];
            kRec += GMS_VAL_MAX;
          } /* loop over GDX records */
        }
        break;
      default:
        error("Unrecognized type of symbol found.");
      } /* end switch(symType) */
      if (!gdxDataReadDone (gdxHandle)) {
        error ("Could not gdxDataReadDone");
      }
      if (kRec < mrows) {
        SEXP newCV, tmp;
        double *newp;
        double *from, *to;

        PROTECT(newCV = allocMatrix(REALSXP, kRec, nCols));
        newp = REAL(newCV);
        for (kk = 0;  kk <= symDim;  kk++) {
          from = p    + kk*mrows;
          to   = newp + kk*kRec;
          MEMCPY (to, from, sizeof(*p)*kRec);
        }
        tmp = outValSp;
        outValSp = newCV;
        UNPROTECT_PTR(tmp);
        mrows = kRec;
      }
    } /* if (withUel .. else .. ) */

    /* here the output uels $uels are allocated and populated */
    if (rSpec->compress) {
      PROTECT(outUels = allocVector(VECSXP, symDimX));
      rgdxAlloc++;
      compressData (symDim, mrows, universe, nUEL, xpFilter,
                    outValSp, outUels);
      /* set domain names to "_compressed": cannot conflict with real set names */
#if 0
      for (iDim = 0;  iDim < symDim;  iDim++) {
        SET_STRING_ELT(outDomains, iDim, mkChar("_compressed"));
      }
#endif
      if (symDimX > symDim) {
        SET_VECTOR_ELT(outUels, iDim, fieldUels);
        SET_STRING_ELT(outDomains, iDim, mkChar("_field"));
      }
      (void) strcpy (domInfoSrc, "compressed");
    }
    else if (! rSpec->withUel) {
      PROTECT(outUels = allocVector(VECSXP, symDimX));
      rgdxAlloc++;
      xpFilterToUels (symDim, xpFilter, universe, outUels);
      if (symDimX > symDim)
        SET_VECTOR_ELT(outUels, iDim, fieldUels);
    }
    else if (! reuseFilter) {
      /* if we got here, we have a UEL filter from the user but must copy it */
      PROTECT(outUels = allocVector(VECSXP, symDimX));
      rgdxAlloc++;
      for (iDim = 0;  iDim < symDim;  iDim++) {
        SET_VECTOR_ELT(outUels, iDim, VECTOR_ELT(rSpec->filterUel, iDim));
      }
      SET_VECTOR_ELT(outUels, iDim, fieldUels);
    }

    /* Converting sparse data into full matrix */
    if (rSpec->dForm == full) {
      double *p0, *p1;

      switch (symDim) {
      case 0:
        if (all == rSpec->dField) {
          PROTECT(outValFull = allocVector(REALSXP, GMS_VAL_MAX));
          rgdxAlloc++;
          p0 = REAL(outValFull);
          PROTECT(dimVect = allocVector(REALSXP, 1));
          REAL(dimVect)[0] = 5;
          PROTECT(dimNames = allocVector(VECSXP, 1));
          SET_VECTOR_ELT(dimNames, 0, fieldUels);
          setAttrib(outValFull, R_DimSymbol, dimVect);
          if (R_NilValue != outDomains) {
            setAttrib(dimNames, R_NamesSymbol, outDomains);
          }
          if (! reuseFilter) {
            if (R_NilValue != outDomains) {
              setAttrib(outUels, R_NamesSymbol, outDomains);
            }
          }
          setAttrib(outValFull, R_DimNamesSymbol, dimNames);
          UNPROTECT(2);
          if (GMS_DT_VAR == symType)
            getDefRecVar (typeCode, p0);
          else {
            getDefRecEqu (typeCode, p0);
            /* error ("not yet implemented XX"); */
          }
          if (rSpec->withUel) {
            /* assume matched is always set for filtered reads */
            if (matched > 0) {
              p1 = REAL(outValSp);
              p1 += GMS_VAL_MAX; /* skip index column */
              (void) memcpy (p0, p1, GMS_VAL_MAX * sizeof(double));
            }
          }
          else {
            if (mrows > 0) {
              p1 = REAL(outValSp);
              p1 += GMS_VAL_MAX; /* skip index column */
              (void) memcpy (p0, p1, GMS_VAL_MAX * sizeof(double));
            }
          }
        }
        else {                  /* all != dField */
          PROTECT(outValFull = allocVector(REALSXP, 1));
          rgdxAlloc++;
          p0 = REAL(outValFull);
          *p0 = getDefVal (symType, typeCode, rSpec->dField);
          if (rSpec->withUel) {
            /* assume matched is always set for filtered reads */
            /* if (outValSp != R_NilValue && (REAL(outValSp) != NULL)) { */
            if (matched > 0) {
              *p0 = REAL(outValSp)[0];
            }
          }
          else {
            if (mrows > 0)
              *p0 = REAL(outValSp)[0];
          }
        } /* all != dField */
        /* sets cannot have symDim 0, so skip conversion of set text */
        break;

      case 1:
        /* caution: for 1-dim params and vars with a single field, we return a matrix!
         * it would be more consistent to return a 1-d array in these cases
         */
        PROTECT(dimVect = allocVector(REALSXP, 2));
        rgdxAlloc++;
        dimVal = REAL(dimVect);
        PROTECT(dimNames = allocVector(VECSXP, 2)); /* for one-dim symbol, val is 2-dim */
        rgdxAlloc++;
        if (all == rSpec->dField) {
          dimVal[1] = 5;
          SET_VECTOR_ELT(dimNames, 1, fieldUels);
        }
        else {
          totalElement = 1;
          dimVal[1] = 1;
          SET_VECTOR_ELT(dimNames, 1, R_NilValue); /* no names for 2nd dimension */
        }
        totalElement = dimVal[1];

        if (rSpec->withUel) {
          SEXP dimNamesNames;

          dimVal[0] = length(VECTOR_ELT(rSpec->filterUel, 0));
          totalElement *= dimVal[0];
          PROTECT(outValFull = allocVector(REALSXP, totalElement));
          rgdxAlloc++;
          PROTECT(dimNamesNames = allocVector(STRSXP, 2));
          if (R_NilValue == VECTOR_ELT(dimNames, 1)) /* no names for 2nd dimension */
            SET_STRING_ELT(dimNamesNames, 1, mkChar(""));
          else
            SET_STRING_ELT(dimNamesNames, 1, mkChar("_field"));
          SET_STRING_ELT(dimNamesNames, 0, STRING_ELT(outDomains, 0));
          setAttrib(dimNames, R_NamesSymbol, dimNamesNames);
          UNPROTECT(1);
          if (reuseFilter) {
            sparseToFull (outValSp, outValFull, rSpec->filterUel, symType,
                          typeCode, rSpec->dField, mrows, symDimX);
            setAttrib(rSpec->filterUel, R_NamesSymbol, outDomains);
          }
          else {
            sparseToFull (outValSp, outValFull, outUels, symType,
                          typeCode, rSpec->dField, mrows, symDimX);
            setAttrib(outUels, R_NamesSymbol, outDomains);
          }
          setAttrib(outValFull, R_DimSymbol, dimVect);
          SET_VECTOR_ELT(dimNames, 0, VECTOR_ELT(rSpec->filterUel, 0));
          /* dimnamesnames done */
          setAttrib(outValFull, R_DimNamesSymbol, dimNames);
        }
        else {
          dimVal[0] = length(VECTOR_ELT(outUels, 0));
          totalElement *= dimVal[0];
          PROTECT(outValFull = allocVector(REALSXP, totalElement));
          rgdxAlloc++;
          sparseToFull (outValSp, outValFull, outUels, symType,
                        typeCode, rSpec->dField, mrows, symDimX);
          setAttrib(outValFull, R_DimSymbol, dimVect);
          SET_VECTOR_ELT(dimNames, 0, VECTOR_ELT(outUels, 0));
          if (R_NilValue != outDomains) {
            SEXP dimNamesNames;
            PROTECT(dimNamesNames = allocVector(STRSXP, 2));
            if (R_NilValue == VECTOR_ELT(dimNames, 1)) { /* no names for 2nd dimension */
              SET_STRING_ELT(dimNamesNames, 1, mkChar(""));
            }
            else {
              SET_STRING_ELT(dimNamesNames, 1, mkChar("_field"));
            }
            SET_STRING_ELT(dimNamesNames, 0, STRING_ELT(outDomains, 0));
            setAttrib(dimNames, R_NamesSymbol, dimNamesNames);
            UNPROTECT(1);
            setAttrib(outUels, R_NamesSymbol, outDomains);
          }
          /* dimnamesnames done */
          setAttrib(outValFull, R_DimNamesSymbol, dimNames);
        }

        if (rSpec->te) {   /* create full dimensional string matrix */
          PROTECT(outTeFull = allocVector(STRSXP, dimVal[0]));
          rgdxAlloc++;
          if (rSpec->withUel) {
            createElementMatrix (outValSp, outTeSp, outTeFull, rSpec->filterUel,
                                 symDim, nnz);
          }
          else {
            createElementMatrix (outValSp, outTeSp, outTeFull, outUels,
                                 symDim, mrows);
          }
          setAttrib(outTeFull, R_DimSymbol, dimVect); /* .te has same dimension as .val */
          /* dimnamesnames done */
          setAttrib(outTeFull, R_DimNamesSymbol, dimNames);
        } /* if rSpec->te */
        break;

      default:
        PROTECT(dimVect = allocVector(REALSXP, symDimX));
        rgdxAlloc++;
        totalElement = 1;
        dimVal = REAL(dimVect);
        if (reuseFilter) {
          for (iDim = 0;  iDim < symDimX;  iDim++) {
            dimVal[iDim] = length(VECTOR_ELT(rSpec->filterUel, iDim));
            totalElement *= dimVal[iDim];
          }
          PROTECT(outValFull = allocVector(REALSXP, totalElement));
          rgdxAlloc++;
          sparseToFull (outValSp, outValFull, rSpec->filterUel, symType, typeCode,
                        rSpec->dField, nnz, symDimX);
          setAttrib(outValFull, R_DimSymbol, dimVect);
          /* dimnamesnames done */
          setAttrib(rSpec->filterUel, R_NamesSymbol, outDomains);
          setAttrib(outValFull, R_DimNamesSymbol, rSpec->filterUel);
        }
        else {
          for (iDim = 0;  iDim < symDimX;  iDim++) {
            dimVal[iDim] = length(VECTOR_ELT(outUels, iDim));
            totalElement *= dimVal[iDim];
          }
          PROTECT(outValFull = allocVector(REALSXP, totalElement));
          rgdxAlloc++;
          sparseToFull (outValSp, outValFull, outUels, symType, typeCode,
                        rSpec->dField, mrows, symDimX);
          setAttrib(outValFull, R_DimSymbol, dimVect);
          if (R_NilValue != outDomains) {
            setAttrib(outUels, R_NamesSymbol, outDomains);
          }
          /* dimnamesnames done */
          setAttrib(outValFull, R_DimNamesSymbol, outUels);
        }

        if (rSpec->te) {   /* create full dimensional string matrix */
          PROTECT(outTeFull = allocVector(STRSXP, totalElement));
          rgdxAlloc++;
          if (rSpec->withUel) {
            createElementMatrix (outValSp, outTeSp, outTeFull, rSpec->filterUel, symDim, nnz);
            setAttrib(outTeFull, R_DimSymbol, dimVect);
            /* dimnamesnames done */
            setAttrib(outTeFull, R_DimNamesSymbol, rSpec->filterUel);
          }
          else {
            createElementMatrix (outValSp, outTeSp, outTeFull, outUels, symDim, mrows);
            setAttrib(outTeFull, R_DimSymbol, dimVect);
            /* dimnamesnames done */
            setAttrib(outTeFull, R_DimNamesSymbol, outUels);
          }
        } /* if rSpec->te */
        break;
      } /* switch(symDim) */
    }   /* if dForm = full */

    /* Creating output string for symbol name */
    PROTECT(outName = allocVector(STRSXP, 1) );
    SET_STRING_ELT(outName, 0, mkChar(symName));
    rgdxAlloc++;
    /* Creating output string for symbol type */
    PROTECT(outType = allocVector(STRSXP, 1) );
    rgdxAlloc++;
    switch (symType) {
    case GMS_DT_SET:
      SET_STRING_ELT(outType, 0, mkChar(types[0]) );
      break;
    case GMS_DT_PAR:
      SET_STRING_ELT(outType, 0, mkChar(types[1]) );
      break;
    case GMS_DT_VAR:
      SET_STRING_ELT(outType, 0, mkChar(types[2]) );
      break;
    case GMS_DT_EQU:
      SET_STRING_ELT(outType, 0, mkChar(types[3]) );
      break;
    default:
      error("Unrecognized type of symbol found.");
    }

    /* Creating int vector for symbol dim */
    PROTECT(outDim = allocVector(INTSXP, 1) );
    INTEGER(outDim)[0] = symDim;
    rgdxAlloc++;
    /* Creating string vector for val data form */
    PROTECT(outForm = allocVector(STRSXP, 1) );
    rgdxAlloc++;
    if (rSpec->dForm == full) {
      SET_STRING_ELT(outForm, 0, mkChar(forms[0]));
    }
    else {
      SET_STRING_ELT(outForm, 0, mkChar(forms[1]));
    }

    outElements++;       /* for $domains */
    outElements++;       /* for $domInfo */

    /* Create a string vector for symbol field */
    if (symType == GMS_DT_VAR || symType == GMS_DT_EQU) {
      outElements++;            /* for $field  */
      PROTECT(outField = allocVector(STRSXP, 1));
      rgdxAlloc++;
      switch(rSpec->dField) {
      case level:
        SET_STRING_ELT(outField, 0, mkChar (fields[GMS_VAL_LEVEL]));
        break;
      case marginal:
        SET_STRING_ELT(outField, 0, mkChar (fields[GMS_VAL_MARGINAL]));
        break;
      case lower:
        SET_STRING_ELT(outField, 0, mkChar (fields[GMS_VAL_LOWER]));
        break;
      case upper:
        SET_STRING_ELT(outField, 0, mkChar (fields[GMS_VAL_UPPER]));
        break;
      case scale:
        SET_STRING_ELT(outField, 0, mkChar (fields[GMS_VAL_SCALE]));
        break;
      case all:
        SET_STRING_ELT(outField, 0, mkChar (fields[GMS_VAL_MAX]));
        break;
      default:
        error("Unrecognized type of field found.");
      }
      if (GMS_DT_VAR == symType)
        outElements++;            /* one for $varTypeText */
      outElements++;            /* one for $typeCode */
    } /* symbol is var or equ */
    if (rSpec->ts) {
      outElements++;
      PROTECT(outTs = allocVector(STRSXP, 1));
      rgdxAlloc++;
      SET_STRING_ELT(outTs, 0, mkChar(symText));
    }
    if (rSpec->te) {
      outElements++;
    }
  } /* if (withList) aa */
  else {
    /* no requestList was input, so returning universe */
    /* Creating output string symbol name */
    PROTECT(outName = allocVector(STRSXP, 1));
    SET_STRING_ELT(outName, 0, mkChar("*"));
    rgdxAlloc++;
    /* Creating output string for symbol type */
    PROTECT(outType = allocVector(STRSXP, 1));
    rgdxAlloc++;
    SET_STRING_ELT(outType, 0, mkChar(types[0]));
    /* Creating int vector for symbol dim */
    PROTECT(outDim = allocVector(INTSXP, 1));
    INTEGER(outDim)[0] = 1;
    rgdxAlloc++;
  }

  PROTECT(outListNames = allocVector(STRSXP, outElements));
  rgdxAlloc++;
  /* populating list element names */
  iElement = 0;
  SET_STRING_ELT(outListNames, iElement, mkChar("name"));  iElement++;
  SET_STRING_ELT(outListNames, iElement, mkChar("type"));  iElement++;
  SET_STRING_ELT(outListNames, iElement, mkChar("dim"));  iElement++;
  SET_STRING_ELT(outListNames, iElement, mkChar("val"));  iElement++;
  SET_STRING_ELT(outListNames, iElement, mkChar("form"));  iElement++;
  SET_STRING_ELT(outListNames, iElement, mkChar("uels"));  iElement++;
  if (withList) {
    SET_STRING_ELT(outListNames, iElement, mkChar("domains"));  iElement++;
    SET_STRING_ELT(outListNames, iElement, mkChar("domInfo"));  iElement++;
    if (symType == GMS_DT_VAR || symType == GMS_DT_EQU) {
      SET_STRING_ELT(outListNames, iElement, mkChar("field"));  iElement++;
      if (GMS_DT_VAR == symType) {
        SET_STRING_ELT(outListNames, iElement, mkChar("varTypeText"));
        iElement++;
      }
      SET_STRING_ELT(outListNames, iElement, mkChar("typeCode"));  iElement++;
    }
    if (rSpec->ts) {
      SET_STRING_ELT(outListNames, iElement, mkChar("ts"));
      iElement++;
    }
    if (rSpec->te) {
      SET_STRING_ELT(outListNames, iElement, mkChar("te"));
      iElement++;
    }
  }
  if (iElement != outElements)
    error ("Internal error creating outListNames: iElement = %d  outElements = %d",
           iElement, outElements);

  PROTECT(outList = allocVector(VECSXP, outElements));
  rgdxAlloc++;

  /* populating list component vector */
  iElement = 0;
  SET_VECTOR_ELT(outList, iElement, outName);  iElement++;
  SET_VECTOR_ELT(outList, iElement, outType);  iElement++;
  SET_VECTOR_ELT(outList, iElement, outDim);  iElement++;
  if (withList) {
    if (rSpec->dForm == full) {
      SET_VECTOR_ELT(outList, iElement, outValFull);  iElement++;
    }
    else {
      SET_VECTOR_ELT(outList, iElement, outValSp);    iElement++;
    }
    SET_VECTOR_ELT(outList, iElement, outForm);       iElement++;
    if (reuseFilter) {
      SET_VECTOR_ELT(outList, iElement, rSpec->filterUel);  iElement++;
    }
    else {
      SET_VECTOR_ELT(outList, iElement, outUels);    iElement++;
    }
    SET_VECTOR_ELT(outList, iElement, outDomains);   iElement++;
    SET_STRING_ELT(outDomInfo, 0, mkChar(domInfoSrc));
    SET_VECTOR_ELT(outList, iElement, outDomInfo);   iElement++;

    if (symType == GMS_DT_VAR || symType == GMS_DT_EQU) {
      SET_VECTOR_ELT(outList, iElement, outField);   iElement++;

      if (GMS_DT_VAR == symType) {
        PROTECT(tmpExp = allocVector(STRSXP, 1));
        SET_STRING_ELT(tmpExp, 0, mkChar(gmsVarTypeText[typeCode]));
        SET_VECTOR_ELT(outList, iElement, tmpExp);   iElement++;
        UNPROTECT(1);
      }

      PROTECT(tmpExp = allocVector(INTSXP, 1));
      INTEGER(tmpExp)[0] = typeCode;
      SET_VECTOR_ELT(outList, iElement, tmpExp);     iElement++;
      UNPROTECT(1);
    }
    if (rSpec->ts) {
      SET_VECTOR_ELT(outList, iElement, outTs);      iElement++;
    }
    if (rSpec->te) {
      if (rSpec->dForm == full) {
        SET_VECTOR_ELT(outList, iElement, outTeFull); iElement++;
      }
      else {
        SET_VECTOR_ELT(outList, iElement, outTeSp);   iElement++;
      }
    }
  }
  else {
    /* no read specifier so return the universe */
    /* entering null values if nothing else makes sense */
    SET_VECTOR_ELT(outList, iElement, R_NilValue);      iElement++;

    SET_VECTOR_ELT(outList, iElement, R_NilValue);      iElement++;
    SET_VECTOR_ELT(outList, iElement, universe);        iElement++;
  }
  if (iElement != outElements)
    error ("Internal error creating outList: iElement = %d  outElements = %d",
           iElement, outElements);

  /* Setting attribute name */
  setAttrib(outList, R_NamesSymbol, outListNames);
  /* Releasing allocated memory */
  free(rSpec);
#if 0
  if (!gdxDataReadDone (gdxHandle)) {
    error ("Could not gdxDataReadDone");
  }
#endif
  errNum = gdxClose (gdxHandle);
  if (errNum != 0) {
    error("Errors detected when closing gdx file");
  }
  (void) gdxFree (&gdxHandle);
  UNPROTECT(rgdxAlloc);
  return outList;
} /* End of rgdx */
