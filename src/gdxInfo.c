/* gdxInfo.c
 * code for gdxrrw::gdxInfo
 * $Id: gdxInfo.c 52440 2015-06-11 21:08:22Z sdirkse $
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

static char *
val2str (gdxHandle_t h, double val, char *s)
{
  int sv;

  if (gdxAcronymName(h, val, s)) {
    return s;
  }
  else {
    gdxMapValue(h, val, &sv);
    if (sv_normal != sv)
      sprintf(s,"%s", gmsSVText[sv]);
    else
      sprintf(s,"%g", val);
    return s;
  }
} /* val2str */



#define GDXLIBRARYVER 0
#define GDXFILEVER    1
#define GDXPRODUCER   2
#define GDXSYMCOUNT   3
#define GDXUELCOUNT   4
#define GDXSETS       5
#define GDXPARS       6
#define GDXVARS       7
#define GDXEQUS       8
#define GDXALIASES    9
#define RETLIST_LEN   10

/* the gateway routine for gdxInfo.
 * If dump=T, dump out the content of the GDX file to the session/screen,
 * using a format similar to gdxdump.
 * If not, return meta-information about the symbols in the GDX file,
 * either as a list or in a data frame.
 */
SEXP
gdxInfoExt (SEXP args)
{
  const char *funcName = "gdxInfo";
  SEXP fileName;
  SEXP dumpExp, returnListExp, returnDFExp;
  SEXP result = R_NilValue;
  SEXP retList = R_NilValue;    /* list to return */
  SEXP elt[RETLIST_LEN];        /* list elements */
  SEXP listNames;

  int dump, returnList, returnDF;
  int arglen;
  shortStringBuf_t gdxFileName;
  int rc, i, j, k, nUels;
  int symDim, symCount;
  int symType, symType2;
  int symUser, symUser2;        /* user data - different meaning for different symbols types */
  int nRecs, FDim, BadUels=0;
  int iDummy;
  int allocCnt = 0;
  int iSym, nSyms;
  int iSet, nSets;
  int iPar, nPars;
  int iVar, nVars;
  int iEqu, nEqus;
  int iAli, nAliases;
  char msg[GMS_SSSIZE];
  char FileVersion[GMS_SSSIZE], FileProducer[GMS_SSSIZE];
  char symName[GMS_SSSIZE], symName2[GMS_SSSIZE], sText[GMS_SSSIZE], UelName[GMS_SSSIZE];
  double Vals[GMS_VAL_MAX];
  double dv[GMS_VAL_MAX];
  int Keys[GMS_MAX_INDEX_DIM];
  char *dn, c;
  char loadPath[GMS_SSSIZE];
  gdxStrIndex_t domNames;
  gdxStrIndexPtrs_t domPtrs;

#if 0
  SEXP ap, el;
  const char *name;

  Rprintf ("gdxInfo called with %d args\n", length(args));
  for (i = 0, ap = args;  ap != R_NilValue;  i++, ap = CDR(ap)) {
    name = isNull(TAG(ap)) ? "" : CHAR(PRINTNAME(TAG(ap)));
    el = CAR(ap);
    if (0 == length(el)) {
      if (NILSXP == TYPEOF(el))
        Rprintf ("[%d] '%s' NILSXP\n", i, name);
      else
        Rprintf ("[%d] '%s' R type, length 0\n", i, name);
      continue;
    }
    switch (TYPEOF(el)) {
    case REALSXP:
      Rprintf ("%d  [%d] '%s' REALSXP %g\n", TYPEOF(el), i, name, REAL(el)[0]);
      break;
    case STRSXP:
      Rprintf ("%d  [%d] '%s' STRSXP %s\n", TYPEOF(el), i, name, CHAR(STRING_ELT(el,0)));
      break;
    case LGLSXP:
      Rprintf ("%d  [%d] '%s' LGLSXP %d\n", TYPEOF(el), i, name, LOGICAL(el)[0]);
      break;
    default:
      Rprintf ("%d  [%d] '%s' unhandled R type\n", TYPEOF(el), i, name);
    }
  }
#endif

  GDXSTRINDEXPTRS_INIT (domNames, domPtrs);

  /* first arg is function name - ignore it */
  arglen = length(args);

  /* ----------------- Check the inputs ------------ */
  if (5 != arglen) {
    error ("usage: %s(gdxName=NULL, dump=TRUE, returnList=FALSE, returnDF=FALSE) - incorrect arg count", funcName);
  }
  args = CDR(args);             /* skip first arg: it's the function name */
  fileName = CAR(args);

  loadGDX();
  if (TYPEOF(fileName) == NILSXP) {
    /* no argument: just load GDX and print the version info */
    rc = gdxCreate (&gdxHandle, msg, sizeof(msg));
    if (0 == rc)
      error ("Error creating GDX object: %s", msg);
    gdxGetLoadPath (loadPath);
    Rprintf ("* Library location: %s\n", *loadPath ? loadPath : "unknown");
    gdxGetDLLVersion (gdxHandle, msg);
    Rprintf ("*  Library version: %s\n", msg);
    (void) gdxFree (&gdxHandle);
    return R_NilValue;
  }

  /* Checking that first argument is of type string */
  if (TYPEOF(fileName) != STRSXP) {
    error ("usage: %s - argument 'gdxName' must be a string", funcName);
  }

  dumpExp = CADR(args);
  /* Checking that dump argument is of type logical */
  if (TYPEOF(dumpExp) != LGLSXP) {
    error ("usage: %s - argument 'dump' must be a logical", funcName);
  }
  dump = LOGICAL(dumpExp)[0];

  returnListExp = CADDR(args);
  /* Checking that returnList argument is of type logical */
  if (TYPEOF(returnListExp) != LGLSXP) {
    error ("usage: %s - argument 'returnList' must be a logical", funcName);
  }
  returnList = LOGICAL(returnListExp)[0];

  returnDFExp = CADDDR(args);
  /* Checking that returnDF argument is of type logical */
  if (TYPEOF(returnDFExp) != LGLSXP) {
    error ("usage: %s - argument 'returnDF' must be a logical", funcName);
  }
  returnDF = LOGICAL(returnDFExp)[0];

  if (returnList && returnDF) {
    Rprintf ("Cannot return both a list and a data frame: setting returnList FALSE\n");
    returnList = 0;
  }

  (void) CHAR2ShortStr (CHAR(STRING_ELT(fileName, 0)), gdxFileName);
  checkFileExtension (gdxFileName);

  rc = gdxCreate (&gdxHandle, msg, sizeof(msg));
  if (0 == rc)
    error ("Error creating GDX object: %s", msg);
  rc = gdxOpenRead (gdxHandle, gdxFileName, &i);
  if (0 == rc) {
    gdxErrorStr (gdxHandle, i, msg);
    error ("Could not read GDX file %s: %s (rc=%d)\n", gdxFileName, msg, rc);
  }

  rc = gdxGetLastError (gdxHandle);
  if (rc) {
    gdxErrorStr (gdxHandle, rc, msg);
    Rprintf ("Problems processing GDX file %s: %s (rc=%d)\n",
             gdxFileName, msg, rc);
  }

  if (dump) {
    gdxFileVersion (gdxHandle, FileVersion, FileProducer);
    gdxSystemInfo (gdxHandle, &nSyms, &nUels);
    Rprintf("*  File version   : %s\n", FileVersion);
    Rprintf("*  Producer       : %s\n", FileProducer);
    Rprintf("*  Symbols        : %d\n", nSyms);
    Rprintf("*  Unique Elements: %d\n", nUels);

    /* Acroynms */
    for (i = 1;  i <= gdxAcronymCount (gdxHandle);  i++) {
      gdxAcronymGetInfo (gdxHandle, i, symName, sText, &rc);
      Rprintf("Acronym %s", symName);
      if (strlen(sText))
        Rprintf(" '%s'", sText);
      Rprintf(";\n");
    }

    /* Symbolinfo */
    Rprintf ("$ontext\n");
    for (iSym = 1;  iSym <= nSyms;  iSym++) {
      gdxSymbolInfo (gdxHandle, iSym, symName, &symDim, &symType);
      gdxSymbolInfoX (gdxHandle, iSym, &symCount, &rc, sText);
      Rprintf ("%-15s %3d %-12s %s\n", symName, symDim, gmsGdxTypeText[symType], sText);
    }
    Rprintf ("$offtext\n");

    Rprintf ("$onempty onembedded\n");
    dn = NULL;
    for (iSym = 1;  iSym <= nSyms;  iSym++) {
      gdxSymbolInfo (gdxHandle, iSym, symName, &symDim, &symType);
      gdxSymbolInfoX (gdxHandle, iSym, &symCount, &symUser, sText);

      if (GMS_DT_VAR == symType || GMS_DT_EQU == symType)
        Rprintf ("$ontext\n");

      if (GMS_DT_VAR == symType) {
        if (symUser < 0 || symUser>=GMS_VARTYPE_MAX)
          symUser = GMS_VARTYPE_FREE;
        MEMCPY (dv, gmsDefRecVar[symUser], GMS_VAL_MAX*sizeof(double));
        dn = (char *) gmsVarTypeText[symUser];
      }
      else if (GMS_DT_EQU == symType) {
        if (symUser < 0 || symUser>=GMS_EQUTYPE_MAX)
          symUser = GMS_EQUTYPE_E;
        MEMCPY (dv, gmsDefRecEqu[symUser], GMS_VAL_MAX*sizeof(double));
      }
      else
        dv[GMS_VAL_LEVEL] = 0.0;

      if (0 == symDim && GMS_DT_PAR == symType) /* Scalar */
        Rprintf ("Scalar");
      else {
        if (GMS_DT_VAR == symType)
          Rprintf ("%s ", dn);
        Rprintf ("%s", gmsGdxTypeText[symType]);
      }
      if (GMS_DT_ALIAS == symType) {
        gdxSymbolInfo (gdxHandle, symUser, symName2, &j, &symType2);
        Rprintf (" (%s, %s);\n", symName, symName2);
      }
      else {
        Rprintf(" %s", symName);
        if (symDim > 0) {
          /* should probably use gdxSymbolGetDomainX instead */
          gdxSymbolGetDomain (gdxHandle, iSym, Keys);
          Rprintf ("(");
          for (j = 0;  j < symDim;  j++) {
            if (Keys[j]==0)
              strcpy (symName2,"*");
            else
              gdxSymbolInfo (gdxHandle, Keys[j], symName2, &symUser2, &symType2);
            if (j < symDim-1)
              Rprintf ("%s,", symName2);
            else
              Rprintf ("%s)", symName2);
          }
        }
        if (strlen(sText))
          Rprintf(" '%s'", sText);
      }
      if (0 == symCount) {
        if (0 == symDim && GMS_DT_PAR == symType) /* Scalar */
          Rprintf (" / 0.0 /;\n");
        else if (GMS_DT_ALIAS != symType)
          Rprintf (" / /;\n");
      }
      else {
        Rprintf ("/\n");
        gdxDataReadRawStart (gdxHandle, iSym, &nRecs);
        while (gdxDataReadRaw (gdxHandle, Keys, Vals, &FDim)) {
          if ((GMS_DT_VAR == symType || GMS_DT_EQU == symType) && 0 == memcmp(Vals,dv,GMS_VAL_MAX*sizeof(double))) /* all default records */
            continue;
          if (GMS_DT_PAR == symType && 0.0 == Vals[GMS_VAL_LEVEL])
            continue;
          for (j = 1;  j <= symDim;  j++) {
            if (1 == gdxUMUelGet (gdxHandle, Keys[j-1], UelName, &iDummy))
              Rprintf ("'%s'", UelName);
            else {
              Rprintf ("L__", Keys[j-1]);
              BadUels++;
            }
            if (j < symDim)
              Rprintf (".");
          }
          if (GMS_DT_PAR == symType)
            Rprintf(" %s\n", val2str(gdxHandle, Vals[GMS_VAL_LEVEL], msg));
          else if (GMS_DT_SET == symType)
            if (Vals[GMS_VAL_LEVEL]) {
              j = (int) Vals[GMS_VAL_LEVEL];
              gdxGetElemText (gdxHandle, j, msg, &iDummy);
              Rprintf (" '%s'\n", msg);
            }
            else
              Rprintf ("\n");
          else if (GMS_DT_VAR == symType || GMS_DT_EQU == symType) {
            Rprintf (" .");
            c = '(';
            for (j = GMS_VAL_LEVEL; j < GMS_VAL_MAX;  j++) {
              if (Vals[j] != dv[j]) {
                if (GMS_VAL_SCALE == j && GMS_DT_VAR == symType &&
                    symUser != GMS_VARTYPE_POSITIVE && symUser != GMS_VARTYPE_NEGATIVE && symUser != GMS_VARTYPE_FREE)
                  Rprintf ("%c prior %s", c, val2str (gdxHandle, Vals[GMS_VAL_SCALE], msg));
                else
                  Rprintf ("%c %s %s", c, gmsValTypeText[j]+1, val2str(gdxHandle, Vals[j], msg));
                if ('(' == c)
                  c = ',';
              }
            }
            Rprintf (" )\n");
          }
        }
        Rprintf ("/;\n");
      } /* if 0 == symCount .. else .. */
      j = 1;
      while (gdxSymbolGetComment (gdxHandle, iSym, j++, msg))
        Rprintf ("* %s\n", msg);
      if (GMS_DT_VAR == symType || GMS_DT_EQU == symType)
        Rprintf ("$offtext\n");
      Rprintf("\n");
    }
    Rprintf ("$offempty offembedded\n");

    if (BadUels > 0)
      Rprintf("**** %d reference(s) to unique elements without a string representation\n", BadUels);
  }

  nSets = nPars = nVars = nEqus = nAliases = 0;
  if (returnList || returnDF) {
    /* generate the components for the list */
    gdxGetDLLVersion (gdxHandle, msg);
    PROTECT(elt[GDXLIBRARYVER] = allocVector(STRSXP, 1));
    allocCnt++;
    SET_STRING_ELT(elt[GDXLIBRARYVER], 0, mkChar(msg));

    gdxFileVersion (gdxHandle, FileVersion, FileProducer);
    PROTECT(elt[GDXFILEVER] = allocVector(STRSXP, 1));
    allocCnt++;
    SET_STRING_ELT(elt[GDXFILEVER], 0, mkChar(FileVersion));
    PROTECT(elt[GDXPRODUCER] = allocVector(STRSXP, 1));
    allocCnt++;
    SET_STRING_ELT(elt[GDXPRODUCER], 0, mkChar(FileProducer));

    gdxSystemInfo (gdxHandle, &nSyms, &nUels);
    PROTECT(elt[GDXSYMCOUNT] = allocVector(INTSXP, 1));
    allocCnt++;
    INTEGER(elt[GDXSYMCOUNT])[0] = nSyms;
    PROTECT(elt[GDXUELCOUNT] = allocVector(INTSXP, 1));
    allocCnt++;
    INTEGER(elt[GDXUELCOUNT])[0] = nUels;

    for (iSym = 1;  iSym <= nSyms;  iSym++) {
      gdxSymbolInfo (gdxHandle, iSym, symName, &symDim, &symType);
      switch (symType) {
      case GMS_DT_SET:
        nSets++;
        break;
      case GMS_DT_PAR:
        nPars++;
        break;
      case GMS_DT_VAR:
        nVars++;
        break;
      case GMS_DT_EQU:
        nEqus++;
        break;
      case GMS_DT_ALIAS:
        nAliases++;
        break;
      }
    }

    /* generate the names for the list */
    PROTECT(listNames = allocVector(STRSXP, RETLIST_LEN));
    allocCnt++;
    SET_STRING_ELT(listNames, GDXLIBRARYVER, mkChar("gdxLibraryVer"));
    SET_STRING_ELT(listNames, GDXFILEVER   , mkChar("gdxFileVer"));
    SET_STRING_ELT(listNames, GDXPRODUCER  , mkChar("producer"));
    SET_STRING_ELT(listNames, GDXSYMCOUNT  , mkChar("symCount"));
    SET_STRING_ELT(listNames, GDXUELCOUNT  , mkChar("uelCount"));
    SET_STRING_ELT(listNames, GDXSETS      , mkChar("sets"));
    SET_STRING_ELT(listNames, GDXPARS      , mkChar("parameters"));
    SET_STRING_ELT(listNames, GDXVARS      , mkChar("variables"));
    SET_STRING_ELT(listNames, GDXEQUS      , mkChar("equations"));
    SET_STRING_ELT(listNames, GDXALIASES   , mkChar("aliases"));

    PROTECT(retList = allocVector(VECSXP, RETLIST_LEN));
    allocCnt++;

    /* populating retList with its common components */
    for (i = 0;  i <= GDXUELCOUNT;  i++) {
      SET_VECTOR_ELT (retList, i, elt[i]);
    }
    /* put in the names for the components */
    setAttrib (retList, R_NamesSymbol, listNames);
    result = retList;
  } /* if (returnList or returnDF */

  if (returnList) {
    PROTECT(elt[GDXSETS] = allocVector(STRSXP, nSets));
    allocCnt++;
    PROTECT(elt[GDXPARS] = allocVector(STRSXP, nPars));
    allocCnt++;
    PROTECT(elt[GDXVARS] = allocVector(STRSXP, nVars));
    allocCnt++;
    PROTECT(elt[GDXEQUS] = allocVector(STRSXP, nEqus));
    allocCnt++;
    PROTECT(elt[GDXALIASES] = allocVector(STRSXP, nAliases));
    allocCnt++;

    iSet = iPar = iVar = iEqu = iAli = 0;
    for (iSym = 1;  iSym <= nSyms;  iSym++) {
      gdxSymbolInfo (gdxHandle, iSym, symName, &symDim, &symType);
      switch (symType) {
      case GMS_DT_SET:
        SET_STRING_ELT(elt[GDXSETS], iSet, mkChar(symName));
        iSet++;
        break;
      case GMS_DT_PAR:
        SET_STRING_ELT(elt[GDXPARS], iPar, mkChar(symName));
        iPar++;
        break;
      case GMS_DT_VAR:
        SET_STRING_ELT(elt[GDXVARS], iVar, mkChar(symName));
        iVar++;
        break;
      case GMS_DT_EQU:
        SET_STRING_ELT(elt[GDXEQUS], iEqu, mkChar(symName));
        iEqu++;
        break;
      case GMS_DT_ALIAS:
        SET_STRING_ELT(elt[GDXALIASES], iAli, mkChar(symName));
        iAli++;
        break;
      }
    }

    /* populating retList with its returnList-specific components */
    for (i = GDXSETS;  i < RETLIST_LEN;  i++) {
      SET_VECTOR_ELT (retList, i, elt[i]);
    }
  } /* if (returnList) */
  else if (returnDF) {
    SEXP dfClass, asIsClass;
    SEXP domTmp, domnamesTmp;

    /* columns for sets DF: name, index, dim, card, text, doms, domnames */
    SEXP setName, setIndex, setDim, setCard, setText, setDoms, setDomnames;
    SEXP setColNames, setRowNames;

    /* columns for parameters DF: name, index, dim, card, text, doms, domnames */
    SEXP parName, parIndex, parDim, parCard, parText, parDoms, parDomnames;
    SEXP parColNames, parRowNames;

    /* columns for variables DF: name, index, dim, card, text, doms, domnames */
    SEXP varName, varIndex, varDim, varCard, varText, varDoms, varDomnames;
    SEXP varColNames, varRowNames;

    /* columns for equations DF: name, index, dim, card, text, doms, domnames */
    SEXP equName, equIndex, equDim, equCard, equText, equDoms, equDomnames;
    SEXP equColNames, equRowNames;

    /* columns for aliases DF: name, index, base */
    SEXP aliName, aliIndex, aliBase;
    SEXP aliColNames, aliRowNames;

    PROTECT(dfClass = allocVector(STRSXP, 1));
    allocCnt++;
    SET_STRING_ELT(dfClass, 0, mkChar("data.frame"));
    PROTECT(asIsClass = allocVector(STRSXP, 1));
    allocCnt++;
    SET_STRING_ELT(asIsClass, 0, mkChar("AsIs"));

    PROTECT(setName = allocVector(STRSXP, nSets));
    allocCnt++;
    PROTECT(setIndex = allocVector(INTSXP, nSets));
    allocCnt++;
    PROTECT(setDim = allocVector(INTSXP, nSets));
    allocCnt++;
    PROTECT(setCard = allocVector(INTSXP, nSets));
    allocCnt++;
    PROTECT(setText = allocVector(STRSXP, nSets));
    allocCnt++;
    PROTECT(setDoms = allocVector(VECSXP, nSets));
    allocCnt++;
    classgets (setDoms, asIsClass);
    PROTECT(setDomnames = allocVector(VECSXP, nSets));
    allocCnt++;
    classgets (setDomnames, asIsClass);

    PROTECT(parName = allocVector(STRSXP, nPars));
    allocCnt++;
    PROTECT(parIndex = allocVector(INTSXP, nPars));
    allocCnt++;
    PROTECT(parDim = allocVector(INTSXP, nPars));
    allocCnt++;
    PROTECT(parCard = allocVector(INTSXP, nPars));
    allocCnt++;
    PROTECT(parText = allocVector(STRSXP, nPars));
    allocCnt++;
    PROTECT(parDoms = allocVector(VECSXP, nPars));
    allocCnt++;
    classgets (parDoms, asIsClass);
    PROTECT(parDomnames = allocVector(VECSXP, nPars));
    allocCnt++;
    classgets (parDomnames, asIsClass);

    PROTECT(varName = allocVector(STRSXP, nVars));
    allocCnt++;
    PROTECT(varIndex = allocVector(INTSXP, nVars));
    allocCnt++;
    PROTECT(varDim = allocVector(INTSXP, nVars));
    allocCnt++;
    PROTECT(varCard = allocVector(INTSXP, nVars));
    allocCnt++;
    PROTECT(varText = allocVector(STRSXP, nVars));
    allocCnt++;
    PROTECT(varDoms = allocVector(VECSXP, nVars));
    allocCnt++;
    classgets (varDoms, asIsClass);
    PROTECT(varDomnames = allocVector(VECSXP, nVars));
    allocCnt++;
    classgets (varDomnames, asIsClass);

    PROTECT(equName = allocVector(STRSXP, nEqus));
    allocCnt++;
    PROTECT(equIndex = allocVector(INTSXP, nEqus));
    allocCnt++;
    PROTECT(equDim = allocVector(INTSXP, nEqus));
    allocCnt++;
    PROTECT(equCard = allocVector(INTSXP, nEqus));
    allocCnt++;
    PROTECT(equText = allocVector(STRSXP, nEqus));
    allocCnt++;
    PROTECT(equDoms = allocVector(VECSXP, nEqus));
    allocCnt++;
    classgets (equDoms, asIsClass);
    PROTECT(equDomnames = allocVector(VECSXP, nEqus));
    allocCnt++;
    classgets (equDomnames, asIsClass);

    PROTECT(aliName = allocVector(STRSXP, nAliases));
    allocCnt++;
    PROTECT(aliIndex = allocVector(INTSXP, nAliases));
    allocCnt++;
    PROTECT(aliBase = allocVector(INTSXP, nAliases));
    allocCnt++;

    iSet = iPar = iVar = iEqu = iAli = 0;
    for (iSym = 1;  iSym <= nSyms;  iSym++) {
      gdxSymbolInfo (gdxHandle, iSym, symName, &symDim, &symType);
      gdxSymbolInfoX (gdxHandle, iSym, &symCount, &symUser, sText);
      switch (symType) {
      case GMS_DT_SET:
        SET_STRING_ELT(setName, iSet, mkChar(symName));
        INTEGER(setIndex)[iSet] = iSym;
        INTEGER(setDim)[iSet] = symDim;
        INTEGER(setCard)[iSet] = symCount;
        SET_STRING_ELT(setText, iSet, mkChar(sText));
        PROTECT(domTmp = allocVector(INTSXP, symDim));
        allocCnt++;
        gdxSymbolGetDomain (gdxHandle, iSym, Keys);
        for (k = 0;  k < symDim;  k++) {
          INTEGER(domTmp)[k] = Keys[k];
        }
        SET_VECTOR_ELT(setDoms, iSet, domTmp);
        domTmp = R_NilValue;
        PROTECT(domnamesTmp = allocVector(STRSXP, symDim));
        allocCnt++;
        (void) gdxSymbolGetDomainX (gdxHandle, iSym, domPtrs);
        for (k = 0;  k < symDim;  k++) {
          SET_STRING_ELT(domnamesTmp, k, mkChar(domPtrs[k]));
        }
        SET_VECTOR_ELT(setDomnames, iSet, domnamesTmp);
        domnamesTmp = R_NilValue;
        iSet++;
        break;
      case GMS_DT_PAR:
        SET_STRING_ELT(parName, iPar, mkChar(symName));
        INTEGER(parIndex)[iPar] = iSym;
        INTEGER(parDim)[iPar] = symDim;
        INTEGER(parCard)[iPar] = symCount;
        SET_STRING_ELT(parText, iPar, mkChar(sText));
        PROTECT(domTmp = allocVector(INTSXP, symDim));
        allocCnt++;
        gdxSymbolGetDomain (gdxHandle, iSym, Keys);
        for (k = 0;  k < symDim;  k++) {
          INTEGER(domTmp)[k] = Keys[k];
        }
        SET_VECTOR_ELT(parDoms, iPar, domTmp);
        domTmp = R_NilValue;
        PROTECT(domnamesTmp = allocVector(STRSXP, symDim));
        allocCnt++;
        (void) gdxSymbolGetDomainX (gdxHandle, iSym, domPtrs);
        for (k = 0;  k < symDim;  k++) {
          SET_STRING_ELT(domnamesTmp, k, mkChar(domPtrs[k]));
        }
        SET_VECTOR_ELT(parDomnames, iPar, domnamesTmp);
        domnamesTmp = R_NilValue;
        iPar++;
        break;
      case GMS_DT_VAR:
        SET_STRING_ELT(varName, iVar, mkChar(symName));
        INTEGER(varIndex)[iVar] = iSym;
        INTEGER(varDim)[iVar] = symDim;
        INTEGER(varCard)[iVar] = symCount;
        SET_STRING_ELT(varText, iVar, mkChar(sText));
        PROTECT(domTmp = allocVector(INTSXP, symDim));
        allocCnt++;
        gdxSymbolGetDomain (gdxHandle, iSym, Keys);
        for (k = 0;  k < symDim;  k++) {
          INTEGER(domTmp)[k] = Keys[k];
        }
        SET_VECTOR_ELT(varDoms, iVar, domTmp);
        domTmp = R_NilValue;
        PROTECT(domnamesTmp = allocVector(STRSXP, symDim));
        allocCnt++;
        (void) gdxSymbolGetDomainX (gdxHandle, iSym, domPtrs);
        for (k = 0;  k < symDim;  k++) {
          SET_STRING_ELT(domnamesTmp, k, mkChar(domPtrs[k]));
        }
        SET_VECTOR_ELT(varDomnames, iVar, domnamesTmp);
        domnamesTmp = R_NilValue;
        iVar++;
        break;
      case GMS_DT_EQU:
        SET_STRING_ELT(equName, iEqu, mkChar(symName));
        INTEGER(equIndex)[iEqu] = iSym;
        INTEGER(equDim)[iEqu] = symDim;
        INTEGER(equCard)[iEqu] = symCount;
        SET_STRING_ELT(equText, iEqu, mkChar(sText));
        PROTECT(domTmp = allocVector(INTSXP, symDim));
        allocCnt++;
        gdxSymbolGetDomain (gdxHandle, iSym, Keys);
        for (k = 0;  k < symDim;  k++) {
          INTEGER(domTmp)[k] = Keys[k];
        }
        SET_VECTOR_ELT(equDoms, iEqu, domTmp);
        domTmp = R_NilValue;
        PROTECT(domnamesTmp = allocVector(STRSXP, symDim));
        allocCnt++;
        (void) gdxSymbolGetDomainX (gdxHandle, iSym, domPtrs);
        for (k = 0;  k < symDim;  k++) {
          SET_STRING_ELT(domnamesTmp, k, mkChar(domPtrs[k]));
        }
        SET_VECTOR_ELT(equDomnames, iEqu, domnamesTmp);
        domnamesTmp = R_NilValue;
        iEqu++;
        break;
      case GMS_DT_ALIAS:
        SET_STRING_ELT(aliName, iAli, mkChar(symName));
        INTEGER(aliIndex)[iAli] = iSym;
        INTEGER(aliBase)[iAli] = symUser;
        iAli++;
        break;
      }
    } /* loop over symbols */

    /* -------------------------------------------------------------- */
    PROTECT(elt[GDXSETS] = allocVector(VECSXP, 7));
    allocCnt++;

    SET_VECTOR_ELT(elt[GDXSETS], 0, setName);
    SET_VECTOR_ELT(elt[GDXSETS], 1, setIndex);
    SET_VECTOR_ELT(elt[GDXSETS], 2, setDim);
    SET_VECTOR_ELT(elt[GDXSETS], 3, setCard);
    SET_VECTOR_ELT(elt[GDXSETS], 4, setText);
    SET_VECTOR_ELT(elt[GDXSETS], 5, setDoms);
    SET_VECTOR_ELT(elt[GDXSETS], 6, setDomnames);

    PROTECT(setColNames = allocVector(STRSXP, 7));
    allocCnt++;
    /* columns for sets DF: name, index, dim, card, text, doms, domnames */
    SET_STRING_ELT(setColNames, 0, mkChar("name"));
    SET_STRING_ELT(setColNames, 1, mkChar("index"));
    SET_STRING_ELT(setColNames, 2, mkChar("dim"));
    SET_STRING_ELT(setColNames, 3, mkChar("card"));
    SET_STRING_ELT(setColNames, 4, mkChar("text"));
    SET_STRING_ELT(setColNames, 5, mkChar("doms"));
    SET_STRING_ELT(setColNames, 6, mkChar("domnames"));
    PROTECT(setRowNames = allocVector(INTSXP, nSets));
    allocCnt++;
    for (iSet = 0;  iSet < nSets;  iSet++)
      INTEGER(setRowNames)[iSet] = iSet+1;

    setAttrib (elt[GDXSETS], R_NamesSymbol, setColNames);
    setAttrib (elt[GDXSETS], R_RowNamesSymbol, setRowNames);
    classgets (elt[GDXSETS], dfClass);

    /* -------------------------------------------------------------- */
    PROTECT(elt[GDXPARS] = allocVector(VECSXP, 7));
    allocCnt++;

    SET_VECTOR_ELT(elt[GDXPARS], 0, parName);
    SET_VECTOR_ELT(elt[GDXPARS], 1, parIndex);
    SET_VECTOR_ELT(elt[GDXPARS], 2, parDim);
    SET_VECTOR_ELT(elt[GDXPARS], 3, parCard);
    SET_VECTOR_ELT(elt[GDXPARS], 4, parText);
    SET_VECTOR_ELT(elt[GDXPARS], 5, parDoms);
    SET_VECTOR_ELT(elt[GDXPARS], 6, parDomnames);

    PROTECT(parColNames = allocVector(STRSXP, 7));
    allocCnt++;
    /* columns for parameters DF: name, index, dim, card, text, doms, domnames */
    SET_STRING_ELT(parColNames, 0, mkChar("name"));
    SET_STRING_ELT(parColNames, 1, mkChar("index"));
    SET_STRING_ELT(parColNames, 2, mkChar("dim"));
    SET_STRING_ELT(parColNames, 3, mkChar("card"));
    SET_STRING_ELT(parColNames, 4, mkChar("text"));
    SET_STRING_ELT(parColNames, 5, mkChar("doms"));
    SET_STRING_ELT(parColNames, 6, mkChar("domnames"));
    PROTECT(parRowNames = allocVector(INTSXP, nPars));
    allocCnt++;
    for (iPar = 0;  iPar < nPars;  iPar++)
      INTEGER(parRowNames)[iPar] = iPar+1;

    setAttrib (elt[GDXPARS], R_NamesSymbol, parColNames);
    setAttrib (elt[GDXPARS], R_RowNamesSymbol, parRowNames);
    classgets (elt[GDXPARS], dfClass);

    /* -------------------------------------------------------------- */
    PROTECT(elt[GDXVARS] = allocVector(VECSXP, 7));
    allocCnt++;

    SET_VECTOR_ELT(elt[GDXVARS], 0, varName);
    SET_VECTOR_ELT(elt[GDXVARS], 1, varIndex);
    SET_VECTOR_ELT(elt[GDXVARS], 2, varDim);
    SET_VECTOR_ELT(elt[GDXVARS], 3, varCard);
    SET_VECTOR_ELT(elt[GDXVARS], 4, varText);
    SET_VECTOR_ELT(elt[GDXVARS], 5, varDoms);
    SET_VECTOR_ELT(elt[GDXVARS], 6, varDomnames);

    PROTECT(varColNames = allocVector(STRSXP, 7));
    allocCnt++;
    /* columns for variables DF: name, index, dim, card, text, doms, domnames */
    SET_STRING_ELT(varColNames, 0, mkChar("name"));
    SET_STRING_ELT(varColNames, 1, mkChar("index"));
    SET_STRING_ELT(varColNames, 2, mkChar("dim"));
    SET_STRING_ELT(varColNames, 3, mkChar("card"));
    SET_STRING_ELT(varColNames, 4, mkChar("text"));
    SET_STRING_ELT(varColNames, 5, mkChar("doms"));
    SET_STRING_ELT(varColNames, 6, mkChar("domnames"));
    PROTECT(varRowNames = allocVector(INTSXP, nVars));
    allocCnt++;
    for (iVar = 0;  iVar < nVars;  iVar++)
      INTEGER(varRowNames)[iVar] = iVar+1;

    setAttrib (elt[GDXVARS], R_NamesSymbol, varColNames);
    setAttrib (elt[GDXVARS], R_RowNamesSymbol, varRowNames);
    classgets (elt[GDXVARS], dfClass);

    /* -------------------------------------------------------------- */
    PROTECT(elt[GDXEQUS] = allocVector(VECSXP, 7));
    allocCnt++;

    SET_VECTOR_ELT(elt[GDXEQUS], 0, equName);
    SET_VECTOR_ELT(elt[GDXEQUS], 1, equIndex);
    SET_VECTOR_ELT(elt[GDXEQUS], 2, equDim);
    SET_VECTOR_ELT(elt[GDXEQUS], 3, equCard);
    SET_VECTOR_ELT(elt[GDXEQUS], 4, equText);
    SET_VECTOR_ELT(elt[GDXEQUS], 5, equDoms);
    SET_VECTOR_ELT(elt[GDXEQUS], 6, equDomnames);

    PROTECT(equColNames = allocVector(STRSXP, 7));
    allocCnt++;
    /* columns for equations DF: name, index, dim, card, text doms, domnames */
    SET_STRING_ELT(equColNames, 0, mkChar("name"));
    SET_STRING_ELT(equColNames, 1, mkChar("index"));
    SET_STRING_ELT(equColNames, 2, mkChar("dim"));
    SET_STRING_ELT(equColNames, 3, mkChar("card"));
    SET_STRING_ELT(equColNames, 4, mkChar("text"));
    SET_STRING_ELT(equColNames, 5, mkChar("doms"));
    SET_STRING_ELT(equColNames, 6, mkChar("domnames"));
    PROTECT(equRowNames = allocVector(INTSXP, nEqus));
    allocCnt++;
    for (iEqu = 0;  iEqu < nEqus;  iEqu++)
      INTEGER(equRowNames)[iEqu] = iEqu+1;

    setAttrib (elt[GDXEQUS], R_NamesSymbol, equColNames);
    setAttrib (elt[GDXEQUS], R_RowNamesSymbol, equRowNames);
    classgets (elt[GDXEQUS], dfClass);

    /* -------------------------------------------------------------- */
    PROTECT(elt[GDXALIASES] = allocVector(VECSXP, 3));
    allocCnt++;

    SET_VECTOR_ELT(elt[GDXALIASES], 0, aliName);
    SET_VECTOR_ELT(elt[GDXALIASES], 1, aliIndex);
    SET_VECTOR_ELT(elt[GDXALIASES], 2, aliBase);

    PROTECT(aliColNames = allocVector(STRSXP, 3));
    allocCnt++;
    /* columns for alias DF: name, index, base */
    SET_STRING_ELT(aliColNames, 0, mkChar("name"));
    SET_STRING_ELT(aliColNames, 1, mkChar("index"));
    SET_STRING_ELT(aliColNames, 2, mkChar("base"));
    PROTECT(aliRowNames = allocVector(INTSXP, nAliases));
    allocCnt++;
    for (iAli = 0;  iAli < nAliases;  iAli++)
      INTEGER(aliRowNames)[iAli] = iAli+1;

    setAttrib (elt[GDXALIASES], R_NamesSymbol, aliColNames);
    setAttrib (elt[GDXALIASES], R_RowNamesSymbol, aliRowNames);
    classgets (elt[GDXALIASES], dfClass);

    /* populating retList with its returnDF-specific components */
    for (i = GDXSETS;  i < RETLIST_LEN;  i++) {
      SET_VECTOR_ELT (retList, i, elt[i]);
    }
  }   /* returnDF */

  (void) gdxClose (gdxHandle);
  gdxFree (&gdxHandle);

  UNPROTECT(allocCnt);
  return result;
} /* gdxInfo */
