#if defined(__GNUC__)
# define _GNU_SOURCE
/* _GNU_SOURCE assures that dladdr() is available */
#endif

#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#if defined(__linux__) || defined(__sparc) || defined(__sun) || defined(__sun__) || defined(__APPLE__)
# include <dlfcn.h>
#endif

#if defined(_WIN32)
# define WIN32_LEAN_AND_MEAN   /* google it */
# include <windows.h>
# define snprintf _snprintf
#endif

#include "loadpathutil.h"

#if defined (_WIN32)
void loadPathHack (char buf[256], void *addr)
{
  HMODULE h;
  size_t sz;
  DWORD k;
  BOOL brc;
  char libBuf[1024], dirBuf[1024], drive[3], *p, *s, *end;

  buf[0] = '\0';
  brc = GetModuleHandleEx (GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
                           GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                           (LPCTSTR)addr, &h);
  if (brc) {  /* OK: got a handle */
    k = GetModuleFileName (h, libBuf, sizeof(libBuf));
    if (k > 0) {                /* success */
      p = dirBuf + 3;
      _splitpath(libBuf,drive,p,NULL,NULL);
      /* trim trailing slashes */
      end = p + strlen(p);
      s = end - 1;
      while ( (s >= p) &&
	      (('/' == *s) || ('\\' == *s)) )
	s--;
      if (s >= p) {		/* s is a non-slash, part of the dir */
	*++s = '\0';
      }
      /* prepend the drive */
      sz = strlen(drive);
      s = drive + sz;
      while (s > drive)
	*--p = *--s;
      sz = strlen(p);
      if (sz <= 255) {
        memcpy (buf, p, sz);
        buf[sz] = '\0';
      }
    }
  }

  return;
} /* loadPathHack */

#elif defined(__linux__) || defined(__sparc) || defined(__sun) || defined(__sun__) || defined(__APPLE__)
void loadPathHack (char buf[256], void *addr)
{
  const char *p;
  Dl_info dlInfo;
  size_t sz;
  int rc;
  char libBuf[PATH_MAX];

  buf[0] = '\0';
  rc = dladdr (addr, &dlInfo);
  if (rc && ('\0' != *dlInfo.dli_fname)) {
    if (realpath(dlInfo.dli_fname, libBuf)) { /* success */
      sz = strlen(libBuf);
      p = libBuf + sz;
      while (('/' != *p) && (p > libBuf)) {
        p--;
      }
      if (p > libBuf)           /* must have stopped on the rightmost / */
        p--;
      sz = p - libBuf + 1;
      if (sz <= 255) {
        memcpy (buf, libBuf, sz);
        buf[sz] = '\0';
      }
    }
  }
  return;
} /* loadPathHack */

#else

/* The code to do this for AIX is UGLY!!!
 * Probably nobody notices or cares if I leave it out.
 */

void loadPathHack (char buf[256], void *addr)
{
  buf[0] = '\0';

  return;
} /* loadPathHack */

#endif
