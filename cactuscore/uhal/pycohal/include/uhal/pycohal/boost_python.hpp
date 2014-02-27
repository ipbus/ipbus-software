#ifndef _uhal_pycohal_patches_hpp_
#define _uhal_pycohal_patches_hpp_

#ifdef __APPLE__

#ifdef _PY_PORT_CTYPE_UTF8_ISSUE
#error "You have already included pyport.h, probably indirectly by including boost.python. Use uhal/pycohal/patches.hpp instead."
#endif

#include "boost/python.hpp"

#undef isalnum
#undef isalpha
#undef islower
#undef isspace
#undef isupper
#undef tolower
#undef toupper

#else
#include "boost/python.hpp"
#endif /* __APPLE__ */

#endif
