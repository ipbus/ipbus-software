#ifndef _uhal_log_hpp_
#define _uhal_log_hpp_

#include "pantheios/pantheios.hpp"
#include <pantheios/inserters/integer.hpp>
#include "pantheios/frontends/stock.h"

#define log_LOCATION log_DEBUG ( __PRETTY_FUNCTION__ , " in " , __FILE__ , ", line " , pantheios::integer(__LINE__) )


#endif
