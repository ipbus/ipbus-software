#ifdef USE_BACKTRACE

/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.


      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

---------------------------------------------------------------------------
*/

/**
  @file
  @author Andrew W. Rose
  @date 2013
*/

#ifndef _uhal_log_BacktraceSymbols_hpp_
#define _uhal_log_BacktraceSymbols_hpp_


#include <cstddef>
#include <stdint.h>
#include <vector>
#include <string>
#include <boost/thread/mutex.hpp>

namespace Backtrace
{
  struct TracePoint
  {
    TracePoint() : file ( "" ) , function ( "" ) , line ( 0 ) {}

    std::string file;
    std::string function;
    uint32_t line;
  };


  void Backtrace ( std::vector< void* >& aBacktrace );

  std::vector< TracePoint > BacktraceSymbols ( const std::vector< void* >& aBacktrace );
}

#endif

#endif
