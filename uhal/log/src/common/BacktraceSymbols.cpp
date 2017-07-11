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

/*
  This is an object-oriented rewrite of an implementation by Jeff Muizelaar
  It passes off all horrid memory allocations
  It has no global variables
  Copyright 2013 Andrew W. Rose
*/

/*
  A hacky replacement for backtrace_symbols in glibc

  backtrace_symbols in glibc looks up symbols using dladdr which is limited in
  the symbols that it sees. libbacktracesymbols opens the executable and shared
  libraries using libbfd and will look up backtrace information using the symbol
  table and the dwarf lLine information.

  It may make more sense for this program to use libelf instead of libbfd.
  However, I have not investigated that yet.

  Derived from addr2line.c from GNU Binutils by Jeff Muizelaar

  Copyright 2007 Jeff Muizelaar
*/

/* addr2line.c -- convert addresses to lLine number and lFunction name
   Copyright 1997, 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Ulrich Lauther <Ulrich.Lauther@mchp.siemens.de>

   This file was part of GNU Binutils.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */


#include <config.h>
#include <string.h>
#include <execinfo.h>
#include <bfd.h>
#define HAVE_DECL_BASENAME 1
#include <libiberty.h>
#include <dlfcn.h>
#include <link.h>
#include <cxxabi.h>
#include <stdlib.h>

#include <cstddef>
#include <vector>
#include <string>
#include <iostream>

#include "uhal/log/BacktraceSymbols.hpp"


namespace Backtrace
{

  /* helper struct for passing data to callbacks */
  struct FindFilesHelperStruct
  {
    const char* lFile;
    void* address;
    void* base;
    void* hdr;
  };

  /* helper struct for passing data to callbacks */
  struct FindAddressHelperStruct
  {
    bfd_vma pc;
    asymbol** syms;
    uint32_t found;
    TracePoint tracepoint;
  };


  /* Callback for bfd_map_over_sections */
  static void FindAddressInSection ( bfd* aBfd, asection* aSection, void* aData )
  {
    FindAddressHelperStruct* lFindAddress = ( FindAddressHelperStruct* ) ( aData );

    if ( ( bfd_get_section_flags ( aBfd, aSection ) & SEC_ALLOC ) == 0 )
    {
      return;
    }

    bfd_vma vma = bfd_get_section_vma ( aBfd, aSection );

    if ( lFindAddress->pc < vma )
    {
      return;
    }

    bfd_size_type lSize = bfd_section_size ( aBfd, aSection );

    if ( lFindAddress->pc >= vma + lSize )
    {
      return;
    }

    const char* lFile;

    const char* lFunction;

    uint32_t lLine;

    lFindAddress->found = bfd_find_nearest_line ( aBfd, aSection, lFindAddress->syms, lFindAddress->pc - vma, &lFile, &lFunction, &lLine );

    if ( !lFindAddress->found )
    {
      return;
    }

    if ( lFunction == NULL || *lFunction == '\0' )
    {
      lFindAddress->tracepoint.function = "??";
    }
    else
    {
      if ( !strncmp ( lFunction,"_Z",2 ) )
      {
        //We have mutex locked the top-level call, so make this static to prevent unnecessary memory allocations
        static int lStatus ( 0 );
        static std::size_t lSize ( 65536 );
        static char lDemangled[ 65536 ];
        abi::__cxa_demangle ( lFunction , lDemangled , &lSize , &lStatus );

        if ( lStatus )
        {
          lFindAddress->tracepoint.function = lFunction;
        }
        else
        {
          lFindAddress->tracepoint.function = lDemangled;
        }
      }
      else
      {
        lFindAddress->tracepoint.function = lFunction;
      }
    }

    if ( lFile == NULL || *lFile == '\0' )
    {
      lFindAddress->tracepoint.file = "??";
    }
    else
    {
      lFindAddress->tracepoint.file = lFile;
    }

    lFindAddress->tracepoint.line = lLine;
  }



  /* Process a file. */
  static bool ProcessFiles ( const char* aFilename, bfd_vma* aAddr, int aNaddr , TracePoint& aRet )
  {
    FindAddressHelperStruct lFindAddress;
    bfd* lBfd = bfd_openr ( aFilename, NULL );

    if ( lBfd == NULL )
    {
      throw 0;
    }

    if ( bfd_check_format ( lBfd, bfd_archive ) )
    {
      std::cout << aFilename << ": can not get addresses from archive" << std::endl;
      throw 0;
    }

    char** matching;

    if ( !bfd_check_format_matches ( lBfd, bfd_object, &matching ) )
    {
      throw 0;
    }

    if ( ( bfd_get_file_flags ( lBfd ) & HAS_SYMS ) == 0 )
    {
      return false;
    }

    uint32_t lSize;
    int32_t lSymCount = bfd_read_minisymbols ( lBfd, false, ( void** ) & lFindAddress.syms, &lSize );

    if ( lSymCount < 0 )
    {
      throw 0;
    }

    if ( lSymCount == 0 )
    {
      lSymCount = bfd_read_minisymbols ( lBfd, true /* dynamic */ , ( void** ) & lFindAddress.syms, &lSize );
    }

    while ( aNaddr )
    {
      lFindAddress.pc = aAddr[aNaddr-1];
      lFindAddress.found = false;
      bfd_map_over_sections ( lBfd, FindAddressInSection, &lFindAddress );

      if ( lFindAddress.found )
      {
        break;
      }

      aNaddr--;
    }

    if ( lFindAddress.syms )
    {
      free ( lFindAddress.syms );
      lFindAddress.syms = NULL;
    }

    bfd_close ( lBfd );
    aRet = lFindAddress.tracepoint;
    return true;
  }


  /* Callback used by dl_iterate_phdr */
  static int FindMatchingFiles ( struct dl_phdr_info* aInfo, size_t aSize, void* aData )
  {
    FindFilesHelperStruct* match = ( FindFilesHelperStruct* ) ( aData );
    /* This code is modeled from Gfind_proc_info-lsb.c:callback() from libunwind */
    const ElfW ( Phdr ) *lPhdr;
    ElfW ( Addr ) lLoadBase = aInfo->dlpi_addr;
    lPhdr = aInfo->dlpi_phdr;

    for ( uint32_t n = 0 ; n != aInfo->dlpi_phnum ; ++n , ++lPhdr )
    {
      if ( lPhdr->p_type == PT_LOAD )
      {
        ElfW ( Addr ) lVaddr = lPhdr->p_vaddr + lLoadBase;

        if ( match->address >= ( void* ) ( lVaddr ) && match->address < ( void* ) ( lVaddr + lPhdr->p_memsz ) )
        {
          /* we found a match */
          match->lFile = aInfo->dlpi_name;
          match->base = ( void* ) ( aInfo->dlpi_addr );
        }
      }
    }

    return 0;
  }


  void Backtrace ( std::vector< void* >& aBacktrace )
  {
    size_t lSize = backtrace ( & ( aBacktrace[0] ) , aBacktrace.size() );
    aBacktrace.resize ( lSize );
  }




  boost::mutex mBacktraceMutex;


  std::vector< TracePoint > BacktraceSymbols ( const std::vector< void* >& aBacktrace )
  {
    boost::lock_guard<boost::mutex> lLock ( mBacktraceMutex );
    std::vector< TracePoint > lRet;
    lRet.reserve ( aBacktrace.size() );
    bfd_init();

    for ( std::vector< void* >::const_iterator x=aBacktrace.begin(); x!=aBacktrace.end(); ++x )
    {
      struct FindFilesHelperStruct match;
      match.address = *x;
      dl_iterate_phdr ( FindMatchingFiles, &match );
      bfd_vma aAddr = ( std::size_t ) ( *x ) - ( std::size_t ) ( match.base );
      TracePoint lTracePoint;

      if ( ProcessFiles ( ( ( match.lFile && strlen ( match.lFile ) ) ?match.lFile:"/proc/self/exe" ) , &aAddr, 1 , lTracePoint ) )
      {
        lRet.push_back ( lTracePoint );
      }
    }

    return lRet;
  }

}

#endif
