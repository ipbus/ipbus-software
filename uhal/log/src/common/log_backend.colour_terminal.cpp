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


#include <uhal/log/log_backend.hpp>

#include <uhal/log/log_inserters.time.hpp>
#include <uhal/log/log_inserters.threadID.hpp>

namespace uhal
{


  namespace colour_terminal
  {
    void log_header ( const char* aStartHeader , const char* aEndHeader )
    {
      put ( aStartHeader );
      log_inserter ( Time ( Now() , TimeFmt< day,'-',mth,'-',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
      put ( " [" );
      log_insert_threadID();
      put ( "] " );
      put ( aEndHeader );
    }

    void log_tail()
    {
      put ( "\033[0m\n" );
    }
  }


  void log_head_Fatal( )
  {
    colour_terminal::log_header ( "\033[0;31m" , "FATAL - " ); //standard red
  }


  void log_head_Error( )
  {
    colour_terminal::log_header ( "\033[0;31m" , "ERROR - " ); //standard red
  }


  void log_head_Warning( )
  {
    colour_terminal::log_header ( "\033[0;33m" , "WARNING - " ); //standard yellow
  }


  void log_head_Notice( )
  {
    colour_terminal::log_header ( "\033[0;32m" , "NOTICE - " ); //standard green
  }


  void log_head_Info( )
  {
    colour_terminal::log_header ( "\033[0;36m" , "INFO - " ); //standard cyan
  }


  void log_head_Debug( )
  {
    colour_terminal::log_header ( "\033[1;34m" , "DEBUG - " ); //standard blue
  }


  void log_tail_Fatal( )
  {
    colour_terminal::log_tail();
  }

  void log_tail_Error( )
  {
    colour_terminal::log_tail();
  }

  void log_tail_Warning( )
  {
    colour_terminal::log_tail();
  }

  void log_tail_Notice( )
  {
    colour_terminal::log_tail();
  }

  void log_tail_Info( )
  {
    colour_terminal::log_tail();
  }

  void log_tail_Debug( )
  {
    colour_terminal::log_tail();
  }



  void put ( const char& aChar )
  {
    fputc ( aChar , stdout );
  }

  void put ( const char* aStr )
  {
    fputs ( aStr , stdout );
  }

  void put ( const char* aStart , const uint32_t& aSize )
  {
    fwrite ( aStart , 1 , aSize , stdout );
  }

}

