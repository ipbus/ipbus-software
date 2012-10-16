
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

