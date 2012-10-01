
#include <uhal/log/log_backend.hpp>

#include <uhal/log/log_inserters.time.hpp>
#include <uhal/log/log_inserters.threadID.hpp>

namespace uhal
{

  
  void log_head_Fatal( )
  {
    put ( "\033[0;31m[" ); //standard red
	log_insert_threadID();	
	put ( ' ' );
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " CRITICAL] " );
  }

  
  void log_head_Error( )
  {
    put ( "\033[0;31m[" ); //standard red
	log_insert_threadID();	
	put ( ' ' );
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " ERROR] " );
  }

  
  void log_head_Warning( )
  {
    put ( "\033[0;33m[" ); //standard yellow
	log_insert_threadID();	
	put ( ' ' );
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " WARNING] " );
  }

  
  void log_head_Notice( )
  {
    put ( "\033[0;32m[" ); //standard green
	log_insert_threadID();	
	put ( ' ' );
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " NOTICE] " );
  }

  
  void log_head_Info( )
  {
    put ( "\033[0;36m[" ); //standard cyan
	log_insert_threadID();	
	put ( ' ' );
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " INFO] " );
  }

  
  void log_head_Debug( )
  {
    put ( "\033[1;34m[" ); //standard blue
	log_insert_threadID();	
	put ( ' ' );
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " DEBUG] " );
  }


  void log_tail_Fatal( )
  {
    put ( "\033[0m\n" );
  }

  void log_tail_Error( )
  {
    put ( "\033[0m\n" );
  }

  void log_tail_Warning( )
  {
    put ( "\033[0m\n" );
  }

  void log_tail_Notice( )
  {
    put ( "\033[0m\n" );
  }

  void log_tail_Info( )
  {
    put ( "\033[0m\n" );
  }

  void log_tail_Debug( )
  {
    put ( "\033[0m\n" );
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

