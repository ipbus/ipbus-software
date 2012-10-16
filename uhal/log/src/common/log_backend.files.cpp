#include "boost/thread.hpp"
#include <uhal/log/log.hpp>

#include <uhal/log/log_inserters.time.hpp>
#include <uhal/log/log_backend.hpp>

#include <uhal/log/log_backend.files.hpp>

#include <sstream>
#include <time.h>

namespace uhal
{
  namespace log_files
  {

    log_file::log_file() {}

    log_file::~log_file() {}


    log_file::log_file_helper::log_file_helper()
    {
      std::stringstream lFilename;
      char buffer[80];
      time_t rawtime;
      time ( &rawtime );
      strftime ( buffer,80,"%Y_%m_%d_%H_%M_%S",localtime ( &rawtime ) );
      lFilename << "log_" << buffer << "_thread_" << boost::this_thread::get_id() << ".txt";
      mFile = fopen ( lFilename.str().c_str() , "w" );
    }

    log_file::log_file_helper::~log_file_helper()
    {
      fclose ( mFile );
    }

    FILE* log_file::get()
    {
      log_file::log_file_helper* lPtr ( mLogFileHelper.get() );

      if ( !lPtr )
      {
        mLogFileHelper.reset ( new log_file_helper() );
        lPtr = mLogFileHelper.get();
      }

      return lPtr->mFile;
    }

    boost::thread_specific_ptr< log_file::log_file_helper > log_file::mLogFileHelper;



    void log_header ( const char* aEndHeader )
    {
      log_inserter ( Time ( Now() , TimeFmt< day,'-',mth,'-',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
      put ( ' ' );
      put ( aEndHeader );
    }

  }




  void log_head_Fatal( )
  {
    log_files::log_header ( "FATAL - " ); //standard red
  }


  void log_head_Error( )
  {
    log_files::log_header ( "ERROR - " ); //standard red
  }


  void log_head_Warning( )
  {
    log_files::log_header ( "WARNING - " ); //standard yellow
  }


  void log_head_Notice( )
  {
    log_files::log_header ( "NOTICE - " ); //standard green
  }


  void log_head_Info( )
  {
    log_files::log_header ( "INFO - " ); //standard cyan
  }


  void log_head_Debug( )
  {
    log_files::log_header ( "DEBUG - " ); //standard blue
  }




  void log_tail_Fatal( )
  {
    put ( '\n' );
  }

  void log_tail_Error( )
  {
    put ( '\n' );
  }

  void log_tail_Warning( )
  {
    put ( '\n' );
  }

  void log_tail_Notice( )
  {
    put ( '\n' );
  }

  void log_tail_Info( )
  {
    put ( '\n' );
  }

  void log_tail_Debug( )
  {
    put ( '\n' );
  }



  void put ( const char& aChar )
  {
    fputc ( aChar , log_files::log_file::get() );
  }

  void put ( const char* aStr )
  {
    fputs ( aStr , log_files::log_file::get() );
  }

  void put ( const char* aStart , const uint32_t& aSize )
  {
    fwrite ( aStart , 1 , aSize , log_files::log_file::get() );
  }

}

