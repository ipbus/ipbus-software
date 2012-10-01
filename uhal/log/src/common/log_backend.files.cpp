
#include <uhal/log/log.hpp>

#include <uhal/log/log_inserters.time.hpp>
#include <uhal/log/log_backend.hpp>

#include <uhal/log/log_backend.files.hpp>

#include <sstream>
#include <time.h>

namespace uhal
{


	log_file::log_file(){}

	log_file::~log_file(){}


	log_file::log_file_helper::log_file_helper(){
		std::stringstream lFilename;

		char buffer[80];
		time_t rawtime;
		time ( &rawtime );
		strftime (buffer,80,"%Y_%m_%d_%H_%M_%S",localtime ( &rawtime ));


		lFilename << "log_" << buffer << "_thread_" << boost::this_thread::get_id() << ".txt";


		mFile = fopen ( lFilename.str().c_str() , "w" );
	}

	log_file::log_file_helper::~log_file_helper(){
		fclose( mFile );
	}

	FILE* log_file::get()
	{
		log_file::log_file_helper* lPtr( mLogFileHelper.get() );

		if( !lPtr )
		{
			mLogFileHelper.reset( new log_file_helper() );
			lPtr = mLogFileHelper.get();
		}

		return lPtr->mFile;
	}
	
	boost::thread_specific_ptr< log_file::log_file_helper > log_file::mLogFileHelper;


/*
  template<>
  void log_head_template_specialization_helper< Fatal >::print ( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " CRITICAL] " );
  }

  template<>
  void log_head_template_specialization_helper< Error >::print ( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " ERROR] " );
  }

  template<>
  void log_head_template_specialization_helper< Warning >::print ( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " WARNING] " );
  }

  template<>
  void log_head_template_specialization_helper< Notice >::print ( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " NOTICE] " );
  }

  template<>
  void log_head_template_specialization_helper< Info >::print ( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " INFO] " );
  }

  template<>
  void log_head_template_specialization_helper< Debug >::print ( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " DEBUG] " );
  }
*/

  
  void log_head_Fatal( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " CRITICAL] " );
  }

  
  void log_head_Error( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " ERROR] " );
  }

  
  void log_head_Warning( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " WARNING] " );
  }

  
  void log_head_Notice( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " NOTICE] " );
  }

  
  void log_head_Info( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " INFO] " );
  }

  
  void log_head_Debug( )
  {
    put ( '[' ); 
    log_inserter ( Time ( Now() , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
    put ( " DEBUG] " );
  }


  void log_tail_Fatal( )
  {}

  void log_tail_Error( )
  {}

  void log_tail_Warning( )
  {}

  void log_tail_Notice( )
  {}

  void log_tail_Info( )
  {}

  void log_tail_Debug( )
  {}



  void put ( const char& aChar )
  {
    fputc ( aChar , log_file::get() );
  }

  void put ( const char* aStr )
  {
    fputs ( aStr , log_file::get() );
  }

  void put ( const char* aStart , const uint32_t& aSize )
  {
    fwrite ( aStart , 1 , aSize , log_file::get() );
  }

}

