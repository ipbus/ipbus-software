
#include <uhal/log/log.hpp>

#include <uhal/log/log_backend.colour_terminal.hpp>

namespace uhal
{

	template<>
	template<>
	void log_formatter< Emergency >::template_specialization_helper< Emergency >::print ( )
	{
		put ( "\033[0;31m[" ); //standard red
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		put ( " EMERGENCY] " );
	}

	template<>
	template<>
	void log_formatter< Alert >::template_specialization_helper< Alert >::print ( )
	{
		put ( "\033[0;31m[" ); //standard red
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		put ( " ALERT] " );
	}

	template<>
	template<>
	void log_formatter< Critical >::template_specialization_helper< Critical >::print ( )
	{
		put ( "\033[0;31m[" ); //standard red
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		put ( " CRITICAL] " );
	}

	template<>
	template<>
	void log_formatter< Error >::template_specialization_helper< Error >::print ( )
	{
		put ( "\033[0;31m[" ); //standard red
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		put ( " ERROR] " );
	}

	template<>
	template<>
	void log_formatter< Warning >::template_specialization_helper< Warning >::print ( )
	{
		put ( "\033[0;33m[" ); //standard yellow
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		put ( " WARNING] " );
	}

	template<>
	template<>
	void log_formatter< Notice >::template_specialization_helper< Notice >::print ( )
	{
		put ( "\033[0;32m[" ); //standard green
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		put ( " NOTICE] " );
	}

	template<>
	template<>
	void log_formatter< Info >::template_specialization_helper< Info >::print ( )
	{
		put ( "\033[0;36m[" ); //standard cyan
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		put ( " INFO] " );
	}

	template<>
	template<>
	void log_formatter< Debug >::template_specialization_helper< Debug >::print ( )
	{
		put ( "\033[1;34m[" ); //standard blue
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		put ( " DEBUG] " );
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

