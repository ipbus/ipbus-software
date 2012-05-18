
#include "uhal/pantheios.be.colour.hh"
#include "pantheios/init_codes.h"

#include <time.h>
#include <sys/time.h>

int pantheios_be_init (
	char const* processIdentity
	, void*       reserved
	, void**      ptoken
)
{
	*ptoken = strdup ( processIdentity );
	return ( NULL == *ptoken )
		   ? PANTHEIOS_INIT_RC_OUT_OF_MEMORY
		   : PANTHEIOS_INIT_RC_SUCCESS;
}


void pantheios_be_uninit (
	void* token
)
{
	free ( token );
}


int pantheios_be_logEntry (
	void*       feToken
	, void*       beToken
	, int         severity
	, char const* entry
	, size_t      cchEntry
)
{
	timeval theTime;
	gettimeofday ( &theTime, NULL );
	char lStr[200];
	int lSize = strftime ( lStr , 200 , "%Y/%m/%d %H:%M:%S" , localtime ( &theTime.tv_sec ) );

	switch ( severity )
	{
		case ( PANTHEIOS_SEV_DEBUG ) :
			fprintf ( stdout, "%s[%s %.*s.%lu %s] %.*s%s\n", "\033[1;34m" , ( char const* ) beToken , lSize , lStr , theTime.tv_usec , "DEBUG", ( int ) cchEntry, entry , "\033[0m" );
			//standard blue
			break;
		case ( PANTHEIOS_SEV_INFORMATIONAL ) :
			fprintf ( stdout, "%s[%s %.*s.%lu %s] %.*s%s\n", "\033[0;36m" , ( char const* ) beToken , lSize , lStr , theTime.tv_usec , "INFORMATIONAL", ( int ) cchEntry, entry , "\033[0m" );
			//standard cyan
			break;
		case ( PANTHEIOS_SEV_NOTICE ) :
			fprintf ( stdout, "%s[%s %.*s.%lu %s] %.*s%s\n", "\033[0;32m" , ( char const* ) beToken , lSize , lStr , theTime.tv_usec , "NOTICE", ( int ) cchEntry, entry , "\033[0m" );
			//standard green
			break;
		case ( PANTHEIOS_SEV_WARNING ) :
			fprintf ( stdout, "%s[%s %.*s.%lu %s] %.*s%s\n", "\033[0;33m" , ( char const* ) beToken , lSize , lStr , theTime.tv_usec , "WARNING", ( int ) cchEntry, entry , "\033[0m" );
			//standard yellow
			break;
		case ( PANTHEIOS_SEV_ERROR ) :
			fprintf ( stdout, "%s[%s %.*s.%lu %s] %.*s%s\n", "\033[0;31m" , ( char const* ) beToken , lSize , lStr , theTime.tv_usec , "ERROR", ( int ) cchEntry, entry , "\033[0m" );
			//standard red
			break;
		case ( PANTHEIOS_SEV_CRITICAL ) :
			fprintf ( stdout, "%s[%s %.*s.%lu %s] %.*s%s\n", "\033[0;31m" , ( char const* ) beToken , lSize , lStr , theTime.tv_usec , "CRITICAL", ( int ) cchEntry, entry , "\033[0m" );
			//standard red
			break;
		case ( PANTHEIOS_SEV_ALERT ) :
			fprintf ( stdout, "%s[%s %.*s.%lu %s] %.*s%s\n", "\033[0;31m" , ( char const* ) beToken , lSize , lStr , theTime.tv_usec , "ALERT", ( int ) cchEntry, entry , "\033[0m" );
			//standard red
			break;
		case ( PANTHEIOS_SEV_EMERGENCY ) :
			fprintf ( stdout, "%s[%s %.*s.%lu %s] %.*s%s\n", "\033[0;31m" , ( char const* ) beToken , lSize , lStr , theTime.tv_usec , "EMERGENCY", ( int ) cchEntry, entry , "\033[0m" );
			//standard red
			break;
	};

	return 0;
}

