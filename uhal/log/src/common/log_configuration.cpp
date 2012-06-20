
#include <uhal/log/log_configuration.hpp>
#include <uhal/log/log_inserters.hpp>

#include <sys/time.h>


namespace uhal
{

	std::vector< std::string > log_configuration::getLogLevels()
	{
		std::vector< std::string > lRet;
		lRet.push_back ( "Emergency" );
		lRet.push_back ( "Alert" );
		lRet.push_back ( "Critical" );
		lRet.push_back ( "Error" );
		lRet.push_back ( "Warning" );
		lRet.push_back ( "Notice" );
		lRet.push_back ( "Info" );
		lRet.push_back ( "Debug" );
		return lRet;
	}


	void log_configuration::setLogLevelTo ( const Emergency& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
		mLoggingIncludesEmergency = true;
		mLoggingIncludesAlert = false;
		mLoggingIncludesCritical = false;
		mLoggingIncludesError = false;
		mLoggingIncludesWarning = false;
		mLoggingIncludesNotice = false;
		mLoggingIncludesInfo = false;
		mLoggingIncludesDebug = false;
#endif
	}

	void log_configuration::setLogLevelTo ( const Alert& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
		mLoggingIncludesEmergency = true;
#ifndef LOGGING_EXCLUDE_ALERT
		mLoggingIncludesAlert = true;
		mLoggingIncludesCritical = false;
		mLoggingIncludesError = false;
		mLoggingIncludesWarning = false;
		mLoggingIncludesNotice = false;
		mLoggingIncludesInfo = false;
		mLoggingIncludesDebug = false;
#endif
#endif
	}

	void log_configuration::setLogLevelTo ( const Critical& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
		mLoggingIncludesEmergency = true;
#ifndef LOGGING_EXCLUDE_ALERT
		mLoggingIncludesAlert = true;
#ifndef LOGGING_EXCLUDE_CRITICAL
		mLoggingIncludesCritical = true;
		mLoggingIncludesError = false;
		mLoggingIncludesWarning = false;
		mLoggingIncludesNotice = false;
		mLoggingIncludesInfo = false;
		mLoggingIncludesDebug = false;
#endif
#endif
#endif
	}

	void log_configuration::setLogLevelTo ( const Error& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
		mLoggingIncludesEmergency = true;
#ifndef LOGGING_EXCLUDE_ALERT
		mLoggingIncludesAlert = true;
#ifndef LOGGING_EXCLUDE_CRITICAL
		mLoggingIncludesCritical = true;
#ifndef LOGGING_EXCLUDE_ERROR
		mLoggingIncludesError = true;
		mLoggingIncludesWarning = false;
		mLoggingIncludesNotice = false;
		mLoggingIncludesInfo = false;
		mLoggingIncludesDebug = false;
#endif
#endif
#endif
#endif
	}

	void log_configuration::setLogLevelTo ( const Warning& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
		mLoggingIncludesEmergency = true;
#ifndef LOGGING_EXCLUDE_ALERT
		mLoggingIncludesAlert = true;
#ifndef LOGGING_EXCLUDE_CRITICAL
		mLoggingIncludesCritical = true;
#ifndef LOGGING_EXCLUDE_ERROR
		mLoggingIncludesError = true;
#ifndef LOGGING_EXCLUDE_WARNING
		mLoggingIncludesWarning = true;
		mLoggingIncludesNotice = false;
		mLoggingIncludesInfo = false;
		mLoggingIncludesDebug = false;
#endif
#endif
#endif
#endif
#endif
	}

	void log_configuration::setLogLevelTo ( const Notice& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
		mLoggingIncludesEmergency = true;
#ifndef LOGGING_EXCLUDE_ALERT
		mLoggingIncludesAlert = true;
#ifndef LOGGING_EXCLUDE_CRITICAL
		mLoggingIncludesCritical = true;
#ifndef LOGGING_EXCLUDE_ERROR
		mLoggingIncludesError = true;
#ifndef LOGGING_EXCLUDE_WARNING
		mLoggingIncludesWarning = true;
#ifndef LOGGING_EXCLUDE_NOTICE
		mLoggingIncludesNotice = true;
		mLoggingIncludesInfo = false;
		mLoggingIncludesDebug = false;
#endif
#endif
#endif
#endif
#endif
#endif
	}

	void log_configuration::setLogLevelTo ( const Info& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
		mLoggingIncludesEmergency = true;
#ifndef LOGGING_EXCLUDE_ALERT
		mLoggingIncludesAlert = true;
#ifndef LOGGING_EXCLUDE_CRITICAL
		mLoggingIncludesCritical = true;
#ifndef LOGGING_EXCLUDE_ERROR
		mLoggingIncludesError = true;
#ifndef LOGGING_EXCLUDE_WARNING
		mLoggingIncludesWarning = true;
#ifndef LOGGING_EXCLUDE_NOTICE
		mLoggingIncludesNotice = true;
#ifndef LOGGING_EXCLUDE_INFO
		mLoggingIncludesInfo = true;
		mLoggingIncludesDebug = false;
#endif
#endif
#endif
#endif
#endif
#endif
#endif
	}

	void log_configuration::setLogLevelTo ( const Debug& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
		mLoggingIncludesEmergency = true;
#ifndef LOGGING_EXCLUDE_ALERT
		mLoggingIncludesAlert = true;
#ifndef LOGGING_EXCLUDE_CRITICAL
		mLoggingIncludesCritical = true;
#ifndef LOGGING_EXCLUDE_ERROR
		mLoggingIncludesError = true;
#ifndef LOGGING_EXCLUDE_WARNING
		mLoggingIncludesWarning = true;
#ifndef LOGGING_EXCLUDE_NOTICE
		mLoggingIncludesNotice = true;
#ifndef LOGGING_EXCLUDE_INFO
		mLoggingIncludesInfo = true;
#ifndef LOGGING_EXCLUDE_DEBUG
		mLoggingIncludesDebug = true;
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
	}

	const bool& log_configuration::LoggingIncludes ( const Emergency& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
		return mLoggingIncludesEmergency;
#endif
		return mFalse;
	}

	const bool& log_configuration::LoggingIncludes ( const Alert& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
#ifndef LOGGING_EXCLUDE_ALERT
		return mLoggingIncludesAlert;
#endif
#endif
		return mFalse;
	}

	const bool& log_configuration::LoggingIncludes ( const Critical& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
#ifndef LOGGING_EXCLUDE_ALERT
#ifndef LOGGING_EXCLUDE_CRITICAL
		return mLoggingIncludesCritical;
#endif
#endif
#endif
		return mFalse;
	}

	const bool& log_configuration::LoggingIncludes ( const Error& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
#ifndef LOGGING_EXCLUDE_ALERT
#ifndef LOGGING_EXCLUDE_CRITICAL
#ifndef LOGGING_EXCLUDE_ERROR
		return mLoggingIncludesError;
#endif
#endif
#endif
#endif
		return mFalse;
	}

	const bool& log_configuration::LoggingIncludes ( const Warning& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
#ifndef LOGGING_EXCLUDE_ALERT
#ifndef LOGGING_EXCLUDE_CRITICAL
#ifndef LOGGING_EXCLUDE_ERROR
#ifndef LOGGING_EXCLUDE_WARNING
		return mLoggingIncludesWarning;
#endif
#endif
#endif
#endif
#endif
		return mFalse;
	}

	const bool& log_configuration::LoggingIncludes ( const Notice& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
#ifndef LOGGING_EXCLUDE_ALERT
#ifndef LOGGING_EXCLUDE_CRITICAL
#ifndef LOGGING_EXCLUDE_ERROR
#ifndef LOGGING_EXCLUDE_WARNING
#ifndef LOGGING_EXCLUDE_NOTICE
		return mLoggingIncludesNotice;
#endif
#endif
#endif
#endif
#endif
#endif
		return mFalse;
	}

	const bool& log_configuration::LoggingIncludes ( const Info& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
#ifndef LOGGING_EXCLUDE_ALERT
#ifndef LOGGING_EXCLUDE_CRITICAL
#ifndef LOGGING_EXCLUDE_ERROR
#ifndef LOGGING_EXCLUDE_WARNING
#ifndef LOGGING_EXCLUDE_NOTICE
#ifndef LOGGING_EXCLUDE_INFO
		return mLoggingIncludesInfo;
#endif
#endif
#endif
#endif
#endif
#endif
#endif
		return mFalse;
	}

	const bool& log_configuration::LoggingIncludes ( const Debug& )
	{
#ifndef LOGGING_EXCLUDE_EMERGENCY
#ifndef LOGGING_EXCLUDE_ALERT
#ifndef LOGGING_EXCLUDE_CRITICAL
#ifndef LOGGING_EXCLUDE_ERROR
#ifndef LOGGING_EXCLUDE_WARNING
#ifndef LOGGING_EXCLUDE_NOTICE
#ifndef LOGGING_EXCLUDE_INFO
#ifndef LOGGING_EXCLUDE_DEBUG
		return mLoggingIncludesDebug;
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
		return mFalse;
	}


	FILE* log_configuration::getDestination()
	{
		return mDestination;
	}



	void log_configuration::log_head ( const Emergency& )
	{
		fputs ( "\033[0;31m[" , mDestination ); //standard red
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		fputs ( " EMERGENCY] " , mDestination );
	}

	void log_configuration::log_head ( const Alert& )
	{
		fputs ( "\033[0;31m[" , mDestination ); //standard red
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		fputs ( " ALERT] " , mDestination );
	}

	void log_configuration::log_head ( const Critical& )
	{
		fputs ( "\033[0;31m[" , mDestination ); //standard red
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		fputs ( " CRITICAL] " , mDestination );
	}

	void log_configuration::log_head ( const Error& )
	{
		fputs ( "\033[0;31m[" , mDestination ); //standard red
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		fputs ( " ERROR] " , mDestination );
	}

	void log_configuration::log_head ( const Warning& )
	{
		fputs ( "\033[0;33m[" , mDestination ); //standard yellow
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		fputs ( " WARNING] " , mDestination );
	}

	void log_configuration::log_head ( const Notice& )
	{
		fputs ( "\033[0;32m[" , mDestination ); //standard green
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		fputs ( " NOTICE] " , mDestination );
	}

	void log_configuration::log_head ( const Info& )
	{
		fputs ( "\033[0;36m[" , mDestination ); //standard cyan
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		fputs ( " INFO] " , mDestination );
	}

	void log_configuration::log_head ( const Debug& )
	{
		fputs ( "\033[1;34m[" , mDestination ); //standard blue
		timeval lTime;
		gettimeofday ( &lTime, NULL );
		log_inserter ( Time ( lTime , TimeFmt< day,'/',mth,'/',yr,' ',hr,':',min,':',sec,'.',usec >() ) );
		fputs ( " DEBUG] " , mDestination );
	}



	void log_configuration::log_tail ( const Emergency& )
	{
		fputc ( '\n' , mDestination );
	}

	void log_configuration::log_tail ( const Alert& )
	{
		fputc ( '\n' , mDestination );
	}

	void log_configuration::log_tail ( const Critical& )
	{
		fputc ( '\n' , mDestination );
	}

	void log_configuration::log_tail ( const Error& )
	{
		fputc ( '\n' , mDestination );
	}

	void log_configuration::log_tail ( const Warning& )
	{
		fputc ( '\n' , mDestination );
	}

	void log_configuration::log_tail ( const Notice& )
	{
		fputc ( '\n' , mDestination );
	}

	void log_configuration::log_tail ( const Info& )
	{
		fputc ( '\n' , mDestination );
	}

	void log_configuration::log_tail ( const Debug& )
	{
		fputc ( '\n' , mDestination );
	}

	bool log_configuration::mLoggingIncludesEmergency = true;
	bool log_configuration::mLoggingIncludesAlert = true;
	bool log_configuration::mLoggingIncludesCritical = true;
	bool log_configuration::mLoggingIncludesError = true;
	bool log_configuration::mLoggingIncludesWarning = true;
	bool log_configuration::mLoggingIncludesNotice = true;
	bool log_configuration::mLoggingIncludesInfo = true;
	bool log_configuration::mLoggingIncludesDebug = true;

	const bool log_configuration::mTrue = true;
	const bool log_configuration::mFalse = false;

	FILE* log_configuration::mDestination = stdout;




}

