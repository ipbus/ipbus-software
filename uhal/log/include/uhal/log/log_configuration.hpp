
#ifndef _log_configuration_hpp_
#define _log_configuration_hpp_

#include <vector>
#include <string>
#include <stdio.h>

namespace uhal
{

	struct Emergency {};
	struct Alert {};
	struct Critical {};
	struct Error {};
	struct Warning {};
	struct Notice {};
	struct Info {};
	struct Debug {};

	class log_configuration
	{
		public:


			static std::vector< std::string > getLogLevels();

			static void setLogLevelTo ( const Emergency& );
			static void setLogLevelTo ( const Alert& );
			static void setLogLevelTo ( const Critical& );
			static void setLogLevelTo ( const Error& );
			static void setLogLevelTo ( const Warning& );
			static void setLogLevelTo ( const Notice& );
			static void setLogLevelTo ( const Info& );
			static void setLogLevelTo ( const Debug& );

			static const bool& LoggingIncludes ( const Emergency& );
			static const bool& LoggingIncludes ( const Alert& );
			static const bool& LoggingIncludes ( const Critical& );
			static const bool& LoggingIncludes ( const Error& );
			static const bool& LoggingIncludes ( const Warning& );
			static const bool& LoggingIncludes ( const Notice& );
			static const bool& LoggingIncludes ( const Info& );
			static const bool& LoggingIncludes ( const Debug& );

			static FILE* getDestination();

			static void log_head ( const Emergency& );
			static void log_head ( const Alert& );
			static void log_head ( const Critical& );
			static void log_head ( const Error& );
			static void log_head ( const Warning& );
			static void log_head ( const Notice& );
			static void log_head ( const Info& );
			static void log_head ( const Debug& );

			static void log_tail ( const Emergency& );
			static void log_tail ( const Alert& );
			static void log_tail ( const Critical& );
			static void log_tail ( const Error& );
			static void log_tail ( const Warning& );
			static void log_tail ( const Notice& );
			static void log_tail ( const Info& );
			static void log_tail ( const Debug& );

		private:

			static bool mLoggingIncludesEmergency;
			static bool mLoggingIncludesAlert;
			static bool mLoggingIncludesCritical;
			static bool mLoggingIncludesError;
			static bool mLoggingIncludesWarning;
			static bool mLoggingIncludesNotice;
			static bool mLoggingIncludesInfo;
			static bool mLoggingIncludesDebug;

			static const bool mTrue;
			static const bool mFalse;

			static FILE* mDestination;

			static const char* mCharacterMapping;
	};

}

#endif
