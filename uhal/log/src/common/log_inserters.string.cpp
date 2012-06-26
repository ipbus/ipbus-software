
#include <uhal/log/log_inserters.string.hpp>

#include <uhal/log/log.hpp>

namespace uhal
{

	template<>
	void log_inserter< char > ( const char& aChar )
	{
		put ( aChar );
	}

	void log_inserter ( const char* aStr )
	{
		put ( aStr );
	}

	template<>
	void log_inserter< std::string > ( const std::string& aStr )
	{
		put ( aStr.data() , aStr.size() );
	}

}
