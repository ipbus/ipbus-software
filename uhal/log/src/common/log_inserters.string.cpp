
#include <uhal/log/log_inserters.string.hpp>

#include <uhal/log/log_configuration.hpp>

namespace uhal
{

	template<>
	void log_inserter< char > ( const char& aChar )
	{
		fputc ( aChar , log_configuration::getDestination() );
	}

	void log_inserter ( const char* aStr )
	{
		fputs ( aStr , log_configuration::getDestination() );
	}

	template<>
	void log_inserter< std::string > ( const std::string& aStr )
	{
		fwrite ( aStr.data() , 1 , aStr.size() , log_configuration::getDestination() );
	}

}
