#include "uhal/log.hpp"

extern "C" const char PANTHEIOS_FE_PROCESS_IDENTITY[] = "uHAL";

namespace uhal
{
	Location::Location ( const char* aFunction , const char* aFile , const uint32_t& aLine ) :
		mFunction ( aFunction ),
		mFile ( aFile ) ,
		mLine ( aLine )
	{}
}

std::ostream& operator<< ( std::ostream& aStream , const uhal::Location& aLocation )
{
	aStream << "function \""  << aLocation.mFunction << "\" in " << aLocation.mFile << ", line " << aLocation.mLine << ".";
	return aStream;
}
