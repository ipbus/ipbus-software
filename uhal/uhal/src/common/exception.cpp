/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#include "uhal/exception.hpp"

#ifdef __GNUG__
#include <cxxabi.h>
#endif

namespace uhal
{

	exception::exception ( const std::string& aExc ) :
		std::exception () ,
		mMessage ( aExc )
	{}

	exception::~exception() throw() {}

	const char* exception::what() const throw()
	{
		if ( mMessage.size() )
		{
			return mMessage.c_str();
		}

#ifdef __GNUG__
		// this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
		int lStatus ( 0 );
		return abi::__cxa_demangle ( typeid ( *this ).name() , 0 , 0 , &lStatus );
#else
		return typeid ( *this ).name();
#endif
	}


	StdException::StdException ( const std::exception& aExc ) :
		uhal::_exception< StdException > ( aExc.what() )
	{}

}
