#ifndef _uhal_exception_hpp_
#define _uhal_exception_hpp_

#include <exception>
#include <string>

#ifdef __GNUG__
#include <cxxabi.h>
#endif

namespace uhal
{

	class exception : public std::exception
	{
		public:
			exception() :
				std::exception() ,
				mMessage ( "" )
			{}

			exception ( const std::exception& aExc ) :
				std::exception ( aExc ) ,
				mMessage ( aExc.what() )
			{}

			virtual ~exception() throw() {}

			virtual const char* what() const throw()
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
		private:

			std::string mMessage;

	};

}


#endif
