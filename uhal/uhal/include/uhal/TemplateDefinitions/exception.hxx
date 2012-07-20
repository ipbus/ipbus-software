/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

namespace uhal
{
	template < class Derived >
	_exception<Derived>::_exception ( const std::string& aExc ) :
		exception ( aExc )
	{}

	template < class Derived >
	_exception<Derived>::~_exception() throw() {}

	template < class Derived >
	void _exception<Derived>::throwFrom ( const Location& aLocation )
	{
		log ( Error() , "Thrown at " , aLocation );
		throw static_cast< Derived& > ( *this );
	}

	template < class Derived >
	void _exception<Derived>::rethrowFrom ( const Location& aLocation )
	{
		log ( Error() , "Rethrown at " , aLocation );
		throw static_cast< Derived& > ( *this );
	}

}
