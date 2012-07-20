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
	void _exception<Derived>::throwFrom_ ( const Location& aLocation )
	{
		log ( Error() , "Exception " , Quote ( this->what() ) , " thrown at " , aLocation );
		throw static_cast< Derived& > ( *this );
	}

	template < class Derived >
	void _exception<Derived>::rethrowFrom_ ( const Location& aLocation )
	{
		log ( Error() , "Rethrown at " , aLocation );
		throw static_cast< Derived& > ( *this );
	}

}
