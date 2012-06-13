
#ifndef _log_inserters_boolean_hpp_
#define _log_inserters_boolean_hpp_

#include <log/log_inserter_helper.hpp> 

#include <string>

namespace uhal{

	enum boolean_format{
		alpha,
		numeric
	};

	static const boolean_format DefaultBooleanFormat( alpha );


	template< boolean_format FORMAT = numeric >
	struct _Boolean : public RefWrapper< bool >
	{	
		_Boolean( const bool& aBoolean ) : RefWrapper< bool >( aBoolean ){}
	};


	_Boolean< DefaultBooleanFormat > Boolean( const bool& aBool );

	template< boolean_format FORMAT >
	_Boolean< FORMAT > Boolean( const bool& aBool );

}

#include <log/log_inserters.boolean.hxx>

#endif
