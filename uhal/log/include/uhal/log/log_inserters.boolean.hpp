
#ifndef _log_inserters_boolean_hpp_
#define _log_inserters_boolean_hpp_

#include <uhal/log/log_inserter_helper.hpp>

#include <string>

namespace uhal
{

	enum boolean_format
	{
		alpha,
		numeric
	};

	static const boolean_format DefaultBooleanFormat ( alpha );

	template< typename T , typename FORMAT > struct BooleanFactory;

	template< boolean_format FORMAT = DefaultBooleanFormat > struct BoolFmt {};

	template< typename T , typename FORMAT >
	class _Boolean : public RefWrapper< T >
	{
			friend class BooleanFactory< T , FORMAT >;
			_Boolean ( const T& aT ) : RefWrapper< T > ( aT ) {}
	};


	template< typename T > _Boolean< T , BoolFmt<> > Boolean ( const T& aT );

	template< typename T , typename FORMAT > _Boolean< T , FORMAT > Boolean ( const T& aT , const FORMAT& aFmt );

}

#include <uhal/log/log_inserters.boolean.hxx>

#endif
