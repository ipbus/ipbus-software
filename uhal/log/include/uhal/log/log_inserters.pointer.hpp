
#ifndef _log_inserters_pointer_hpp_
#define _log_inserters_pointer_hpp_

#include <uhal/log/log_inserter_helper.hpp>

#include <stdint.h>

namespace uhal
{

	template< typename T > class _Pointer;

	template< typename T > _Pointer< T > Pointer ( const T* aT );

	template< typename T >
	class _Pointer : public RefWrapper< T* >
	{
			friend _Pointer< T > Pointer<> ( const T* aT );
			_Pointer ( const T* aT ) : RefWrapper< T* > ( aT ) {}
	};




}

#include <uhal/log/log_inserters.pointer.hxx>

#endif
