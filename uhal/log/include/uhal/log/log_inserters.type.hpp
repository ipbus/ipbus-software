
#ifndef _log_inserters_type_hpp_
#define _log_inserters_type_hpp_

#include <uhal/log/log_inserter_helper.hpp>

#include <stdint.h>

namespace uhal
{

  template< typename T > class _Type;

  template< typename T > _Type< T > Type ( );
  template< typename T > _Type< T > Type ( const T& aT );

  template< typename T >
  class _Type
  {
      friend _Type< T > Type<> ();
      friend _Type< T > Type<> ( const T& aT );
      _Type ( ) {}
  };




}

#include <uhal/log/log_inserters.type.hxx>

#endif
