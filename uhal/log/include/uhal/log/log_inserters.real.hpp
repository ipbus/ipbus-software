
#ifndef _log_inserters_real_hpp_
#define _log_inserters_real_hpp_

#include <uhal/log/log_inserter_helper.hpp>

#include <stdint.h>

namespace uhal
{

  static const uint32_t DefaultRealWidth ( 10 );

  template< typename T , typename FORMAT > struct RealFactory;

  template< uint32_t WIDTH = DefaultRealWidth > struct RealFmt {};

  template< typename T , typename FORMAT >
  class _Real : public RefWrapper< T >
  {
      friend class RealFactory< T , FORMAT >;
      _Real ( const T& aReal ) : RefWrapper< T > ( aReal ) {}
  };

  template< typename T > _Real< T , RealFmt<> > Real ( const T& aT );
  template< typename T , typename FORMAT > _Real< T , FORMAT > Real ( const T& aT , const FORMAT& aFmt );

}

#include <uhal/log/log_inserters.real.hxx>

#endif
