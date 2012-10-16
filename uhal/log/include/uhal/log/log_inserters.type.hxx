
#include <uhal/log/log.hpp>

#ifdef __GNUG__
#include <cxxabi.h>
#endif

namespace uhal
{

  template< typename T >
  void log_inserter ( const _Type< T >& aType )
  {
#ifdef __GNUG__
    // this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
    int lStatus ( 0 );
    put ( abi::__cxa_demangle ( typeid ( T ).name() , 0 , 0 , &lStatus ) );
#else
    put ( typeid ( T ).name() );
#endif
  }

  template< typename T >
  _Type< T > Type ( )
  {
    return _Type< T > ( );
  }

  template< typename T >
  _Type< T > Type ( const T& aT )
  {
    return _Type< T > ( );
  }

}

