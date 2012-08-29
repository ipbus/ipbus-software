
#include <uhal/log/log_inserters.boolean.hpp>

#include <uhal/log/log.hpp>

namespace uhal
{

  template<>
  void log_inserter< _Boolean< bool , BoolFmt<alpha> > > ( const _Boolean< bool , BoolFmt<alpha> >& aBoolean )
  {
    if ( aBoolean.value() )
    {
      put ( "True" );
    }
    else
    {
      put ( "False" );
    }
  }


  template<>
  void log_inserter< _Boolean< bool , BoolFmt<numeric> > > ( const _Boolean< bool , BoolFmt<numeric> >& aBoolean )
  {
    if ( aBoolean.value() )
    {
      put ( '1' );
    }
    else
    {
      put ( '0' );
    }
  }



}
