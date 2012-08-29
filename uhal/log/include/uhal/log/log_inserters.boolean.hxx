
#include <uhal/log/log.hpp>

namespace uhal
{

  template< typename FORMAT >
  struct BooleanFactory < bool , FORMAT >
  {
    static _Boolean< bool , FORMAT > Construct ( const bool& aBool )
    {
      return _Boolean< bool , FORMAT > ( aBool );
    }
  };


  template< typename T >
  _Boolean< T , BoolFmt<> > Boolean ( const T& aT )
  {
    return BooleanFactory< T , BoolFmt<> >::Construct ( aT );
  }


  template< typename T , typename FORMAT >
  _Boolean< T , FORMAT > Boolean ( const T& aT , const FORMAT& aFmt )
  {
    return BooleanFactory< T , FORMAT >::Construct ( aT );
  }



}

