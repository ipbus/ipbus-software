
#include <uhal/log/log.hpp>

#include <stdlib.h>

namespace uhal
{

  template< typename T , uint32_t WIDTH >
  void log_inserter ( const _Real< T , RealFmt<WIDTH> >& aReal )
  {
    char lBuffer[ WIDTH+10 ]; // Make the buffer plenty wide enough, since the exponent is not counted in the width term!
    // An IEEE double has an 11=bit exponent = -1023 to 1024, adding the "e" character means that the
    // maximum size of the exponent is 6 characters. 10 is a nice round number and is on the safe side.
    gcvt ( aReal.value() , WIDTH , lBuffer );
    put ( lBuffer );
  }

  template< typename FORMAT >
  struct RealFactory < double , FORMAT >
  {
    static _Real< double , FORMAT > Construct ( const double& aReal )
    {
      return _Real< double , FORMAT > ( aReal );
    }
  };

  template< typename FORMAT >
  struct RealFactory < float , FORMAT >
  {
    static _Real< float , FORMAT > Construct ( const float& aReal )
    {
      return _Real< float , FORMAT > ( aReal );
    }
  };


  template< typename T >
  _Real< T , RealFmt<> > Real ( const T& aT )
  {
    return RealFactory< T , RealFmt<> >::Construct ( aT );
  }


  template< typename T , typename FORMAT >
  _Real< T , FORMAT > Real ( const T& aT , const FORMAT& aFmt )
  {
    return RealFactory< T , FORMAT >::Construct ( aT );
  }

}

