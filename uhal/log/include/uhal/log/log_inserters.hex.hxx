
#include <uhal/log/log.hpp>

#include <uhal/log/log_inserters.type.hpp>

namespace uhal
{

  template< typename T >
  void log_inserter ( const _Hex< T >& aHex )
  {
    put ( "[ " );
    log_inserter ( Type< T >() );
    put ( " ] 0x" );
    static const char* lCharacterMapping ( "0123456789ABCDEF" );
    uint8_t* lStart ( ( uint8_t* ) ( & aHex.value() ) );
    uint8_t* lPtr ( lStart + sizeof ( T ) );

    do
    {
      --lPtr;
      put ( * ( lCharacterMapping + ( ( ( *lPtr ) &0xF0 ) >>4 ) ) );
      put ( * ( lCharacterMapping + ( ( ( *lPtr ) &0x0F ) ) ) );
    }
    while ( lPtr!=lStart );
  }


  template< typename T >
  _Hex< T > Hex ( const T& aT )
  {
    return _Hex< T > ( aT );
  }

}

