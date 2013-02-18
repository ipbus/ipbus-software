/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.


      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

---------------------------------------------------------------------------
*/


#include <uhal/log/log.hpp>

namespace uhal
{


  template< typename FORMAT >
  struct IntegerFactory < uint8_t , FORMAT >
  {
    static _Integer< uint8_t , FORMAT > Construct ( const uint8_t& aInt )
    {
      return _Integer< uint8_t , FORMAT > ( aInt );
    }
  };

  template< typename FORMAT >
  struct IntegerFactory < int8_t , FORMAT >
  {
    static _Integer< int8_t , FORMAT > Construct ( const int8_t& aInt )
    {
      return _Integer< int8_t , FORMAT > ( aInt );
    }
  };

  template< typename FORMAT >
  struct IntegerFactory < uint16_t , FORMAT >
  {
    static _Integer< uint16_t , FORMAT > Construct ( const uint16_t& aInt )
    {
      return _Integer< uint16_t , FORMAT > ( aInt );
    }
  };

  template< typename FORMAT >
  struct IntegerFactory < int16_t , FORMAT >
  {
    static _Integer< int16_t , FORMAT > Construct ( const int16_t& aInt )
    {
      return _Integer< int16_t , FORMAT > ( aInt );
    }
  };

  template< typename FORMAT >
  struct IntegerFactory < uint32_t , FORMAT >
  {
    static _Integer< uint32_t , FORMAT > Construct ( const uint32_t& aInt )
    {
      return _Integer< uint32_t , FORMAT > ( aInt );
    }
  };

  template< typename FORMAT >
  struct IntegerFactory < int32_t , FORMAT >
  {
    static _Integer< int32_t , FORMAT > Construct ( const int32_t& aInt )
    {
      return _Integer< int32_t , FORMAT > ( aInt );
    }
  };

  template< typename FORMAT >
  struct IntegerFactory < uint64_t , FORMAT >
  {
    static _Integer< uint64_t , FORMAT > Construct ( const uint64_t& aInt )
    {
      return _Integer< uint64_t , FORMAT > ( aInt );
    }
  };

  template< typename FORMAT >
  struct IntegerFactory < int64_t , FORMAT >
  {
    static _Integer< int64_t , FORMAT > Construct ( const int64_t& aInt )
    {
      return _Integer< int64_t , FORMAT > ( aInt );
    }
  };




  template< typename T >
  _Integer< T , IntFmt<> > Integer ( const T& aT )
  {
    return IntegerFactory< T , IntFmt<> >::Construct ( aT );
  }

  template< typename T , typename FORMAT >
  _Integer< T , FORMAT > Integer ( const T& aT , const FORMAT& aFmt )
  {
    return IntegerFactory< T , FORMAT >::Construct ( aT );
  }






  template< typename T >
  void SignHelper ( const T& aInt )
  {}


  template< typename T , uint32_t WIDTH >
  void log_inserter ( const _Integer< T , IntFmt<bin , fixed , WIDTH> >& aInt )
  {
    uint32_t lSize ( sizeof ( T ) << 3 ); //number of characters
    put ( "0b" );
    int32_t i ( WIDTH-lSize );

    if ( i > 0 )
    {
      for ( ; i!=0 ; --i )
      {
        put ( '0' );
      }
    }

    T lValue ( aInt.value() );
    T lMask ( 0x1 );
    lMask <<= ( lSize-1 );

    for ( uint32_t i=0 ; i!=lSize ; ++i )
    {
      put ( ( lValue & lMask ) ?'1':'0' );
      lValue <<= 1;
    }
  }


  template< typename T , uint32_t WIDTH >
  void log_inserter ( const _Integer< T , IntFmt<dec , fixed , WIDTH> >& aInt )
  {
    static const char* lCharacterMapping ( "9876543210123456789" );
    static const char* lCharacterMappingCenter ( lCharacterMapping + 9 );
    char lBuffer[24]; //greater than the size of a 64bit decimal number
    char* lPtr = lBuffer;
    T value ( aInt.value() );
    T tmp_value;
    SignHelper ( value );

    do
    {
      tmp_value = value;
      value /= 10;
      *lPtr++ = * ( lCharacterMappingCenter + tmp_value - ( value * 10 ) );
    }
    while ( value );

    int32_t i ( WIDTH- ( lPtr-lBuffer ) );

    if ( i > 0 )
    {
      for ( ; i!=0 ; --i )
      {
        put ( '0' );
      }
    }

    do
    {
      put ( * ( --lPtr ) );
    }
    while ( lPtr!=lBuffer );
  }


  template< typename T , uint32_t WIDTH >
  void log_inserter ( const _Integer< T , IntFmt<hex , fixed , WIDTH> >& aInt )
  {
    uint32_t lSize ( sizeof ( T ) << 1 ); //number of characters
    static const char* lCharacterMapping ( "0123456789ABCDEF" );
    put ( "0x" );
    int32_t i ( WIDTH-lSize );

    if ( i > 0 )
    {
      for ( ; i!=0 ; --i )
      {
        put ( '0' );
      }
    }

    uint8_t* lStart ( ( uint8_t* ) ( & aInt.value() ) );
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
  void log_inserter ( const _Integer< T , IntFmt<bin , variable , 0> >& aInt )
  {
    if ( aInt.value() == T ( 0 ) )
    {
      put ( "0b0" );
    }
    else
    {
      uint32_t lSize ( sizeof ( T ) <<3 );
      put ( "0b" );
      T lValue ( aInt.value() );
      T lMask ( 0x1 );
      lMask <<= ( lSize-1 );
      bool lPrint ( false );
      bool lCurrent ( false );

      for ( uint32_t i=0 ; i!=lSize ; ++i )
      {
        lCurrent = lValue & lMask;

        if ( lPrint |= lCurrent )
        {
          put ( lCurrent?'1':'0' );
        }

        lValue <<= 1;
      }
    }
  }



  template< typename T >
  void log_inserter ( const _Integer< T , IntFmt<dec , variable , 0> >& aInt )
  {
    static const char* lCharacterMapping ( "9876543210123456789" );
    static const char* lCharacterMappingCenter ( lCharacterMapping + 9 );
    char lBuffer[24]; //greater than the size of a 64bit decimal number
    char* lPtr = lBuffer;
    T value ( aInt.value() );
    T tmp_value;
    SignHelper ( value );

    do
    {
      tmp_value = value;
      value /= 10;
      *lPtr++ = * ( lCharacterMappingCenter + tmp_value - ( value * 10 ) );
    }
    while ( value );

    do
    {
      put ( * ( --lPtr ) );
    }
    while ( lPtr!=lBuffer );
  }



  template< typename T >
  void log_inserter ( const _Integer< T , IntFmt<hex , variable , 0> >& aInt )
  {
    static const char* lCharacterMapping ( "0123456789ABCDEF" );

    if ( aInt.value() == T ( 0 ) )
    {
      put ( "0x0" );
    }
    else
    {
      uint32_t lSize ( sizeof ( T ) );
      put ( "0x" );
      bool lPrint ( false );
      uint32_t lPos ( 0 );
      uint8_t* lStart ( ( uint8_t* ) ( & aInt.value() ) );
      uint8_t* lPtr ( lStart + lSize );

      do
      {
        --lPtr;
        lPos = ( ( ( *lPtr ) &0xF0 ) >>4 );

        if ( lPrint |= ( bool ) ( lPos ) )
        {
          put ( * ( lCharacterMapping + lPos ) );
        }

        lPos = ( ( *lPtr ) &0x0F );

        if ( lPrint |= ( bool ) ( lPos ) )
        {
          put ( * ( lCharacterMapping + lPos ) );
        }
      }
      while ( lPtr!=lStart );
    }
  }




}

