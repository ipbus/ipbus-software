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


#ifndef _log_inserters_integer_hpp_
#define _log_inserters_integer_hpp_

#include <uhal/log/log_inserter_helper.hpp>

#include <stdint.h>
#include <iostream>

namespace uhal
{


  enum integer_base
  {
    bin,
    dec,
    hex
  };

  enum integer_format
  {
    fixed,
    variable
  };

  static const integer_base DefaultIntegerBase ( dec );
  static const integer_format DefaultIntegerFormat ( variable );

  template< integer_base BASE = DefaultIntegerBase , integer_format FORMAT = DefaultIntegerFormat , uint32_t WIDTH = 0 > struct IntFmt {};

  template< typename T , typename FORMAT >
  class _Integer;

  template< typename T > _Integer< T , IntFmt<> > Integer ( const T& aT );
  template< typename T , integer_base BASE , integer_format FORMAT , uint32_t WIDTH > _Integer< T , IntFmt<BASE , FORMAT , WIDTH> > Integer ( const T& aT , const IntFmt<BASE , FORMAT , WIDTH>& aFmt );

  template< typename T >
  void sign_helper ( std::ostream& aStr, const T& aInt );
  template<>
  void sign_helper ( std::ostream& aStr, const int8_t& aInt );
  template<>
  void sign_helper ( std::ostream& aStr, const int16_t& aInt );
  template<>
  void sign_helper ( std::ostream& aStr, const int32_t& aInt );
  template<>
  void sign_helper ( std::ostream& aStr, const int64_t& aInt );

  template< typename T , typename FORMAT >
  class _Integer : public RefWrapper< T >
  {
    public:
      _Integer ( const T& aT );
      void print ( std::ostream& aStr ) const;
  };


  template< typename T , uint32_t WIDTH >
  class _Integer< T , IntFmt<bin , fixed , WIDTH> > : public RefWrapper< T >
  {
    public:

      _Integer ( const T& aT ) : RefWrapper< T > ( aT ) {}
      void print ( std::ostream& aStr ) const;
  };

  template< typename T , uint32_t WIDTH >
  class _Integer< T , IntFmt<bin , variable , WIDTH> > : public RefWrapper< T >
  {
    public:

      _Integer ( const T& aT ) : RefWrapper< T > ( aT ) {}
      void print ( std::ostream& aStr ) const;
  };


  template< typename T , uint32_t WIDTH >
  class _Integer< T , IntFmt<dec , fixed , WIDTH> > : public RefWrapper< T >
  {
    public:

      _Integer ( const T& aT ) : RefWrapper< T > ( aT ) {}
      void print ( std::ostream& aStr ) const;
  };

  template< typename T , uint32_t WIDTH >
  class _Integer< T , IntFmt<dec , variable , WIDTH> > : public RefWrapper< T >
  {
    public:

      _Integer ( const T& aT ) : RefWrapper< T > ( aT ) {}
      void print ( std::ostream& aStr ) const;
  };

  template< typename T , uint32_t WIDTH >
  class _Integer< T , IntFmt<hex , fixed , WIDTH> > : public RefWrapper< T >
  {
    public:

      _Integer ( const T& aT ) : RefWrapper< T > ( aT ) {}
      void print ( std::ostream& aStr ) const;
  };

  template< typename T , uint32_t WIDTH >
  class _Integer< T , IntFmt<hex , variable , WIDTH> > : public RefWrapper< T >
  {
    public:

      _Integer ( const T& aT ) : RefWrapper< T > ( aT ) {}
      void print ( std::ostream& aStr ) const;
  };

}


template< typename FORMAT >
std::ostream& operator<< ( std::ostream& aStr , const uhal::_Integer< uint8_t , FORMAT >& aInt );

template< typename FORMAT >
std::ostream& operator<< ( std::ostream& aStr , const uhal::_Integer< int8_t , FORMAT >& aInt );

template< typename FORMAT >
std::ostream& operator<< ( std::ostream& aStr , const uhal::_Integer< uint16_t , FORMAT >& aInt );

template< typename FORMAT >
std::ostream& operator<< ( std::ostream& aStr , const uhal::_Integer< int16_t , FORMAT >& aInt );

template< typename FORMAT >
std::ostream& operator<< ( std::ostream& aStr , const uhal::_Integer< uint32_t , FORMAT >& aInt );

template< typename FORMAT >
std::ostream& operator<< ( std::ostream& aStr , const uhal::_Integer< int32_t , FORMAT >& aInt );

template< typename FORMAT >
std::ostream& operator<< ( std::ostream& aStr , const uhal::_Integer< uint64_t , FORMAT >& aInt );

template< typename FORMAT >
std::ostream& operator<< ( std::ostream& aStr , const uhal::_Integer< int64_t , FORMAT >& aInt );


#include <uhal/log/log_inserters.integer.hxx>

#endif
