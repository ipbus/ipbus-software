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

#include "uhal/ValMem.hpp"


#include "uhal/log/log.hpp"
#include "uhal/utilities/bits.hpp"


namespace uhal
{

  _ValHeader_::_ValHeader_ ( const bool& aValid ) :
    valid ( aValid )
  {
  }


  template< typename T >

  _ValWord_<T>::_ValWord_ ( const T& aValue , const bool& aValid , const uint32_t aMask ) :
    _ValHeader_ ( aValid ) ,
    value ( aValue ) ,
    mask ( aMask )
  {
  }



  template< typename T >

  _ValVector_<T>::_ValVector_ ( const std::vector<T>& aValue , const bool& aValid ) :
    _ValHeader_ ( aValid ),
    value ( aValue )
  {
  }




  ValHeader::ValHeader() :
    mMembers ( new _ValHeader_ ( false ) )
  {
  }


  bool ValHeader::valid()
  {
    return mMembers->valid;
  }

  void ValHeader::valid ( bool aValid )
  {
    mMembers->valid = aValid;
  }




  template< typename T >
  ValWord< T >::ValWord ( const T& aValue , const uint32_t& aMask ) :
    mMembers ( new _ValWord_<T> ( aValue , false , aMask ) )
  {
  }


  template< typename T >
  ValWord< T >::ValWord ( const ValWord<T>& aVal ) :
    mMembers ( aVal.mMembers )
  {
  }


  template< typename T >
  ValWord< T >::ValWord() :
    mMembers ( new _ValWord_<T> ( T() , false , 0xFFFFFFFF ) )
  {
  }


  template< typename T >
  bool ValWord< T >::valid()
  {
    return mMembers->valid;
  }


  template< typename T >
  void ValWord< T >::valid ( bool aValid )
  {
    mMembers->valid = aValid;
  }


  template< typename T >
  ValWord< T >& ValWord< T >::operator = ( const T& aValue )
  {
    mMembers->value = aValue ;
    return *this;
  }


  template< typename T >
  ValWord< T >::operator T()
  {
    return value();
  }


  template< typename T >
  T ValWord< T >::value() const
  {
    if ( mMembers->valid )
    {
      return ( mMembers->value & mMembers->mask ) >> utilities::TrailingRightBits ( mMembers->mask ) ;
    }
    else
    {
      exception::NonValidatedMemory lExc;
      log ( lExc , "Access attempted on non-validated memory" );
      throw lExc;
    }
  }


  template< typename T >
  void ValWord< T >::value ( const T& aValue )
  {
    if ( !mMembers->valid )
    {
      mMembers->value = aValue;
    }
    else
    {
      exception::ValMemImutabilityViolation lExc;
      log ( lExc , "Attempted to modify validated memory" );
      throw lExc;
    }
  }


  template< typename T >
  const uint32_t& ValWord< T >::mask() const
  {
    return mMembers->mask;
  }


  template< typename T >
  void ValWord< T >::mask ( const uint32_t& aMask )
  {
    mMembers->mask = aMask ;
  }




  template< typename T >
  ValVector< T >::ValVector ( const std::vector<T>& aValues ) :
    mMembers ( new _ValVector_<T> ( aValues , false ) )
  {
  }


  template< typename T >
  ValVector< T >::ValVector ( const ValVector& aValues ) :
    mMembers ( aValues.mMembers )
  {
  }


  template< typename T >
  ValVector< T >::ValVector ( const uint32_t& aSize )  :
    mMembers ( new _ValVector_<T> ( std::vector<T> ( aSize , T() ) , false ) )
  {
  }


  template< typename T >
  ValVector< T >::ValVector() :
    mMembers ( new _ValVector_<T> ( std::vector<T>() , false ) )
  {
  }


  template< typename T >
  bool ValVector< T >::valid()
  {
    return mMembers->valid;
  }


  template< typename T >
  void ValVector< T >::valid ( bool aValid )
  {
    mMembers->valid = aValid;
  }



  template< typename T >
  void ValVector< T >::push_back ( const T& aValue )
  {
    if ( !mMembers->valid )
    {
      mMembers->value.push_back ( aValue );
    }
    else
    {
      exception::ValMemImutabilityViolation lExc;
      log ( lExc , "Attempted to modify validated memory" );
      throw lExc;
    }
  }


  template< typename T >
  const T& ValVector< T >::operator[] ( std::size_t aIndex ) const
  {
    if ( mMembers->valid )
    {
      return ( mMembers->value ) [aIndex];
    }
    else
    {
      exception::NonValidatedMemory lExc;
      log ( lExc , "Access attempted on non-validated memory" );
      throw lExc;
    }
  }


  template< typename T >
  const T& ValVector< T >::at ( std::size_t aIndex ) const
  {
    if ( mMembers->valid )
    {
      return  mMembers->value.at ( aIndex );
    }
    else
    {
      exception::NonValidatedMemory lExc;
      log ( lExc , "Access attempted on non-validated memory" );
      throw lExc;
    }
  }


  template< typename T >
  std::size_t ValVector< T >::size() const
  {
    return mMembers->value.size();
  }


  template< typename T >
  const T* ValVector< T >::data() const
  {
    if ( mMembers->valid )
    {
      return mMembers->value.data();
    }
    else
    {
      exception::NonValidatedMemory lExc;
      log ( lExc , "Access attempted on non-validated memory" );
      throw lExc;
    }
  }


  template< typename T >
  void ValVector< T >::clear()
  {
    mMembers->valid = false;
    mMembers->value.clear();
  }


  template< typename T >
  typename ValVector< T >::const_iterator ValVector< T >::begin() const
  {
    if ( mMembers->valid )
    {
      return  mMembers->value.begin();
    }
    else
    {
      exception::NonValidatedMemory lExc;
      log ( lExc , "Access attempted on non-validated memory" );
      throw lExc;
    }
  }


  template< typename T >
  typename ValVector< T >::const_iterator ValVector< T >::end() const
  {
    if ( mMembers->valid )
    {
      return  mMembers->value.end();
    }
    else
    {
      exception::NonValidatedMemory lExc;
      log ( lExc , "Access attempted on non-validated memory" );
      throw lExc;
    }
  }


  template< typename T >
  typename ValVector< T >::const_reverse_iterator ValVector< T >::rbegin() const
  {
    if ( mMembers->valid )
    {
      return  mMembers->value.rbegin();
    }
    else
    {
      exception::NonValidatedMemory lExc;
      log ( lExc , "Access attempted on non-validated memory" );
      throw lExc;
    }
  }


  template< typename T >
  typename ValVector< T >::const_reverse_iterator ValVector< T >::rend() const
  {
    if ( mMembers->valid )
    {
      return  mMembers->value.rend();
    }
    else
    {
      exception::NonValidatedMemory lExc;
      log ( lExc , "Access attempted on non-validated memory" );
      throw lExc;
    }
  }


  template< typename T >
  std::vector<T> ValVector< T >::value() const
  {
    if ( mMembers->valid )
    {
      return mMembers->value;
    }
    else
    {
      exception::NonValidatedMemory lExc;
      log ( lExc , "Access attempted on non-validated memory" );
      throw lExc;
    }
  }

  template< typename T >
  void ValVector< T >::value ( const std::vector<T>& aValue )
  {
    if ( !mMembers->valid )
    {
      mMembers->value = aValue;
    }
    else
    {
      exception::ValMemImutabilityViolation lExc;
      log ( lExc , "Attempted to modify validated memory" );
      throw lExc;
    }
  }


  template class ValWord< uint8_t >;
  template class ValWord< uint32_t >;

  template class ValVector< uint8_t >;
  template class ValVector< uint32_t >;

}
