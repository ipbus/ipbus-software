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


#include <uhal/ValMem.hpp>

#include "uhal/log/log.hpp"
#include "uhal/Utilities.hpp"

namespace uhal
{

  _ValHeader_::_ValHeader_ ( const bool& aValid ) :
    valid ( aValid )
  {
    logging();
  }


  template< typename T >

  _ValWord_<T>::_ValWord_ ( const T& aValue , const bool& aValid , const uint32_t aMask ) :
    _ValHeader_ ( aValid ) ,
    value ( aValue ) ,
    mask ( aMask )
  {
    logging();
  }



  template< typename T >

  _ValVector_<T>::_ValVector_ ( const std::vector<T>& aValue , const bool& aValid ) :
    _ValHeader_ ( aValid ),
    value ( aValue )
  {
    logging();
  }






  ValHeader::ValHeader() :
    mMembers ( new _ValHeader_ ( false ) )
  {
    logging();
  }


  bool ValHeader::valid()
  {
    logging();
    return mMembers->valid;
  }

  void ValHeader::valid ( bool aValid )
  {
    logging();
    mMembers->valid = aValid;
  }


  // const std::deque<uint32_t>& ValHeader::returnedHeaders()
  // {
  // try
  // {
  // if ( mMembers->valid )
  // {
  // return mMembers->IPbusHeaders ;
  // }
  // else
  // {
  // log ( Error() , "Access attempted on non-validated memory" );
  // throw // NonValidatedMemory();
  // }
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }




  template< typename T >

  ValWord< T >::ValWord ( const T& aValue , const uint32_t& aMask ) :
    mMembers ( new _ValWord_<T> ( aValue , false , aMask ) )
  {
    logging();
  }


  template< typename T >

  ValWord< T >::ValWord ( const ValWord<T>& aVal ) :
    mMembers ( aVal.mMembers )
  {
    logging();
  }

  template< typename T >

  ValWord< T >::ValWord() :
    mMembers ( new _ValWord_<T> ( T() , false , 0xFFFFFFFF ) )
  {
    logging();
  }

  template< typename T >
  bool ValWord< T >::valid()
  {
    logging();
    return mMembers->valid;
  }

  template< typename T >
  void ValWord< T >::valid ( bool aValid )
  {
    logging();
    mMembers->valid = aValid;
  }

  template< typename T >
  ValWord< T >& ValWord< T >::operator = ( const T& aValue )
  {
    logging();
    mMembers->value = aValue ;
    return *this;
  }

  // template< typename T >
  // ValWord< T >::operator const T&()
  // {
  // return value();
  // }

  // template< typename T >
  // const T& ValWord< T >::value() const
  // {
  // if ( mMembers->valid )
  // {
  // return mMembers->value;
  // }
  // else
  // {
  //
  // NonValidatedMemory().throwFrom( ThisLocation() );
  // }
  // }

  template< typename T >
  ValWord< T >::operator T()
  {
    logging();
    return value();
  }

  template< typename T >
  T ValWord< T >::value() const
  {
    logging();

    if ( mMembers->valid )
    {
      return ( mMembers->value & mMembers->mask ) >> utilities::TrailingRightBits ( mMembers->mask ) ;
    }
    else
    {
      log ( Error() , "Access attempted on non-validated memory" );
      throw NonValidatedMemory();
    }
  }

  template< typename T >
  void ValWord< T >::value ( const T& aValue )
  {
    logging();

    if ( !mMembers->valid )
    {
      mMembers->value = aValue;
    }
    else
    {
      log ( Error() , "Attempted  to modify validated memory" );
      throw ValMemImutabilityViolation();
    }
  }

  template< typename T >
  const uint32_t& ValWord< T >::mask() const
  {
    logging();
    return mMembers->mask;
  }

  template< typename T >
  void ValWord< T >::mask ( const uint32_t& aMask )
  {
    logging();
    mMembers->mask = aMask ;
  }


  // template< typename T >
  // const std::deque<uint32_t>& ValWord< T >::returnedHeaders()
  // {
  // try
  // {
  // if ( mMembers->valid )
  // {
  // return mMembers->IPbusHeaders ;
  // }
  // else
  // {
  // log ( Error() , "Access attempted on non-validated memory" );
  // throw // NonValidatedMemory();
  // }
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }






  template< typename T >

  ValVector< T >::ValVector ( const std::vector<T>& aValues ) :
    mMembers ( new _ValVector_<T> ( aValues , false ) )
  {
    logging();
  }


  template< typename T >

  ValVector< T >::ValVector ( const ValVector& aValues ) :
    mMembers ( aValues.mMembers )
  {
    logging();
  }


  template< typename T >

  ValVector< T >::ValVector ( uint32_t aSize )  :
    mMembers ( new _ValVector_<T> ( std::vector<T> ( aSize , T() ) , false ) )
  {
    logging();
  }


  template< typename T >

  ValVector< T >::ValVector() :
    mMembers ( new _ValVector_<T> ( std::vector<T>() , false ) )
  {
    logging();
  }


  template< typename T >
  bool ValVector< T >::valid()
  {
    logging();
    return mMembers->valid;
  }

  template< typename T >
  void ValVector< T >::valid ( bool aValid )
  {
    logging();
    mMembers->valid = aValid;
  }



  template< typename T >
  void ValVector< T >::push_back ( const T& aValue )
  {
    logging();

    if ( !mMembers->valid )
    {
      mMembers->value.push_back ( aValue );
    }
    else
    {
      log ( Error() , "Attempted  to modify validated memory" );
      throw ValMemImutabilityViolation();
    }
  }

  template< typename T >
  const T& ValVector< T >::operator[] ( std::size_t aIndex ) const
  {
    logging();

    if ( mMembers->valid )
    {
      return ( mMembers->value ) [aIndex];
    }
    else
    {
      log ( Error() , "Access attempted on non-validated memory" );
      throw NonValidatedMemory();
    }
  }

  template< typename T >
  const T& ValVector< T >::at ( std::size_t aIndex ) const
  {
    logging();

    if ( mMembers->valid )
    {
      return  mMembers->value.at ( aIndex );
    }
    else
    {
      log ( Error() , "Access attempted on non-validated memory" );
      throw NonValidatedMemory();
    }
  }

  template< typename T >
  std::size_t ValVector< T >::size() const
  {
    logging();
    return mMembers->value.size();
    /*
    if ( mMembers->valid )
    {
    	return  mMembers->value.size();
    }
    else
    {
    	log ( Error() , "Access attempted on non-validated memory" );

    	NonValidatedMemory().throwFrom( ThisLocation() );
    }
    */
  }

  template< typename T >
  void ValVector< T >::clear()
  {
    logging();
    mMembers->valid = false;
    mMembers->value.clear();
  }

  template< typename T >
  typename ValVector< T >::const_iterator ValVector< T >::begin() const
  {
    logging();

    if ( mMembers->valid )
    {
      return  mMembers->value.begin();
    }
    else
    {
      log ( Error() , "Access attempted on non-validated memory" );
      throw NonValidatedMemory();
    }
  }

  template< typename T >
  typename ValVector< T >::const_iterator ValVector< T >::end() const
  {
    logging();

    if ( mMembers->valid )
    {
      return  mMembers->value.end();
    }
    else
    {
      log ( Error() , "Access attempted on non-validated memory" );
      throw NonValidatedMemory();
    }
  }

  template< typename T >
  typename ValVector< T >::const_reverse_iterator ValVector< T >::rbegin() const
  {
    logging();

    if ( mMembers->valid )
    {
      return  mMembers->value.rbegin();
    }
    else
    {
      log ( Error() , "Access attempted on non-validated memory" );
      throw NonValidatedMemory();
    }
  }

  template< typename T >
  typename ValVector< T >::const_reverse_iterator ValVector< T >::rend() const
  {
    logging();

    if ( mMembers->valid )
    {
      return  mMembers->value.rend();
    }
    else
    {
      log ( Error() , "Access attempted on non-validated memory" );
      throw NonValidatedMemory();
    }
  }


  template< typename T >
  std::vector<T> ValVector< T >::value() const
  {
    logging();

    if ( mMembers->valid )
    {
      return mMembers->value;
    }
    else
    {
      log ( Error() , "Access attempted on non-validated memory" );
      throw NonValidatedMemory();
    }
  }

  template< typename T >
  void ValVector< T >::value ( const std::vector<T>& aValue )
  {
    logging();

    if ( !mMembers->valid )
    {
      mMembers->value = aValue;
    }
    else
    {
      log ( Error() , "Attempted  to modify validated memory" );
      throw ValMemImutabilityViolation();
    }
  }

  /*
  	template< typename T >
  	typename ValVector< T >::iterator ValVector< T >::begin()
  	{
  		try
  		{
  			if ( !mMembers->valid )
  			{
  				return  mMembers->value.begin();
  			}
  			else
  			{
  				log ( Error() , "Attempted  to modify validated memory. If you do not intend to modify the memory, please use a const_iterator." );

  				ValMemImutabilityViolation().throwFrom( ThisLocation() );
  			}
  		}
  		catch ( uhal::exception& aExc )
  {
  aExc.rethrowFrom( ThisLocation() );
  }
  catch ( const std::exception& aExc )
  		{
  			log ( Error() , "Exception " , Quote( aExc.what() ) , " caught at " , ThisLocation() );
  StdException ( aExc ).throwFrom( ThisLocation() );
  		}
  	}

  	template< typename T >
  	typename ValVector< T >::iterator ValVector< T >::end()
  	{
  		try
  		{
  			if ( !mMembers->valid )
  			{
  				return  mMembers->value.end();
  			}
  			else
  			{
  				log ( Error() , "Attempted  to modify validated memory. If you do not intend to modify the memory, please use a const_iterator." );

  				ValMemImutabilityViolation().throwFrom( ThisLocation() );
  			}
  		}
  		catch ( uhal::exception& aExc )
  {
  aExc.rethrowFrom( ThisLocation() );
  }
  catch ( const std::exception& aExc )
  		{
  			log ( Error() , "Exception " , Quote( aExc.what() ) , " caught at " , ThisLocation() );
  StdException ( aExc ).throwFrom( ThisLocation() );
  		}
  	}

  	template< typename T >
  	typename ValVector< T >::reverse_iterator ValVector< T >::rbegin()
  	{
  		try
  		{
  			if ( !mMembers->valid )
  			{
  				return  mMembers->value.rbegin();
  			}
  			else
  			{
  				log ( Error() , "Attempted  to modify validated memory. If you do not intend to modify the memory, please use a const_reverse_iterator." );

  				ValMemImutabilityViolation().throwFrom( ThisLocation() );
  			}
  		}
  		catch ( uhal::exception& aExc )
  {
  aExc.rethrowFrom( ThisLocation() );
  }
  catch ( const std::exception& aExc )
  		{
  			log ( Error() , "Exception " , Quote( aExc.what() ) , " caught at " , ThisLocation() );
  StdException ( aExc ).throwFrom( ThisLocation() );
  		}
  	}

  	template< typename T >
  	typename ValVector< T >::reverse_iterator ValVector< T >::rend()
  	{
  		try
  		{
  			if ( !mMembers->valid )
  			{
  				return  mMembers->value.rend();
  			}
  			else
  			{
  				log ( Error() , "Attempted  to modify validated memory. If you do not intend to modify the memory, please use a const_iterator." );

  				ValMemImutabilityViolation().throwFrom( ThisLocation() );
  			}
  		}
  		catch ( uhal::exception& aExc )
  {
  aExc.rethrowFrom( ThisLocation() );
  }
  catch ( const std::exception& aExc )
  		{
  			log ( Error() , "Exception " , Quote( aExc.what() ) , " caught at " , ThisLocation() );
  StdException ( aExc ).throwFrom( ThisLocation() );
  		}
  	}
  */


  // template< typename T >
  // const std::deque<uint32_t>& ValVector< T >::returnedHeaders()
  // {
  // try
  // {
  // if ( mMembers->valid )
  // {
  // return mMembers->IPbusHeaders ;
  // }
  // else
  // {
  // log ( Error() , "Access attempted on non-validated memory" );
  // throw // NonValidatedMemory();
  // }
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }



  template class ValWord< uint32_t >;
  template class ValWord< int32_t >;
  template class ValVector< uint32_t >;
  template class ValVector< int32_t >;

}
