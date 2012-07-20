
#include <uhal/ValMem.hpp>

#include "uhal/log/log.hpp"
#include "uhal/Utilities.hpp"

namespace uhal
{

_ValHeader_::_ValHeader_ ( const bool& aValid ) try :
		valid ( aValid ) ,
			  IPbusHeader ( 0 )
		{}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}



	template< typename T >

_ValWord_<T>::_ValWord_ ( const T& aValue , const bool& aValid , const uint32_t aMask ) try :
		value ( aValue ) ,
			  valid ( aValid ) ,
			  mask ( aMask ) ,
			  IPbusHeader ( 0 )
		{}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}


	template< typename T >

_ValVector_<T>::_ValVector_ ( const std::vector<T>& aValue , const bool& aValid ) try :
		value ( aValue ) ,
			  valid ( aValid )
		{}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}





ValHeader::ValHeader() try :
		mMembers ( new _ValHeader_ ( false ) )
	{
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}

	bool ValHeader::valid()
	{
		try
		{
			return mMembers->valid;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	void ValHeader::valid ( bool aValid )
	{
		try
		{
			mMembers->valid = aValid;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}






	template< typename T >

ValWord< T >::ValWord ( const T& aValue , const uint32_t& aMask ) try :
		mMembers ( new _ValWord_<T> ( aValue , false , aMask ) )
	{
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}

	template< typename T >

ValWord< T >::ValWord ( const ValWord<T>& aVal ) try :
		mMembers ( aVal.mMembers )
	{
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}

	template< typename T >

ValWord< T >::ValWord() try :
		mMembers ( new _ValWord_<T> ( T() , false , 0xFFFFFFFF ) )
	{
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}

	template< typename T >
	bool ValWord< T >::valid()
	{
		try
		{
			return mMembers->valid;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	void ValWord< T >::valid ( bool aValid )
	{
		try
		{
			mMembers->valid = aValid;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	ValWord< T >& ValWord< T >::operator = ( const T& aValue )
	{
		try
		{
			mMembers->value = aValue ;
			return *this;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
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
	// log ( Error() , "Throwing at " , ThisLocation() );
	// NonValidatedMemory().throwFrom( ThisLocation() );
	// }
	// }

	template< typename T >
	ValWord< T >::operator T()
	{
		try
		{
			return value();
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	T ValWord< T >::value() const
	{
		try
		{
			if ( mMembers->valid )
			{
				return ( mMembers->value & mMembers->mask ) >> utilities::TrailingRightBits ( mMembers->mask ) ;
			}
			else
			{
				log ( Error() , "Access attempted on non-validated memory" );
				log ( Error() , "Throwing at " , ThisLocation() );
				NonValidatedMemory().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	void ValWord< T >::value ( const T& aValue )
	{
		try
		{
			if ( !mMembers->valid )
			{
				mMembers->value = aValue;
			}
			else
			{
				log ( Error() , "Attempted  to modify validated memory" );
				log ( Error() , "Throwing at " , ThisLocation() );
				ValMemImutabilityViolation().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	const uint32_t& ValWord< T >::mask() const
	{
		try
		{
			return mMembers->mask;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	void ValWord< T >::mask ( const uint32_t& aMask )
	{
		try
		{
			mMembers->mask = aMask ;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}






	template< typename T >

ValVector< T >::ValVector ( const std::vector<T>& aValues ) try :
		mMembers ( new _ValVector_<T> ( aValues , false ) )
	{
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}

	template< typename T >

ValVector< T >::ValVector ( const ValVector& aValues ) try :
		mMembers ( aValues.mMembers )
	{
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}

	template< typename T >

ValVector< T >::ValVector ( uint32_t aSize ) try :
		mMembers ( new _ValVector_<T> ( std::vector<T> ( aSize , T() ) , false ) )
	{
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}

	template< typename T >

ValVector< T >::ValVector() try :
		mMembers ( new _ValVector_<T> ( std::vector<T>() , false ) )
	{
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}

	template< typename T >
	bool ValVector< T >::valid()
	{
		try
		{
			return mMembers->valid;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	void ValVector< T >::valid ( bool aValid )
	{
		try
		{
			mMembers->valid = aValid;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}



	template< typename T >
	void ValVector< T >::push_back ( const T& aValue )
	{
		try
		{
			if ( !mMembers->valid )
			{
				mMembers->value.push_back ( aValue );
			}
			else
			{
				log ( Error() , "Attempted  to modify validated memory" );
				log ( Error() , "Throwing at " , ThisLocation() );
				ValMemImutabilityViolation().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	const T& ValVector< T >::operator[] ( std::size_t aIndex ) const
	{
		try
		{
			if ( mMembers->valid )
			{
				return ( mMembers->value ) [aIndex];
			}
			else
			{
				log ( Error() , "Access attempted on non-validated memory" );
				log ( Error() , "Throwing at " , ThisLocation() );
				NonValidatedMemory().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	const T& ValVector< T >::at ( std::size_t aIndex ) const
	{
		try
		{
			if ( mMembers->valid )
			{
				return  mMembers->value.at ( aIndex );
			}
			else
			{
				log ( Error() , "Access attempted on non-validated memory" );
				log ( Error() , "Throwing at " , ThisLocation() );
				NonValidatedMemory().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	std::size_t ValVector< T >::size() const
	{
		try
		{
			return mMembers->value.size();
			/*
			if ( mMembers->valid )
			{
				return  mMembers->value.size();
			}
			else
			{
				log ( Error() , "Access attempted on non-validated memory" );
				log ( Error() , "Throwing at " , ThisLocation() );
				NonValidatedMemory().throwFrom( ThisLocation() );
			}
			*/
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	void ValVector< T >::clear()
	{
		try
		{
			mMembers->valid = false;
			mMembers->value.clear();
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	typename ValVector< T >::const_iterator ValVector< T >::begin() const
	{
		try
		{
			if ( mMembers->valid )
			{
				return  mMembers->value.begin();
			}
			else
			{
				log ( Error() , "Access attempted on non-validated memory" );
				log ( Error() , "Throwing at " , ThisLocation() );
				NonValidatedMemory().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	typename ValVector< T >::const_iterator ValVector< T >::end() const
	{
		try
		{
			if ( mMembers->valid )
			{
				return  mMembers->value.end();
			}
			else
			{
				log ( Error() , "Access attempted on non-validated memory" );
				log ( Error() , "Throwing at " , ThisLocation() );
				NonValidatedMemory().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	typename ValVector< T >::const_reverse_iterator ValVector< T >::rbegin() const
	{
		try
		{
			if ( mMembers->valid )
			{
				return  mMembers->value.rbegin();
			}
			else
			{
				log ( Error() , "Access attempted on non-validated memory" );
				log ( Error() , "Throwing at " , ThisLocation() );
				NonValidatedMemory().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	template< typename T >
	typename ValVector< T >::const_reverse_iterator ValVector< T >::rend() const
	{
		try
		{
			if ( mMembers->valid )
			{
				return  mMembers->value.rend();
			}
			else
			{
				log ( Error() , "Access attempted on non-validated memory" );
				log ( Error() , "Throwing at " , ThisLocation() );
				NonValidatedMemory().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
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
					log ( Error() , "Throwing at " , ThisLocation() );
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
					log ( Error() , "Throwing at " , ThisLocation() );
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
					log ( Error() , "Throwing at " , ThisLocation() );
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
					log ( Error() , "Throwing at " , ThisLocation() );
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


	template class ValWord< uint32_t >;
	template class ValWord< int32_t >;
	template class ValVector< uint32_t >;
	template class ValVector< int32_t >;

}
