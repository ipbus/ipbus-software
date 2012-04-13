
#include <uhal/ValMem.hpp>

#include "uhal/log.hpp"
#include "uhal/Utilities.hpp"

namespace uhal
{
	template< typename T >

ValWord< T >::ValWord ( const T& aValue , const uint32_t& aMask ) try :
		// mValid ( new bool ( false ) ),
		// mValue ( new T ( aValue ) )
		mMembers ( new _ValWord_<T> ( aValue , false , aMask ) )
	{
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< typename T >

ValWord< T >::ValWord ( const ValWord<T>& aVal ) try :
		// mValid ( aVal.mValid ),
		// mValue ( aVal.mValue )
		mMembers ( aVal.mMembers )
	{
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< typename T >

ValWord< T >::ValWord() try :
		// mValid ( new bool ( false ) ),
		// mValue ( new T() )
		mMembers ( new _ValWord_<T> ( T() , false , 0xFFFFFFFF ) )
	{
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< typename T >
	bool ValWord< T >::valid()
	{
		try
		{
			return /* *mValid */ mMembers->valid;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	void ValWord< T >::valid ( bool aValid )
	{
		try
		{
			/* *mValid */ mMembers->valid = aValid;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	ValWord< T >& ValWord< T >::operator = ( const T& aValue )
	{
		try
		{
			/* *mValue */ mMembers->value = aValue ;
			return *this;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
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
	// if ( /* *mValid */ mMembers->valid )
	// {
	// return /* *mValue */ mMembers->value;
	// }
	// else
	// {
	// pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
	// throw NonValidatedMemory();
	// }
	// }

	template< typename T >
	ValWord< T >::operator T()
	{
		try
		{
			return value();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	T ValWord< T >::value() const
	{
		try
		{
			if ( /* *mValid */ mMembers->valid )
			{
				return /* *mValue */ ( mMembers->value & mMembers->mask ) >> utilities::TrailingRightBits ( mMembers->mask ) ;
			}
			else
			{
				pantheios::log_ERROR ( "Access attempted on non-validated memory" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw NonValidatedMemory();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	const uint32_t& ValWord< T >::mask() const
	{
		try
		{
			return mMembers->mask;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	void ValWord< T >::mask ( const uint32_t& aMask )
	{
		try
		{
			mMembers->mask = aMask ;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	template< typename T >
	void ValWord< T >::value ( const T& aValue )
	{
		try
		{
			if ( !/* *mValid */ mMembers->valid )
			{
				/* *mValue */ mMembers->value = aValue;
			}
			else
			{
				pantheios::log_ERROR ( "Attempted  to modify validated memory" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw ValMemImutabilityViolation();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}




	template< typename T >

ValVector< T >::ValVector ( const std::vector<T>& aValues ) try :
		// mValid ( new bool ( false ) ),
		// mValues ( new std::vector<T> ( aValues ) )
		mMembers ( new _ValVector_<T> ( aValues , false ) )
	{
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< typename T >

ValVector< T >::ValVector ( const ValVector< T >::ValVector& aValues ) try :
		// mValid ( aValues.mValid ),
		// mValues ( aValues.mValues )
		mMembers ( aValues.mMembers )
	{
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< typename T >

ValVector< T >::ValVector ( uint32_t aSize ) try :
		// mValid ( new bool ( false ) ),
		// mValues ( new std::vector<T> ( aSize ) )
		mMembers ( new _ValVector_<T> ( std::vector<T> ( aSize , T() ) , false ) )
	{
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< typename T >

ValVector< T >::ValVector() try :
		// mValid ( new bool ( false ) ),
		// mValues ( new std::vector<T>() )
		mMembers ( new _ValVector_<T> ( std::vector<T>() , false ) )
	{
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< typename T >
	bool ValVector< T >::valid()
	{
		try
		{
			return /* *mValid */ mMembers->valid;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	void ValVector< T >::valid ( bool aValid )
	{
		try
		{
			/* *mValid */ mMembers->valid = aValid;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}



	template< typename T >
	void ValVector< T >::push_back ( const T& aValue )
	{
		try
		{
			if ( !/* *mValid */ mMembers->valid )
			{
				/* mValues-> */ mMembers->value.push_back ( aValue );
			}
			else
			{
				pantheios::log_ERROR ( "Attempted  to modify validated memory" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw ValMemImutabilityViolation();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	const T& ValVector< T >::operator[] ( std::size_t aSize ) const
	{
		try
		{
			if ( /* *mValid */ mMembers->valid )
			{
				return ( /* *mValue */ mMembers->value ) [aSize];
			}
			else
			{
				pantheios::log_ERROR ( "Access attempted on non-validated memory" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw NonValidatedMemory();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	const T& ValVector< T >::at ( std::size_t aSize ) const
	{
		try
		{
			if ( /* *mValid */ mMembers->valid )
			{
				return /* mValues-> */ mMembers->value.at ( aSize );
			}
			else
			{
				pantheios::log_ERROR ( "Access attempted on non-validated memory" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw NonValidatedMemory();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	std::size_t ValVector< T >::size() const
	{
		try
		{
			if ( /* *mValid */ mMembers->valid )
			{
				return /* mValues-> */ mMembers->value.size();
			}
			else
			{
				pantheios::log_ERROR ( "Access attempted on non-validated memory" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw NonValidatedMemory();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	void ValVector< T >::clear()
	{
		try
		{
			/* *mValid */ mMembers->valid = false;
			/* mValues-> */
			mMembers->value.clear();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	typename ValVector< T >::const_iterator ValVector< T >::begin() const
	{
		try
		{
			if ( /* *mValid */ mMembers->valid )
			{
				return /* mValues-> */ mMembers->value.begin();
			}
			else
			{
				pantheios::log_ERROR ( "Access attempted on non-validated memory" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw NonValidatedMemory();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	typename ValVector< T >::const_iterator ValVector< T >::end() const
	{
		try
		{
			if ( /* *mValid */ mMembers->valid )
			{
				return /* mValues-> */ mMembers->value.end();
			}
			else
			{
				pantheios::log_ERROR ( "Access attempted on non-validated memory" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw NonValidatedMemory();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	typename ValVector< T >::const_reverse_iterator ValVector< T >::rbegin() const
	{
		try
		{
			if ( /* *mValid */ mMembers->valid )
			{
				return /* mValues-> */ mMembers->value.rbegin();
			}
			else
			{
				pantheios::log_ERROR ( "Access attempted on non-validated memory" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw NonValidatedMemory();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	typename ValVector< T >::const_reverse_iterator ValVector< T >::rend() const
	{
		try
		{
			if ( /* *mValid */ mMembers->valid )
			{
				return /* mValues-> */ mMembers->value.rend();
			}
			else
			{
				pantheios::log_ERROR ( "Access attempted on non-validated memory" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw NonValidatedMemory();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}




	template class ValWord< uint32_t >;
	template class ValWord< int32_t >;
	template class ValVector< uint32_t >;
	template class ValVector< int32_t >;

}
