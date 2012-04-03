
#include <uhal/ValMem.hpp>

#include "uhal/log.hpp"
#include "uhal/Utilities.hpp"

namespace uhal
{
	template< typename T >
	ValWord< T >::ValWord ( const T& aValue , const uint32_t& aMask ) :
		// mValid ( new bool ( false ) ),
		// mValue ( new T ( aValue ) )
		mMembers( new _ValWord_<T>( aValue , false , aMask ) )
	{
	};

	template< typename T >
	ValWord< T >::ValWord ( const ValWord<T>& aVal ) :
		// mValid ( aVal.mValid ),
		// mValue ( aVal.mValue )
		mMembers( aVal.mMembers )
	{
	};

	template< typename T >
	ValWord< T >::ValWord() :
		// mValid ( new bool ( false ) ),
		// mValue ( new T() )
		mMembers( new _ValWord_<T>( T() , false , 0xFFFFFFFF ) )
	{
	}

	template< typename T >
	bool ValWord< T >::valid()
	{
		return /* *mValid */ mMembers->valid;
	}

	template< typename T >
	void ValWord< T >::valid ( bool aValid )
	{
		/* *mValid */ mMembers->valid = aValid;
	}

	template< typename T >
	ValWord< T >& ValWord< T >::operator = ( const T& aValue )
	{
		/* *mValue */ mMembers->value = aValue ;
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
		// if ( /* *mValid */ mMembers->valid )
		// {
			// return /* *mValue */ mMembers->value;
		// }
		// else
		// {
			// pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			// throw NonValidatedMemory();
		// }
	// }
	
	template< typename T >
	ValWord< T >::operator T()
	{
		return value();
	}

	template< typename T >
	T ValWord< T >::value() const
	{
		if ( /* *mValid */ mMembers->valid )
		{
			return /* *mValue */ ( mMembers->value & mMembers->mask ) >> utilities::TrailingRightBits ( mMembers->mask ) ;
		}
		else
		{
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw NonValidatedMemory();
		}
	}
	
	template< typename T >
	const uint32_t& ValWord< T >::mask() const
	{	
		return mMembers->mask;
	}
	
	template< typename T >
	void ValWord< T >::mask( const uint32_t& aMask)
	{
		mMembers->mask = aMask ;
	}
	
	
	template< typename T >
	void ValWord< T >::value ( const T& aValue )
	{
		if ( !/* *mValid */ mMembers->valid )
		{
			/* *mValue */ mMembers->value = aValue;
		}
		else
		{
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw ValMemImutabilityViolation();
		}
	}




	template< typename T >
	ValVector< T >::ValVector ( const std::vector<T>& aValues ) :
		// mValid ( new bool ( false ) ),
		// mValues ( new std::vector<T> ( aValues ) )
		mMembers( new _ValVector_<T>( aValues , false ) )
	{
	}

	template< typename T >
	ValVector< T >::ValVector ( const ValVector< T >::ValVector& aValues ) :
		// mValid ( aValues.mValid ),
		// mValues ( aValues.mValues )
		mMembers( aValues.mMembers )
	{
	}

	template< typename T >
	ValVector< T >::ValVector ( uint32_t aSize ) :
		// mValid ( new bool ( false ) ),
		// mValues ( new std::vector<T> ( aSize ) )
		mMembers( new _ValVector_<T>( std::vector<T> ( aSize ) , false ) )
	{
	}

	template< typename T >
	ValVector< T >::ValVector() :
		// mValid ( new bool ( false ) ),
		// mValues ( new std::vector<T>() )
		mMembers( new _ValVector_<T>( std::vector<T>() , false ) ) 
	{
	}

	template< typename T >
	bool ValVector< T >::valid()
	{
		return /* *mValid */ mMembers->valid;
	}

	template< typename T >
	void ValVector< T >::valid ( bool aValid )
	{
		/* *mValid */ mMembers->valid = aValid;
	}



	template< typename T >
	void ValVector< T >::push_back ( const T& aValue )
	{
		if ( !/* *mValid */ mMembers->valid )
		{
			/* mValues-> */ mMembers->value.push_back ( aValue );
		}
		else
		{
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw ValMemImutabilityViolation();
		}
	}

	template< typename T >
	const T& ValVector< T >::operator[] ( std::size_t aSize ) const
	{
		if ( /* *mValid */ mMembers->valid )
		{
			return ( /* *mValue */ mMembers->value ) [aSize];
		}
		else
		{
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	const T& ValVector< T >::at ( std::size_t aSize ) const
	{
		if ( /* *mValid */ mMembers->valid )
		{
			return /* mValues-> */ mMembers->value.at ( aSize );
		}
		else
		{
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	std::size_t ValVector< T >::size() const
	{
		if ( /* *mValid */ mMembers->valid )
		{
			return /* mValues-> */ mMembers->value.size();
		}
		else
		{
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	void ValVector< T >::clear()
	{
		/* *mValid */ mMembers->valid = false;
		/* mValues-> */ mMembers->value.clear();
	}

	template< typename T >
	typename ValVector< T >::const_iterator ValVector< T >::begin() const
	{
		if ( /* *mValid */ mMembers->valid )
		{
			return /* mValues-> */ mMembers->value.begin();
		}
		else
		{
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	typename ValVector< T >::const_iterator ValVector< T >::end() const
	{
		if ( /* *mValid */ mMembers->valid )
		{
			return /* mValues-> */ mMembers->value.end();
		}
		else
		{
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	typename ValVector< T >::const_reverse_iterator ValVector< T >::rbegin() const
	{
		if ( /* *mValid */ mMembers->valid )
		{
			return /* mValues-> */ mMembers->value.rbegin();
		}
		else
		{
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	typename ValVector< T >::const_reverse_iterator ValVector< T >::rend() const
	{
		if ( /* *mValid */ mMembers->valid )
		{
			return /* mValues-> */ mMembers->value.rend();
		}
		else
		{
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw NonValidatedMemory();
		}
	}


	
	
	template class ValWord< uint32_t >; 
	template class ValWord< int32_t >; 
	template class ValVector< uint32_t >; 
	template class ValVector< int32_t >; 

}
