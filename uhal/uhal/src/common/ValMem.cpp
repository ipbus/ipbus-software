
#include <uhal/ValMem.hpp>



namespace uhal
{
	template< typename T >
	ValWord< T >::ValWord ( const T& aValue ) :
		mValid ( new bool ( false ) ),
		mValue ( new T ( aValue ) )
	{
	};

	template< typename T >
	ValWord< T >::ValWord ( const ValWord<T>& aVal ) :
		mValid ( aVal.mValid ),
		mValue ( aVal.mValue )
	{
	};

	template< typename T >
	ValWord< T >::ValWord() :
		mValid ( new bool ( false ) ),
		mValue ( new T() )
	{
	}

	template< typename T >
	bool ValWord< T >::valid()
	{
		return *mValid;
	}

	template< typename T >
	void ValWord< T >::valid ( bool aValid )
	{
		*mValid = aValid;
	}

	template< typename T >
	ValWord< T >& ValWord< T >::operator = ( const T& aValue )
	{
		*mValue = aValue ;
		return *this;
	}

	template< typename T >
	ValWord< T >::operator const T&()
	{
		return value();
	}

	template< typename T >
	const T& ValWord< T >::value() const
	{
		if ( *mValid )
		{
			return *mValue;
		}
		else
		{
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	void ValWord< T >::value ( const T& aValue )
	{
		if ( !*mValid )
		{
			*mValue = aValue;
		}
		else
		{
			throw ValMemImutabilityViolation();
		}
	}




	template< typename T >
	ValVector< T >::ValVector ( const std::vector<T>& aValues ) :
		mValid ( new bool ( false ) ),
		mValues ( new std::vector<T> ( aValues ) )
	{
	}

	template< typename T >
	ValVector< T >::ValVector ( const ValVector< T >::ValVector& aValues ) :
		mValid ( aValues.mValid ),
		mValues ( aValues.mValues )
	{
	}

	template< typename T >
	ValVector< T >::ValVector ( uint32_t aSize ) :
		mValid ( new bool ( false ) ),
		mValues ( new std::vector<T> ( aSize ) )
	{
	}

	template< typename T >
	ValVector< T >::ValVector() :
		mValid ( new bool ( false ) ),
		mValues ( new std::vector<T>() )
	{
	}

	template< typename T >
	bool ValVector< T >::valid()
	{
		return *mValid;
	}

	template< typename T >
	void ValVector< T >::valid ( bool aValid )
	{
		*mValid = aValid;
	}



	template< typename T >
	void ValVector< T >::push_back ( const T& aValue )
	{
		if ( !*mValid )
		{
			mValues->push_back ( aValue );
		}
		else
		{
			throw ValMemImutabilityViolation();
		}
	}

	template< typename T >
	const T& ValVector< T >::operator[] ( std::size_t aSize ) const
	{
		if ( *mValid )
		{
			return ( *mValues ) [aSize];
		}
		else
		{
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	const T& ValVector< T >::at ( std::size_t aSize ) const
	{
		if ( *mValid )
		{
			return mValues->at ( aSize );
		}
		else
		{
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	std::size_t ValVector< T >::size() const
	{
		if ( *mValid )
		{
			return mValues->size();
		}
		else
		{
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	void ValVector< T >::clear()
	{
		*mValid = false;
		mValues->clear();
	}

	template< typename T >
	typename ValVector< T >::const_iterator ValVector< T >::begin() const
	{
		if ( *mValid )
		{
			return mValues->begin();
		}
		else
		{
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	typename ValVector< T >::const_iterator ValVector< T >::end() const
	{
		if ( *mValid )
		{
			return mValues->end();
		}
		else
		{
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	typename ValVector< T >::const_reverse_iterator ValVector< T >::rbegin() const
	{
		if ( *mValid )
		{
			return mValues->rbegin();
		}
		else
		{
			throw NonValidatedMemory();
		}
	}

	template< typename T >
	typename ValVector< T >::const_reverse_iterator ValVector< T >::rend() const
	{
		if ( *mValid )
		{
			return mValues->rend();
		}
		else
		{
			throw NonValidatedMemory();
		}
	}


	
	
	template class ValWord< uint32_t >; 
	template class ValWord< int32_t >; 
	template class ValVector< uint32_t >; 
	template class ValVector< int32_t >; 

}
