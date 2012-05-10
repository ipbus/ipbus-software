/**
	@file
	@author Andrew W. Rose
	@date 2012
*/






template< typename T >
std::ostream& operator<< ( std::ostream& aStream , const uhal::_lazy_stream_inserter_<T>& aLazyInserter )
{
	aStream << aLazyInserter.data();
	return aStream;
}


namespace uhal
{
	template< typename T >
	_lazy_stream_inserter_< T >::_lazy_stream_inserter_ ( const T& aT ) :
		mT ( aT ),
		mLength ( 0 ),
		mValue ( NULL )
	{
	}

	template< typename T >
	_lazy_stream_inserter_< T >::~_lazy_stream_inserter_()
	{
		try
		{
			if ( mValue )
			{
				delete mValue;
				mValue = NULL;
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	char const* _lazy_stream_inserter_< T >::data() const
	{
		try
		{
			if ( !mValue )
			{
				construct();
			}

			return mValue;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	size_t _lazy_stream_inserter_< T >::length() const
	{
		try
		{
			if ( !mValue )
			{
				construct();
			}

			return mLength;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	void _lazy_stream_inserter_< T >::construct() const
	{
		try
		{
			const_cast<_lazy_stream_inserter_*> ( this )->construct();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	void _lazy_stream_inserter_< T >::construct()
	{
		try
		{
			std::stringstream lStream;
			lStream << mT;
			std::string lStr ( lStream.str() );
			mLength = lStr.size() +1;
			mValue = new char [ mLength ];
			strcpy ( mValue, lStr.c_str() );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	template< typename T >
	inline _lazy_stream_inserter_<T> lazy_stream_inserter ( const T& aT )
	{
		try
		{
			return _lazy_stream_inserter_<T> ( aT );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}



	template< typename T >
	inline char const* c_str_data_a ( const _lazy_stream_inserter_<T>& aLazyInserter )
	{
		try
		{
			return aLazyInserter.data();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	inline size_t c_str_len_a ( const _lazy_stream_inserter_<T>& aLazyInserter )
	{
		try
		{
			return aLazyInserter.length();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


}

