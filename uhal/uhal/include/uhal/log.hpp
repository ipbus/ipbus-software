#ifndef _uhal_log_hpp_
#define _uhal_log_hpp_

#include <sstream>

#include "uhal/exception.hpp"

#include "pantheios/pantheios.hpp"
#include "pantheios/inserters/integer.hpp"
#include "pantheios/frontends/stock.h"

#include "boost/lexical_cast.hpp"

#define ThisLocation() ( std::string( "function \"" ) \
						+ std::string( __PRETTY_FUNCTION__ ) \
						+ std::string( "\" in " ) \
						+ std::string( __FILE__ ) \
						+ std::string( ", line " ) \
						+ boost::lexical_cast<std::string>(__LINE__) \
						+ std::string( "." ) \
					)

#define log_LOCATION() log_DEBUG( "In " , ThisLocation() )

#define log_EXCEPTION( AEXC ) log_ERROR( "Exception \"" , AEXC.what() , "\" caught at " , ThisLocation() );

// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{

	template< typename T > class _lazy_inserter_;
	template< typename T > _lazy_inserter_<T> lazy_inserter ( const T& aT );

	template< typename T >
	class _lazy_inserter_
	{
			friend _lazy_inserter_<T> lazy_inserter<T> ( const T& aT );

		private:
			_lazy_inserter_ ( const T& aT ) :
				mT ( aT ),
				mLength ( 0 ),
				mValue ( NULL )
			{
			}

		public:
			virtual ~_lazy_inserter_()
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

		public:
			char const* data() const
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

			size_t length() const
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

		private:
			void construct() const
			{
				try
				{
					const_cast<_lazy_inserter_*> ( this )->construct();
				}
				catch ( const std::exception& aExc )
				{
					pantheios::log_EXCEPTION ( aExc );
					throw uhal::exception ( aExc );
				}
			}

			void construct()
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

		private:
			const T&	mT;
			size_t		mLength;
			char*		mValue;
	};


	template< typename T >
	inline _lazy_inserter_<T> lazy_inserter ( const T& aT )
	{
		try
		{
			return _lazy_inserter_<T> ( aT );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< typename T >
	inline char const* c_str_data_a ( const _lazy_inserter_<T>& aLazyInserter )
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
	inline size_t c_str_len_a ( const _lazy_inserter_<T>& aLazyInserter )
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
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#endif
