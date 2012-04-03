#ifndef _uhal_log_hpp_
#define _uhal_log_hpp_

#include <sstream>

#include "pantheios/pantheios.hpp"
#include "pantheios/inserters/integer.hpp"
#include "pantheios/frontends/stock.h"

#include "boost/lexical_cast.hpp"

#define ThisLocation() ( std::string( "function \"" ) \
						+ std::string( __PRETTY_FUNCTION__ ) \
						+ std::string( "\" in \"" ) \
						+ std::string( __FILE__ ) \
						+ std::string( "\", line " ) \
						+ boost::lexical_cast<std::string>(__LINE__) \
						+ std::string( "." ) \
					)

#define log_LOCATION log_DEBUG ( "In " , ThisLocation() )



// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{

	template< typename T > class _lazy_inserter_;
	template< typename T > _lazy_inserter_<T> lazy_inserter(const T & aT);

	template< typename T >
	class _lazy_inserter_
	{
		friend _lazy_inserter_<T> lazy_inserter<T> (const T & aT);
		
		private:
			_lazy_inserter_(const T & aT) :
				mT(aT),
				mLength(0),
				mValue(NULL)
			{
			}
		
		public:
			virtual ~_lazy_inserter_()
			{
				if( mValue ){
					delete mValue;
					mValue = NULL;
				}
			}		

		public:
			char const* data() const
			{
				if( !mValue )
				{
					construct();
				}
				return mValue;
			}
			
			size_t length() const
			{
				if( !mValue )
				{
					construct();
				}
				return mLength;
			}

		private:
			void construct() const{
				const_cast<_lazy_inserter_*>(this)->construct();
			}
				
			void construct()
			{
				std::stringstream lStream;
				lStream << mT;
				
				std::string lStr( lStream.str() );
			
				mLength = lStr.size()+1;
				mValue = new char [ mLength ];
				strcpy (mValue, lStr.c_str());
			}

		private:
			const T&	mT;
			size_t		mLength;
			char*		mValue;
	};


	template< typename T >
	inline _lazy_inserter_<T> lazy_inserter(const T & aT)
	{
		return _lazy_inserter_<T>( aT );
	}

	template< typename T >
	inline char const* c_str_data_a( const _lazy_inserter_<T>& aLazyInserter )
	{
		return aLazyInserter.data();
	}

	template< typename T >
	inline size_t c_str_len_a( const _lazy_inserter_<T>& aLazyInserter )
	{
		return aLazyInserter.length();
	}
	
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#endif
