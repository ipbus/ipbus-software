#ifndef _uhal_log_hpp_
#define _uhal_log_hpp_

#include <sstream>

#include "uhal/exception.hpp"

#include "pantheios/pantheios.hpp"
#include "pantheios/inserters.hpp"
#include "pantheios/frontends/stock.h"


#define ThisLocation()  uhal::lazy_inserter( uhal::Location( __PRETTY_FUNCTION__ , __FILE__ , __LINE__ ) )

#define log_LOCATION() log_DEBUG( "In " , ThisLocation() )

#define log_EXCEPTION( AEXC ) log_ERROR( "Exception \"" , AEXC.what() , "\" caught at " , ThisLocation() );


// --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// From http://blog.pantheios.org/2010/10/choosing-severity-levels.html

// A recent enquiry on the Pantheios Help Forum requested guidance for what severity levels to use in what circumstances.
// Although I've a strong opinion on this, which I'll include in a series of articles on diagnostic logging that I'm currently preparing for Dr. Dobbs (and which I hope will see publication over Dec-Feb),
// I'm happy to volunteer the basic ideas here.
//
// Although Pantheios can be used with any range severity levels that fits into the int type - which usually means 32- or 64-bits, depending on platform - as it ships Pantheios defines eight stock severity levels,
// corresponding to the eight levels defined by SysLog, as follows:
//
// Syslog level		Pantheios level (C)				Pantheios level (C++)		Integer value
//
// Emergency		PANTHEIOS_SEV_EMERGENCY			pantheios::emergency		0
// Alert			PANTHEIOS_SEV_ALERT				pantheios::alert			1
// Critical			PANTHEIOS_SEV_CRITICAL			pantheios::critical			2
// Error			PANTHEIOS_SEV_ERROR				pantheios::error			3
// Warning			PANTHEIOS_SEV_WARNING			pantheios::warning			4
// Notice			PANTHEIOS_SEV_NOTICE			pantheios::notice			5
// Info				PANTHEIOS_SEV_INFOFORMATIONAL	pantheios::informational	6
// Debug			PANTHEIOS_SEV_DEBUG				pantheios::debug			7
//
// Briefly, we treat these levels as follows. (Note, the terminology introduced in the first and second parts of my Quality Matters column for ACCU's Overload will be helpful in several instances.)
//
// - Emergency is used for (attempting to) record contract violations, i.e. the firing of an active runtime contract, indicating that the program is now behaving in contradiction to its design.
//   According to the principle of irrecoverability, a program in this state must immediately terminate, and thus it is possible to see an emergency level message being the last, or one of the last,
//   messages in the diagnostic logging sequence of a program that has faulted.
//
// - Alert is used for (attempting to) record practically-unrecoverable conditions, e.g. out of memory. It is customary to see an alert level message being the last, or one of the last,
//   messages in the diagnostic logging sequence of a program that has ended processing in a non-normative manner.
//
// - Critical and error are used for conditions that usually indicate that the normative behaviour of the program cannot be achieved.
//   There is some ambiguity as to the exact distinction to draw between them, and I'm still in two minds about it.
//   Obviously, critical is more severe, and may be associated with conditions more likely to result in program failure than does those designated as error.
//
// - Warning is used for recording warning conditions.
//
// - Notice is used for logging information that is to be displayed in "normal" operation, e.g. "database connection achieved"
//
// - Informational is used for logging information that is useful when actively monitoring the health of a system, but that is not necessarily displayed as part of "normal" operation.
//   Ideally it should be possible to turn on all logging of this level without exceeding the program's performance criteria.
//
// - Debug is used for debugging statements. Note that one of the important advanges of using Pantheios is that its high efficiency means that you don't have to make decisions about eliding certain statements at compile-time;
//   they can instead be made at runtime, which is appropriate. Using Pantheios means eliminating #ifdef DEBUG from application code, forever!
// --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	class Location;
}
std::ostream& operator<< ( std::ostream& aStream , const uhal::Location& aLocation );

namespace uhal
{

	class Location
	{
			friend std::ostream& ( ::operator<< ) ( std::ostream& aStream , const uhal::Location& aLocation )
			{
				aStream << "function \""  << aLocation.mFunction << "\" in " << aLocation.mFile << ", line " << aLocation.mLine << ".";
				return aStream;
			}

		public:
			Location ( const char* aFunction , const char* aFile , const uint32_t& aLine ) :
				mFunction ( aFunction ),
				mFile ( aFile ) ,
				mLine ( aLine )
			{}

			virtual ~Location() {}

		private:
			const char* mFunction;
			const char* mFile;
			const uint32_t mLine;
	};
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




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
