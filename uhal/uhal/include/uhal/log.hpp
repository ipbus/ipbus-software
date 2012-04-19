/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_log_hpp_
#define _uhal_log_hpp_

#include <sstream>

#include "uhal/exception.hpp"

#include "pantheios/pantheios.hpp"
#include "pantheios/inserters.hpp"
#include "pantheios/frontends/stock.h"

//! Macro function to add a string specifying the current function and line in the file
#define ThisLocation()  uhal::lazy_stream_inserter( uhal::Location( __PRETTY_FUNCTION__ , __FILE__ , __LINE__ ) )

//! Helper macro Function for debugging
#define log_LOCATION() log_DEBUG( "In " , ThisLocation() )

//! Helper macro function to format exception error messages
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
// forward declaration so that we can declare friends
namespace uhal
{
	class Location;
}

/**
	A streaming operator to format the location for display
	@param aStream a stream to output the time onto
	@param aLocation the location to be output
	@return a stream for further appending
*/
std::ostream& operator<< ( std::ostream& aStream , const uhal::Location& aLocation );

namespace uhal
{
	//! A class to wrap the function name, filename and line-number location of its construction for the purpose of debugging and tracking unwinding exceptions
	class Location
	{
			/**
				A streaming operator to format the location for display
				@param aStream a stream to output the time onto
				@param aLocation the location to be output
				@return a stream for further appending
			*/
			friend std::ostream& ( ::operator<< ) ( std::ostream& aStream , const uhal::Location& aLocation );

		public:
			/**
				Constructor
				@param aFunction the name of the current function as returned by the __PRETTY_FUNCTION__ macro
				@param aFile the name of the current file as returned by the __FILE__ macro
				@param aLine the number of the current line as returned by the __LINE__ macro
			*/
			Location ( const char* aFunction , const char* aFile , const uint32_t& aLine );

		private:
			//! the name of the current function as returned by the __PRETTY_FUNCTION__ macro
			const char* mFunction;
			//! the name of the current file as returned by the __FILE__ macro
			const char* mFile;
			//! the number of the current line as returned by the __LINE__ macro
			const uint32_t mLine;
	};
}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	// forward declaration so that we can declare friends
	template< typename T > class _lazy_stream_inserter_;

	/**
		A helper function to build templated types for us
		@param aT an object to be wrapped by a lazy stream inserter
		@return a templated lazy stream inserter wrapping the object that needed wrapping
	*/
	template< typename T > inline _lazy_stream_inserter_<T> lazy_stream_inserter ( const T& aT );

	/**
		A class to make life easier with pantheios.
		Although pantheios is lightning fast, it does not use the std c++ style streams. This means that you run into a problem if you want to use a class with a stream style formatter (i.e. most external libraries).
		You could just stream to a std::stringstream and convert that to a std::string, but that would be evaluated before the call to pantheios, and so would be performed even if the log level meant that this code was not going to be used.
		Instead, this class can wrap a reference to the object and only evaluates the streaming function when the output is actually going to be used! Thus, no compromise on performance!
	*/
	template< typename T >
	class _lazy_stream_inserter_
	{

			/**
				A helper function to build templated types for us
				@param aT an object to be wrapped by a lazy stream inserter
			*/
			friend _lazy_stream_inserter_<T> lazy_stream_inserter<T> ( const T& aT );

		private:
			/**
				Constructor
				Private so that all construction is done through the helper function
				@param aT an object to be wrapped by a lazy stream inserter
			*/
			_lazy_stream_inserter_ ( const T& aT );

		public:
			/**
				Destructor
			*/
			virtual ~_lazy_stream_inserter_();

		public:
			/**
				If it has not already been done, evaluate the streaming of the object, and then return the string of characters that the lazy_stream_inserter wants to output
				@return a c-style string of characters that the lazy_stream_inserter wants to output
			*/
			char const* data() const;

			/**
				If it has not already been done, evaluate the streaming of the object, and then return the number of characters that the stream wants to output
				@return the number of characters that the stream wants to output
			*/
			size_t length() const;

		private:
			/**
				Function to perform the stream extraction of the object if it has not been done previously
			*/
			void construct() const;

			/**
				Function to perform the stream extraction of the object if it has not been done previously
			*/
			void construct();

		private:
			//! A const reference to the object which is to be displayed subject to lazy evaluation
			const T&	mT;
			//! The number of characters that the lazy_stream_inserter wants to output
			size_t		mLength;
			//! The c-style string of characters that the lazy_stream_inserter wants to output
			char*		mValue;
	};


	/**
		One of the two functions which is required by pantheios to perform its lightning fast formatting, returning the string of characters that the lazy_stream_inserter wants to output
		@param aLazyInserter aLazyInserter to be evaluated and output
		@return a c-style string of characters that the lazy_stream_inserter wants to output
	*/
	template< typename T > inline char const* c_str_data_a ( const _lazy_stream_inserter_<T>& aLazyInserter );

	/**
		One of the two functions which is required by pantheios to perform its lightning fast formatting, returning the number of characters that the lazy_stream_inserter wants to output
		@param aLazyInserter aLazyInserter to be evaluated and output
		@return the number of characters that the lazy_stream_inserter wants to output
	*/
	template< typename T > inline size_t c_str_len_a ( const _lazy_stream_inserter_<T>& aLazyInserter );



}
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

/**
	A streaming operator so that lazy_stream_inserters can also be used in streams
	@param aStream a stream to output the time onto
	@param aLazyInserter a LazyInserter to be evaluated and output
	@return a stream for further appending
*/
template< typename T > std::ostream& operator<< ( std::ostream& aStream , const uhal::_lazy_stream_inserter_<T>& aLazyInserter );


#include "TemplateDefinitions/log.hxx"

#endif
