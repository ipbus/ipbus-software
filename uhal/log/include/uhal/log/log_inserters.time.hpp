
#ifndef _log_inserters_time_hpp_
#define _log_inserters_time_hpp_

#include <uhal/log/log_inserter_helper.hpp>

#include <sys/time.h>
#include <time.h>
#include <stdio.h>
#include <stdint.h>

namespace uhal
{

  //! Enumerated type defining the different elements which can be used for formatting a time
  enum time_element
  {
    null,	/**< an unused entry */
    year,	/**< year formatted as four digits e.g. 2012 */
    yr,		/**< year formatted as two digits e.g. 12 */
    strmth,	/**< month formatted as three character string e.g. Jun */
    mth,	/**< month formatted as two digits e.g. 06 */
    day,	/**< day of the month formatted as two digits e.g. 27 */
    hr,		/**< hour of the day formatted as two digits, 24-hour clock e.g. 14 */
    min,	/**< minutes past the hour formatted as two digits e.g. 06 */
    sec,	/**< seconds past the minute formatted as two digits e.g. 09 */
    usec	/**< microseconds past the second formatted as exactly six digits e.g. 012345 */
  };

  //! Forward declaration
  template< typename T , typename FORMAT > class _Time;

  //! A helper struct to allow us to do template specialization if necessary
  template< typename T , typename FORMAT >
  struct TimeFactory
  {
    /**
    	A factory function
    	@param aTime a time object we will wrap as a _Time
    	@return the time wrapper with templated format
    */
    static _Time< T , FORMAT > Construct ( const T& aTime );
  };


  //! A struct whose template parameters represent a time format
  template< time_element T0 , char D0 = ' ' ,
  time_element T1 = null, char D1 = ' ' ,
  time_element T2 = null, char D2 = ' ' ,
  time_element T3 = null, char D3 = ' ' ,
  time_element T4 = null, char D4 = ' ' ,
  time_element T5 = null, char D5 = ' ' ,
  time_element T6 = null > struct TimeFmt {};

  //! Typedef the most commonly used time format (day/month/year hour:minut:second) for convenience
  typedef TimeFmt<day,'/',mth,'/',year,' ',hr,':',min,':',sec> DefaultTimeFmt;

  /**
  	A private class whose template parameters fully encapsulate the formatting and which is used to wrap a reference to an underlying type. A log_inserter function must be available to handle this type.
  	The first parameter (the type) was templated so that we could overload for multiple time types but, so far, only timeval has been implemented
  */
  template< typename T , typename FORMAT >
  class _Time : public RefWrapper< T >
  {
      //! Make the helper struct our friend
      friend class TimeFactory< T , FORMAT >;
      /**
      	Constructor
      	@param aT an object for which we will are wrapping a reference
      */
      _Time ( const T& aT ) : RefWrapper< T > ( aT ) {}
  };


  //! A helper struct to allow us to do template specialization when formatting the time for sending to the log
  template< time_element T >
  struct TimeSpecializationHelper
  {
    /**
    	Format a time element for for sending to the log
    	@param aTm a tm struct containing the time to be formatted (down to the second)
    	@param aUsec the number of microseconds past the second
    */
    static void print ( const tm* aTm , const uint32_t& aUsec );
  };

  /**
  	Helper function which wrap the template uglyness in a pretty package
  	@param aT a time object to be formatted and logged
  	@return a time wrapper object whose template parameters fully encapsulate the default formatting
  */
  template< typename T > _Time< T , DefaultTimeFmt > Time ( const T& aT );

  /**
  	Helper function which wrap the template uglyness in a pretty package
  	@param aT a time object to be formatted and logged
  	@param aFmt a time format object whose template parameters fully encapsulate the desired formatting
  	@return a time wrapper object whose template parameters fully encapsulate the desired formatting
  */
  template< typename T , typename FORMAT > _Time< T , FORMAT > Time ( const T& aT , const FORMAT& aFmt );

  /**
  	A helper function to return the current time
  	@return the current time
  */
  timeval Now();

}

#include <uhal/log/log_inserters.time.hxx>

#endif
