/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_exception_hpp_
#define _uhal_exception_hpp_

#include <exception>
#include <string>

#include "uhal/log/log.hpp"

namespace uhal
{

  //! An abstract base exception class providing an interface to a throw/rethrow mechanism which will allow us to catch the base type and rethrow the derived type
  class exception : public std::exception
  {
    public:
      /**
      	Destructor
      */
      virtual ~exception() throw();

      /**
      	Function which returns the error message associated with an exception
      	If no error message has previously been defined, then it makes the typename readable (where appropriate) and returns this instead.
      	@return the error message associated with an exception
      */
      virtual const char* what() const throw();

    protected:
      /**
      	Constructor
      	@param aExc a standard string to be used as a message
      */
      exception ( const std::string& aExc = "" );

    private:

      //! The message given to the exception at the time of construction
      std::string mMessage;

  };



}


#endif
