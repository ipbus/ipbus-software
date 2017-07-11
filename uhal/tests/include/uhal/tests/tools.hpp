/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

---------------------------------------------------------------------------
*/

#ifndef _uhal_tests_tools_hpp_
#define _uhal_tests_tools_hpp_

#include <stdint.h>
#include <iostream>
#include <map>
#include <string>
#include <exception>
#include <sys/time.h>

namespace uhal
{
  namespace tests
  {

    //!timeval difference in micro seconds
    long usdiff ( const timeval& end, const timeval& start );

    //!Return the first argument
    std::map<std::string,std::string> default_arg_parsing ( int argc,char* argv[] );

    uint32_t failedTestCount ( 0 );
    uint32_t passedTestCount ( 0 );
  }
}


//!Checks if the condition is fullfilled and it does not throw.
#define CACTUS_CHECK(cond) \
  do {	\
    std::cout << std::dec; \
    std::cerr << std::dec; \
    try {								\
      if (cond) { \
	      std::cout << "CHECK PASSED: " << #cond << std::endl; \
	      uhal::tests::passedTestCount++;				\
      }  else  {							\
		std::cerr << "CHECK FAILED @" << __FILE__ << ":" << __LINE__ << std::endl; \
		uhal::tests::failedTestCount++;				\
      }									\
    } catch(std::exception& e) {						\
      std::cerr << "CHECK FAILED by THROWING @" << __FILE__ << ":" << __LINE__ << " with what() returning:" << e.what() << std::endl; \
      uhal::tests::failedTestCount++;					\
    } catch(...) {							\
      std::cerr << "CHECK FAILED by THROWING @" << __FILE__ << ":" << __LINE__ << " with unknown exception type." << std::endl; \
      uhal::tests::failedTestCount++;					\
    }									\
  } while(0)

//!Test that the expression is executed without exceptions and measures the execution time
#define CACTUS_TEST(expr)				\
  do {						\
    std::cout << std::dec; \
    std::cerr << std::dec; \
    try{          \
      timeval start,end;      \
      gettimeofday ( &start, NULL );    \
      expr;         \
      gettimeofday ( &end, NULL );          \
      std::cout << "TEST COMPLETED in " <<  uhal::tests::usdiff(end,start) << " usec: " << #expr << std::endl; \
    } catch(std::exception& e) {          \
      std::cerr << "TEST FAILED by THROWING @" << __FILE__ << ":" << __LINE__ << " with what() returning:" << e.what() << std::endl; \
      uhal::tests::failedTestCount++;         \
    } catch(...) {              \
      std::cerr << "TEST FAILED by THROWING @" << __FILE__ << ":" << __LINE__ << " with unknown exception type." << std::endl; \
      uhal::tests::failedTestCount++;         \
    }  \
  } while(0)



#define CACTUS_TEST_NOTHROW(expr)				\
  do {						\
    std::cout << std::dec; \
    std::cerr << std::dec; \
    try{					\
      expr;					\
      std::cout << "TEST_NOTHROW PASSED: "  << #expr << std::endl; \
      uhal::tests::passedTestCount++;					\
    } catch(std::exception& e) {					\
      std::cerr << "TEST_NOTHROW FAILED by THROWING @" << __FILE__ << ":" << __LINE__ << " with what() returning:" << e.what() << ": " << std::endl; \
      uhal::tests::failedTestCount++;					\
    } catch(...) {							\
      std::cerr << "TEST_NOTHROW FAILED by THROWING @" << __FILE__ << ":" << __LINE__ << " with unknown exception type." << ": " << std::endl; \
      uhal::tests::failedTestCount++;					\
    }									\
  } while(0)

//!Test that the expression throws a specific signature
#define CACTUS_TEST_THROW(expr,signature)		\
  do {						\
    std::cout << std::dec; \
    std::cerr << std::dec; \
    try{					\
      expr;								\
      std::cerr << "TEST_THROW FAILED by NOT THROWING @" << __FILE__ << ":" << __LINE__ << std::endl; \
      uhal::tests::failedTestCount++;					\
    } catch(signature& e) {					\
      std::cout << "TEST_THROW PASSED: " << #expr << std::endl; \
      uhal::tests::passedTestCount++;					\
    } catch(std::exception& e) {						\
      std::cerr << "TEST_THROW FAILED by NOT THROWING REQUIRED TYPE '" << #signature << "' @" << __FILE__ << ":" << __LINE__ << " with what() returning:" << e.what() << std::endl; \
      uhal::tests::failedTestCount++;					\
    }	catch(...) {							\
      std::cerr << "TEST_THROW FAILED by NOT THROWING REQUIRED TYPE '" << #signature << "' @" << __FILE__ << ":" << __LINE__ << " with unknown exception type." << std::endl; \
      uhal::tests::failedTestCount++;		\
    }									\
  } while(0)

#endif

#define CACTUS_TEST_RESULT()  \
  do { \
    std::cout << std::dec; \
    std::cerr << std::dec; \
    if( uhal::tests::failedTestCount == 0 ){				\
      std::cout << "TEST PASSED, " << __FILE__ << ", ALL " << uhal::tests::passedTestCount << " TESTS PASSED." << std::endl; \
      return 0; \
    }else{  \
      std::cerr << "TEST FAILED, " << __FILE__ << ", " << uhal::tests::failedTestCount << " TESTS FAILED," << uhal::tests::passedTestCount << " TESTS PASSED." << std::endl; \
      return 1; \
    } \
  } while(0)


