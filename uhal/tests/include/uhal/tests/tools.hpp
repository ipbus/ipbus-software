#ifndef _uhal_tests_tools_hpp_
#define _uhal_tests_tools_hpp_

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
  }
}

//!Checks if the condition is fullfilled and it does not throw.
#define CACTUS_CHECK(cond) \
  do {	\
    try {								\
      if (cond) { \
	std::cout << "CHECK PASSED: " << #cond << std::endl; \
      }  else  {							\
	std::cerr << "CHECK FAILED @" << __FILE__ << ":" << __LINE__ << std::endl; \
      }									\
    } catch(std::exception& e) {						\
      std::cerr << "CHECK FAILED by THROWING " << e.what() << " @" << __FILE__ << ":" << __LINE__ << std::endl; \
    } catch(...) {							\
      std::cerr << "CHECK FAILED by THROWING unknown @" << __FILE__ << ":" << __LINE__ << std::endl; \
    }									\
  } while(0)

//!Test that the expression is executed without exceptions and measures the execution time
#define CACTUS_TEST(expr)				\
  do {						\
    try{					\
      timeval start,end;			\
      gettimeofday ( &start, NULL );		\
      expr;					\
      gettimeofday ( &end, NULL );					\
      std::cout << "TEST PASSED in " << uhal::tests::usdiff(end,start) << " usec: " << #expr << std::endl; \
    } catch(std::exception& e) {					\
      std::cerr << "TEST FAILED by THROWING " << e.what() << " @" << __FILE__ << ":" << __LINE__ << std::endl; \
    } catch(...) {							\
      std::cerr << "TEST FAILED by THROWING unknown @" << __FILE__ << ":" << __LINE__ << std::endl; \
    }									\
  } while(0)

#define CACTUS_TEST_NOTHROW(expr)				\
  do {						\
    try{					\
      expr;					\
      std::cout << "TEST_NOTHROW PASSED: "  << #expr << std::endl; \
    } catch(std::exception& e) {					\
      std::cerr << "TEST_NOTHROW FAILED by THROWING " << e.what() << " @" << __FILE__ << ":" << __LINE__ << ": " << std::endl; \
    } catch(...) {							\
      std::cerr << "TEST_NOTHROW FAILED by THROWING unknown @" << __FILE__ << ":" << __LINE__ << ": " << std::endl; \
    }									\
  } while(0)

//!Test that the expression throws a specific signature
#define CACTUS_TEST_THROW(expr,signature)		\
  do {						\
    try{					\
      expr;								\
      std::cerr << "TEST_THROW FAILED by NOT THROWING @" << __FILE__ << ":" << __LINE__ << std::endl; \
    } catch(signature& e) {					\
      std::cout << "TEST_THROW PASSED: " << #expr << std::endl; \
    } catch(std::exception& e) {						\
      std::cerr << "TEST_THROW FAILED by THROWING " << e.what() << " @" << __FILE__ << ":" << __LINE__ << std::endl; \
    }	catch(...) {							\
      std::cerr << "TEST_THROW FAILED by THROWING unknown @" << __FILE__ << ":" << __LINE__ << std::endl; \
    }									\
  } while(0)

#endif
