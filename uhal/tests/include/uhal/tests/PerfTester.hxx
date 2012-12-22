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

#ifndef _uhal_tests_PerfTester_hpp_
#define _uhal_tests_PerfTester_hpp_

/*!
* \class PerfTester
* \brief Generate custom IPbus/uHAL tests from the command line
*
*  See the following link for more details on this data type:
*  http://cmsdoc.cern.ch/cms/TRIDAS/horizontal/RUWG/DAQ_IF_guide/DAQ_IF_guide.html#CDF
*
* \author Robert Frazier
* \date July 2012
*/


// C++ headers
#include <vector>
#include <string>

// Linux C++ headers
#include <sys/time.h>

// uHAL headers
#include "uhal/uhal.hpp"

namespace uhal
{
  namespace tests
  {
    class PerfTester
    {
      public:

        /// Constructor - takes no arguments, does nothing.
        PerfTester();

        /// Destructor
        ~PerfTester() {}

        /// Pass in the two command-line parameter variables, this will define the test that then gets run.
        /*!
         * Returns 0 if a test was run, or non-zero if something was wrong with the command-line arguments, etc.
         */
        int run ( int argc, char* argv[] );


      private:

        // PRIVATE TYPEDEFS

        /// An unsigned 32-bit word.
        typedef uint32_t U32;

        /// A vector of unsigned 32-bit words.
        typedef std::vector< U32 > U32Vec;

        /// A Validated Vector of U32
        typedef uhal::ValVector< U32 > U32ValVec;

        /// A vector of strings
        typedef std::vector< std::string > StringVec;

        /// Function pointer typedef to a test function.
        typedef void ( PerfTester::*PtrToTestFunc ) ();

        /// Typedef for a map that links the name of a test to the function that performs it.
        typedef std::map<std::string, PtrToTestFunc> TestFuncMap;

        /// Typedef for a map that links the name of a test to its description
        typedef std::map<std::string, std::string> TestDescMap;

        /// Typedef for a ClientInterface shared_ptr
        typedef boost::shared_ptr<uhal::ClientInterface> ClientPtr;

        /// Typedef for a vector of raw client interfaces
        typedef std::vector< ClientPtr > ClientVec;


        // PRIVATE MEMBER VARIABLES

        TestDescMap m_testDescMap;  ///< Maps test name to test description
        TestFuncMap m_testFuncMap;  ///< Maps test name to test function
        StringVec m_deviceURIs;  ///< Vector of individual connection URI strings.
        ClientVec m_clients;  ///< Vector of low-level uHAL clients.
        std::string m_testName;  ///< Holds the test name
        unsigned m_iterations;  ///< Number of test iterations
        std::string m_baseAddrStr;  ///< Base addr of reg/ram the test will use. Use a string as workaround for hex input via boost::program_options
        boost::uint32_t m_baseAddr;  ///< The m_baseAddrStr as converted into an actual unsigned value.
        boost::uint32_t m_bandwidthTestDepth;  ///< The depth of read/write used in bandwidth tests
        bool m_verbose;  ///< Verbosity true/false flag.
        bool m_perIterationDispatch; ///< Perform a network dispatch every iteration flag.


        // PRIVATE MEMBER FUNCTIONS - Test infrastructure

        /// Outputs the standard help text to screen.
        void outputHelpText ( const std::string& argDescriptions ) const;

        /// Outputs the test names and descriptions to screen.
        void outputTestDescriptionsList() const;

        /// Returns true if the user has entered bad command line arguments
        bool badInput() const;

        /// Outputs the user's choices to screen
        void outputUserChoices() const;

        /// Constructs and sets up the appropriate IPbusClient for use in the test
        void buildClients();

        /// Outputs a standard result set to screen - provide it with the number of seconds the test took.
        void outputStandardResults ( double totalSeconds ) const;

        /// Returns a buffer of random numbers
        U32Vec getRandomBuffer ( unsigned size ) const;

        /// Compares a write buffer with one or more ValVec read responses
        bool buffersEqual ( const U32Vec& writeBuffer, const U32ValVec& readBuffer ) const;


        // PRIVATE MEMBER FUNCTIONS - IPbus test functions that users can run

        void bandwidthRxTest();  ///< Read bandwidth test
        void bandwidthTxTest();  ///< Write bandwidth test
        void validationTest();   ///< Historic basic firmware/software validation test
        void sandbox();          ///< An area for a user-definable test

    }; /* End of class PerfTester */


    /// A very simple timer
    class Timer
    {
      public:

        Timer() :m_start()
        {
          gettimeofday ( &m_start, NULL );
        }

        /// Returns number of elapsed seconds since the timer was instantiated.
        double elapsedSeconds()
        {
          timeval now;
          gettimeofday ( &now, NULL );
          time_t sec = now.tv_sec - m_start.tv_sec;
          suseconds_t usec = now.tv_usec - m_start.tv_usec;
          return static_cast<double> ( sec + usec/1000000. );
        }

      private:
        timeval m_start;

    }; /* End of class Timer */

  }  /* End of namespace tests */

} /* End of namespace uhal */

#endif /* _uhal_tests_PerfTester_hpp_ */
