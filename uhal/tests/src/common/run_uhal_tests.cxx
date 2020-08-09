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

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

---------------------------------------------------------------------------
*/


/* 
 * File:   run_uhal_tests.cxx
 * Author: Tom Williams
 * Date:   September 2017
 */



#define BOOST_TEST_MODULE uhalTests

// BOOST_TEST_NO_MAIN: Disable auto-generation of main function, in order to define our own, which parses some arguments
#define BOOST_TEST_NO_MAIN


#include <boost/program_options.hpp>
#include <boost/test/unit_test.hpp>
#if BOOST_VERSION >= 105900
  #include <boost/test/tree/traverse.hpp>
  #include <boost/test/tree/visitor.hpp>
#endif

#include "uhal/tests/definitions.hpp"
#include "uhal/tests/fixtures.hpp"
#include "uhal/tests/tools.hpp"
#include "uhal/log/log.hpp"


#ifdef BOOST_TEST_DYN_LINK

// Inspired by https://stackoverflow.com/questions/25385011/getting-all-boost-test-suites-test-cases
struct ListWritingVisitor : boost::unit_test::test_tree_visitor
{
  size_t level;
  size_t maxDepth;

  ListWritingVisitor(const size_t aMaxDepth) :
    level(0),
    maxDepth(aMaxDepth == 0 ? std::numeric_limits<size_t>::max() : aMaxDepth)
  {}

  void visit( boost::unit_test::test_case const& test )
  {
    if ((level+1) <= maxDepth)
      std::cout << std::string(2 * (level > 2 ? level - 2 : 0), ' ') << "  └> " << test.p_name << std::endl;
  }

  bool test_suite_start( boost::unit_test::test_suite const& suite )
  {
    level += 1;

    if (level <= maxDepth)
      std::cout << std::string(2 * (level > 2 ? level - 2 : 0), ' ') << (level > 1 ? "└ " : "") << suite.p_name << std::endl;
    return true;
  }

  void test_suite_finish( boost::unit_test::test_suite const& suite )
  {
    level -= 1;
  }
};


using namespace uhal::tests;

const std::string kOptionHelp = "help";
const std::string kOptionList = "list";
const std::string kOptionConnFile = "connection-file";
const std::string kOptionHwTimeout = "timeout";
const std::string kOptionQuickTest = "quick";
const std::string kOptionVerbose = "verbose";
const std::string kOptionVeryVerbose = "very-verbose";

int BOOST_TEST_CALL_DECL
main( int argc, char* argv[] )
{
  namespace po = boost::program_options;

  size_t lListDepth = 0;

  po::options_description lDesc ( "Allowed options" );
  lDesc.add_options()
  ( (kOptionHelp + ",h").c_str(), "produce help message" )
  ( (kOptionList + ",l").c_str(), po::value<size_t>(&lListDepth)->implicit_value(0), "List all test suites and test cases (max depth can be specified if wanted; if not, all depths are shown)" )
  ( (kOptionConnFile + ",c").c_str(), po::value<std::string>(&AbstractFixture::connectionFileURI), "Connection file URI" )
  ( (kOptionHwTimeout + ",t").c_str(), po::value<size_t>(&AbstractFixture::timeout)->default_value(1000), "Timeout for Client/HwInterface (unit: ms)" )
  ( kOptionQuickTest.c_str(), "Run brief tests (fewer iterations)" )
  ( (kOptionVerbose + ",v").c_str(), "Verbose output" )
  ( (kOptionVeryVerbose + ",V").c_str(), "Very verbose output" )
  ;
  po::variables_map vm;


  std::vector<std::string> lOptionsForBoostUTF;
  try
  {
    po::parsed_options lParsedOptions = po::command_line_parser(argc, argv).options(lDesc).allow_unregistered().run();
    po::store ( lParsedOptions, vm );
    po::notify ( vm );
    lOptionsForBoostUTF = po::collect_unrecognized(lParsedOptions.options, po::include_positional);
  }
  catch ( std::exception& e )
  {
    std::cerr << "ERROR : " << e.what() << std::endl << std::endl;
    std::cout << "Usage : " << argv[0] << " [OPTIONS]" << std::endl;
    std::cout << lDesc << std::endl;
    exit ( 1 );
  }

  if ( vm.count ( kOptionHelp ) )
  {
    std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
    std::cout << lDesc << std::endl;
    exit ( 0 );
  }

  if ( vm.count ( kOptionList ) )
  {
    ListWritingVisitor lVisitor(lListDepth);
    boost::unit_test::traverse_test_tree( boost::unit_test::framework::master_test_suite(), lVisitor);
    exit ( 0 );
  }

  if ( vm.count (kOptionConnFile) == 0)
  {
    std::cerr << " ERROR : Option '" << kOptionConnFile << "' must be specified under normal running mode." << std::endl << std::endl;
    std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
    std::cout << lDesc << std::endl;
    exit ( 1 );
  }
  else if ( AbstractFixture::connectionFileURI.find("file://") != 0 )
    AbstractFixture::connectionFileURI = "file://" + AbstractFixture::connectionFileURI;

  AbstractFixture::quickTest = ( vm.count(kOptionQuickTest) > 0);

  std::cout << "Supplied arguments ..." << std::endl;
  std::cout << "   connection file = " << AbstractFixture::connectionFileURI << std::endl;
  std::cout << "   timeout = " << AbstractFixture::timeout << " ms" << std::endl;
  std::cout << "   " << (AbstractFixture::quickTest ? "quick" : "full") << " test will be run" << std::endl;

  std::cout << "Log level set to ";
  if ( vm.count ( kOptionVeryVerbose ) ) {
    uhal::setLogLevelTo ( uhal::Debug() );
    std::cout << "DEBUG";
  }
  else if ( vm.count ( kOptionVerbose ) ) {
    uhal::setLogLevelTo ( uhal::Notice() );
    std::cout << "NOTICE";
  }
  else {
    uhal::setLogLevelTo ( uhal::Fatal() );
    std::cout << "FATAL";
  }
  std::cout << std::endl << std::endl;


  std::vector<const char*> lArgvForBoostUTF;
  lArgvForBoostUTF.push_back(argv[0]);
  if (lOptionsForBoostUTF.empty())
    std::cout << "N.B. Didn't find any arguments/options to pass to boost UTF" << std::endl;
  else {
    std::cout << "Passing " << lOptionsForBoostUTF.size() << " arguments/options to boost UTF:" << std::endl << "  ";
    for (size_t i=0; i<lOptionsForBoostUTF.size(); i++) {
      std::cout << " " << lOptionsForBoostUTF.at(i);
      lArgvForBoostUTF.push_back(lOptionsForBoostUTF.at(i).c_str());
    }
  }
  std::cout << std::endl << std::endl;

  lArgvForBoostUTF.push_back(0);
  return ::boost::unit_test::unit_test_main( &init_unit_test, lArgvForBoostUTF.size()-1, const_cast<char**>(lArgvForBoostUTF.data()) );
}

#endif