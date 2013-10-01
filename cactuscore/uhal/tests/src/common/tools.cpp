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

#include "uhal/tests/tools.hpp"

#include "uhal/log/log.hpp"

#include <boost/program_options.hpp>

namespace po = boost::program_options;

long uhal::tests::usdiff ( const timeval& end, const timeval& start )
{
  long usec ( 0 );
  usec= ( end.tv_sec-start.tv_sec ) *1000000;
  usec+= ( end.tv_usec-start.tv_usec );
  return usec;
}

std::map<std::string,std::string> uhal::tests::default_arg_parsing ( int argc,char* argv[] )
{
  // Declare the supported options.
  po::options_description desc ( "Allowed options" );
  desc.add_options()
  ( "help,h", "produce help message" )
  ( "connection_file,c", po::value<std::string>()->default_value ( "", "Connection file URI" ) )
  ( "device_id,d", po::value<std::string>()->default_value ( "", "Device identifier" ) )
  ( "verbose,v", "Verbose output" )
  ( "very_verbose,V", "Very verbose output" )
  ;
  po::variables_map vm;

  try
  {
    po::store ( po::parse_command_line ( argc, argv, desc ), vm );
    po::notify ( vm );
  }
  catch ( std::exception& e )
  {
    std::cerr << "ERROR: " << e.what() << std::endl << std::endl;
    std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
    std::cout << desc << std::endl;
    exit ( 1 );
  }

  if ( vm.count ( "help" ) )
  {
    std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
    std::cout << desc << std::endl;
    exit ( 0 );
  }

  std::map<std::string,std::string> result;
  result["connection_file"] = vm["connection_file"].as<std::string>();
  result["device_id"] = vm["device_id"].as<std::string>();

  if ( vm.count ( "very_verbose" ) )
  {
    uhal::setLogLevelTo ( Debug() );
    result["very_verbose"] = "true";
    result["verbose"] = "true";
  }
  else if ( vm.count ( "verbose" ) )
  {
    uhal::setLogLevelTo ( Notice() );
    result["very_verbose"] = "false";
    result["verbose"] = "true";
  }
  else
  {
    uhal::setLogLevelTo ( Fatal() );
    result["very_verbose"] = "false";
    result["verbose"] = "false";
  }

  return result;
}

