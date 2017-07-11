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

#include "uhal/uhal.hpp"

#include "uhal/tests/tools.hpp"

#include <vector>
#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;


int main ( int argc,char* argv[] )
{
 
  std::map<std::string,std::string> params( tests::default_arg_parsing ( argc,argv ) );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];

  if( connection_file.size() == 0 )
  {
    std::cerr << "Need to specify a connection_file with the --connection_file or -c flag" << std::endl;
    return 1;
  }

  if( device_id.size() == 0 )
  {
    std::cerr << "Need to specify a device_id with the --device_id or -d flag" << std::endl;
    return 1;
  }

  std::cout << "Using Connection File: " << connection_file << std::endl;
  std::cout << "Using Device Id: " << device_id << std::endl;

  try
  {
    ConnectionManager manager ( connection_file );
    HwInterface hw=manager.getDevice ( device_id );

    // --------------------------------------------------------------------------------
    // Block write and read 
    uint32_t lReg = static_cast<uint32_t> ( rand() );
    hw.getNode ( "REG" ).write ( lReg );
    ValWord< uint32_t > mem1 = hw.getNode ( "REG" ).read();

    // Doing mem1.valid() should return false
    // Accessing data should throw exception

    hw.dispatch();

    // Doing mem1.valid() should return true
    // Accessing data should be fine

    std::cout << (( mem1.value() == lReg )?"SUCCESS":"FAILURE") << std::endl;
    // --------------------------------------------------------------------------------

    // --------------------------------------------------------------------------------
    // Block write and read 
    std::vector<uint32_t> lMem;
    lMem.reserve ( 1024 );
    for ( size_t i=0; i!= 1024; ++i )
    {
      lMem.push_back ( static_cast<uint32_t> ( rand() ) );
    }

    hw.getNode ( "LARGE_MEM" ).writeBlock ( lMem );
    ValVector< uint32_t > mem2 = hw.getNode ( "LARGE_MEM" ).readBlock ( 1024 );

    // Doing mem2.valid() should return false
    // Accessing data should throw exception

    hw.dispatch();

    // Doing mem2.valid() should return true
    // Accessing data should be fine

    std::vector< uint32_t >::const_iterator j=lMem.begin();
    ValVector< uint32_t >::const_iterator i ( mem2.begin() );

    for ( ; i!=mem2.end(); ++i , ++j )
    {
      std::cout << (( *i == *j )?"SUCCESS":"FAILURE") << std::endl;
    }


    // --------------------------------------------------------------------------------
  }catch( const std::exception& aExc ){
    std::cout << aExc.what() << std::endl;
  }catch( const std::exception* aExc ){
    std::cout << aExc->what() << std::endl;
  }

}
