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

#define N_1MB    1024*1024/4
#define N_10MB   10*1024*1024/4


int main ( int argc,char* argv[] )
{
  try
  {
    std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
    std::string connection_file = params["connection_file"];
    std::string device_id = params["device_id"];
    ConnectionManager manager ( connection_file );
    HwInterface hw=manager.getDevice ( device_id );
    std::vector<uint32_t> xx;
    xx.reserve ( N_10MB );

    for ( size_t i=0; i!= N_10MB; ++i )
    {
      xx.push_back ( static_cast<uint32_t> ( rand() ) );
    }

    hw.getNode ( "LARGE_MEM" ).writeBlock ( xx );
    ValVector< uint32_t > mem = hw.getNode ( "LARGE_MEM" ).readBlock ( N_10MB );
    hw.dispatch();
  }
  catch ( std::exception& e )
  {
    std::cerr << e.what() << std::endl;
  }
}
