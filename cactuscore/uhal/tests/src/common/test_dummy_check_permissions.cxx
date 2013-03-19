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

#include <iostream>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

void check_permissions ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  CACTUS_TEST_THROW ( hw.getNode ( "REG_READ_ONLY" ).write ( 1 ),uhal::exception::WriteAccessDenied );
  CACTUS_TEST_THROW ( hw.getNode ( "REG_WRITE_ONLY" ).read(),uhal::exception::ReadAccessDenied );
  uint32_t x = static_cast<uint32_t> ( rand() );
  hw.getNode ( "REG_WRITE_ONLY" ).write ( x );
  ValWord< uint32_t > mem = hw.getNode ( "REG_READ_ONLY" ).read();
  hw.dispatch();
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  CACTUS_TEST ( check_permissions ( connection_file,device_id ) );
  CACTUS_TEST_RESULT();
}
