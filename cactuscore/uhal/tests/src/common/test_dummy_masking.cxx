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
#include <ios>
#include <cstdlib>
#include <typeinfo>

using namespace uhal;

void write_read_masked ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  uint32_t x = static_cast<uint32_t> ( rand() );
  hw.getNode ( "REG_LOWER_MASK" ).write ( x && 0xFFFF );
  hw.getNode ( "REG_UPPER_MASK" ).write ( x >> 16 );
  CACTUS_TEST_THROW ( hw.getNode ( "REG_LOWER_MASK" ).write ( 0x1FFFF ),uhal::exception::exception );
  CACTUS_TEST_THROW ( hw.getNode ( "REG_UPPER_MASK" ).write ( 0x1FFFF ),uhal::exception::exception );
  CACTUS_CHECK ( hw.getNode ( "REG_LOWER_MASK" ).getMask() == 0xFFFF );
  CACTUS_CHECK ( hw.getNode ( "REG_UPPER_MASK" ).getMask() == 0xFFFF0000 );
  ValWord<uint32_t> reg_l = hw.getNode ( "REG_LOWER_MASK" ).read();
  ValWord<uint32_t> reg_u = hw.getNode ( "REG_UPPER_MASK" ).read();
  CACTUS_CHECK ( !reg_l.valid() && !reg_u.valid() );
  CACTUS_TEST_THROW ( reg_l.value(),uhal::exception::exception );
  CACTUS_TEST_THROW ( reg_u.value(),uhal::exception::exception );
  CACTUS_TEST ( hw.dispatch() );
  CACTUS_CHECK ( reg_l.value() <= 0xFFFF );
  CACTUS_CHECK ( reg_u.value() <= 0xFFFF );
  CACTUS_CHECK ( reg_l.valid() && reg_u.valid() );
  CACTUS_CHECK ( reg_l.value() == ( x && 0xFFFF ) );
  CACTUS_CHECK ( reg_u.value() == ( x >> 16 ) );
}



int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  CACTUS_TEST ( write_read_masked ( connection_file,device_id ) );
  CACTUS_TEST_RESULT();
}
