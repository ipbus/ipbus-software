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

using namespace uhal;

void test_docu_addr_table_examples ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  // This line is majority of the test (i.e. load the addr table without exception).
  HwInterface hw=manager.getDevice ( id );
  // ***** "Single Register Address Table" example *****
  CACTUS_TEST_NOTHROW
  (
    ValWord< uint32_t > reg = hw.getNode ( "A" ).read();
    hw.dispatch();
  );
  // ***** "Single Register on a Hierarchical Address Table" *****
  CACTUS_TEST_NOTHROW
  (
    //This is equivalent to getNode("B.A")
    ValWord< uint32_t > reg = hw.getNode ( "B" ).getNode ( "A" ).read();
    hw.dispatch();
  );
  // ***** "Multiple Modules with Identical Structure" *****
  CACTUS_TEST_NOTHROW
  (
    ValWord< uint32_t > reg = hw.getNode ( "D1.A2" ).read();
    hw.dispatch();
  );
  // ***** "Read and Write Blocks of Memory and FIFOs" *****
  CACTUS_TEST_NOTHROW
  (
    //read
    ValVector< uint32_t > mem = hw.getNode ( "F.A3" ).readBlock ( 16 );
    ValVector< uint32_t > fifo = hw.getNode ( "F.A6" ).readBlock ( 16 );
    //write
    std::vector<uint32_t> x;

    //fill x...
    for ( unsigned int iFill = 0 ; iFill < 16 ; ++iFill )
{
  x.push_back ( iFill );
  }
  hw.getNode ( "F.A4" ).writeBlock ( x );
  hw.getNode ( "F.A7" ).writeBlock ( x );
  hw.dispatch();
  );
}

int main ( int argc, char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  CACTUS_TEST ( test_docu_addr_table_examples ( connection_file, device_id ) );
  CACTUS_TEST_RESULT();
}

