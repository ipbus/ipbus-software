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

void check_meta_info ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );

  boost::unordered_map<std::string,std::string> lPars;
  boost::unordered_map<std::string,std::string>::iterator iPar;
  boost::unordered_map<std::string,std::string> lFwInfo;
  boost::unordered_map<std::string,std::string>::iterator iFwInfo;

  //REG
  CACTUS_CHECK ( hw.getNode ( "REG" ).getAddress() == 0x000001 );
  CACTUS_CHECK ( hw.getNode ( "REG" ).getId() == "REG" );
  CACTUS_CHECK ( hw.getNode ( "REG" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "REG" ).getSize() == 1 );
  CACTUS_CHECK ( hw.getNode ( "REG" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "REG" ).getMode() == uhal::defs::SINGLE );
  CACTUS_CHECK ( hw.getNode ( "REG" ).getTags() == "test" );
  CACTUS_CHECK ( hw.getNode ( "REG" ).getParameters().empty() );
  //REG_READ_ONLY
  CACTUS_CHECK ( hw.getNode ( "REG_READ_ONLY" ).getAddress() == 0x0002 );
  CACTUS_CHECK ( hw.getNode ( "REG_READ_ONLY" ).getId() == "REG_READ_ONLY" );
  CACTUS_CHECK ( hw.getNode ( "REG_READ_ONLY" ).getPermission() == uhal::defs::READ );
  CACTUS_CHECK ( hw.getNode ( "REG_READ_ONLY" ).getSize() == 1 );
  CACTUS_CHECK ( hw.getNode ( "REG_READ_ONLY" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "REG_READ_ONLY" ).getMode() == uhal::defs::SINGLE );
  CACTUS_CHECK ( hw.getNode ( "REG_READ_ONLY" ).getTags() == "" );
  CACTUS_CHECK ( hw.getNode ( "REG_READ_ONLY" ).getParameters().empty() );
  //REG_WRITE_ONLY
  CACTUS_CHECK ( hw.getNode ( "REG_WRITE_ONLY" ).getAddress() == 0x0003 );
  CACTUS_CHECK ( hw.getNode ( "REG_WRITE_ONLY" ).getId() == "REG_WRITE_ONLY" );
  CACTUS_CHECK ( hw.getNode ( "REG_WRITE_ONLY" ).getPermission() == uhal::defs::WRITE );
  CACTUS_CHECK ( hw.getNode ( "REG_WRITE_ONLY" ).getSize() == 1 );
  CACTUS_CHECK ( hw.getNode ( "REG_WRITE_ONLY" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "REG_WRITE_ONLY" ).getMode() == uhal::defs::SINGLE );
  CACTUS_CHECK ( hw.getNode ( "REG_WRITE_ONLY" ).getTags() == "" );
  CACTUS_CHECK ( hw.getNode ( "REG_WRITE_ONLY" ).getParameters().empty() );
  //REG_UPPER_MASK
  CACTUS_CHECK ( hw.getNode ( "REG_UPPER_MASK" ).getAddress() == 0x0004 );
  CACTUS_CHECK ( hw.getNode ( "REG_UPPER_MASK" ).getId() == "REG_UPPER_MASK" );
  CACTUS_CHECK ( hw.getNode ( "REG_UPPER_MASK" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "REG_UPPER_MASK" ).getSize() == 1 );
  CACTUS_CHECK ( hw.getNode ( "REG_UPPER_MASK" ).getMask() == 0xFFFF0000 );
  CACTUS_CHECK ( hw.getNode ( "REG_UPPER_MASK" ).getMode() == uhal::defs::SINGLE );
  CACTUS_CHECK ( hw.getNode ( "REG_UPPER_MASK" ).getTags() == "" );
  CACTUS_CHECK ( hw.getNode ( "REG_UPPER_MASK" ).getParameters().empty() );
  //REG_LOWER_MASK 
  CACTUS_CHECK ( hw.getNode ( "REG_LOWER_MASK" ).getAddress() == 0x0004 );
  CACTUS_CHECK ( hw.getNode ( "REG_LOWER_MASK" ).getId() == "REG_LOWER_MASK" );
  CACTUS_CHECK ( hw.getNode ( "REG_LOWER_MASK" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "REG_LOWER_MASK" ).getSize() == 1 );
  CACTUS_CHECK ( hw.getNode ( "REG_LOWER_MASK" ).getMask() == 0x0000FFFF );
  CACTUS_CHECK ( hw.getNode ( "REG_LOWER_MASK" ).getMode() == uhal::defs::SINGLE );
  CACTUS_CHECK ( hw.getNode ( "REG_LOWER_MASK" ).getTags() == "" );
  CACTUS_CHECK ( hw.getNode ( "REG_LOWER_MASK" ).getParameters().empty() );
  //REG_WITH_PARS
  CACTUS_CHECK ( hw.getNode ( "REG_PARS" ).getAddress() == 0x0005 );
  CACTUS_CHECK ( hw.getNode ( "REG_PARS" ).getId() == "REG_PARS" );
  CACTUS_CHECK ( hw.getNode ( "REG_PARS" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "REG_PARS" ).getSize() == 1 );
  CACTUS_CHECK ( hw.getNode ( "REG_PARS" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "REG_PARS" ).getMode() == uhal::defs::SINGLE );
  CACTUS_CHECK ( hw.getNode ( "REG_PARS" ).getTags() == "" );
  CACTUS_CHECK ( hw.getNode ( "REG_PARS" ).getParameters().size() == 2 );
  lPars = hw.getNode("REG_PARS").getParameters();
  CACTUS_CHECK ( ( iPar = lPars.find("arg0") ) != lPars.end() && iPar->second == "val100" );
  CACTUS_CHECK ( ( iPar = lPars.find("arg1") ) != lPars.end() && iPar->second == "val101" );
  //SUBSYSTEM1.REG
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.REG" ).getAddress() == 0x210002 );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.REG" ).getId() == "REG" );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.REG" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.REG" ).getSize() == 1 );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.REG" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.REG" ).getMode() == uhal::defs::SINGLE );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.REG" ).getTags() == "test" );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.REG" ).getParameters().empty() );
  //SUBSYSTEM2.REG
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.REG" ).getAddress() == 0x310002 );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.REG" ).getId() == "REG" );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.REG" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.REG" ).getSize() == 1 );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.REG" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.REG" ).getMode() == uhal::defs::SINGLE );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.REG" ).getTags() == "test" );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.REG" ).getParameters().empty() );
  //FIFO
  CACTUS_CHECK ( hw.getNode ( "FIFO" ).getAddress() == 0x0100 );
  CACTUS_CHECK ( hw.getNode ( "FIFO" ).getId() == "FIFO" );
  CACTUS_CHECK ( hw.getNode ( "FIFO" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "FIFO" ).getSize() == 1024*1024*1024/4 );
  CACTUS_CHECK ( hw.getNode ( "FIFO" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "FIFO" ).getMode() == uhal::defs::NON_INCREMENTAL );
  CACTUS_CHECK ( hw.getNode ( "FIFO" ).getTags() == "test" );
  CACTUS_CHECK ( hw.getNode ( "FIFO" ).getParameters().empty() );
  //MEM
  CACTUS_CHECK ( hw.getNode ( "MEM" ).getAddress() == 0x100000 );
  CACTUS_CHECK ( hw.getNode ( "MEM" ).getId() == "MEM" );
  CACTUS_CHECK ( hw.getNode ( "MEM" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "MEM" ).getSize() == 1024*1024/4 );
  CACTUS_CHECK ( hw.getNode ( "MEM" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "MEM" ).getMode() == uhal::defs::INCREMENTAL );
  CACTUS_CHECK ( hw.getNode ( "MEM" ).getTags() == "" );
  CACTUS_CHECK ( hw.getNode ( "MEM" ).getParameters().empty() );
  //SMALL_MEM
  CACTUS_CHECK ( hw.getNode ( "SMALL_MEM" ).getAddress() == 0x400000 );
  CACTUS_CHECK ( hw.getNode ( "SMALL_MEM" ).getId() == "SMALL_MEM" );
  CACTUS_CHECK ( hw.getNode ( "SMALL_MEM" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "SMALL_MEM" ).getSize() == 256 );
  CACTUS_CHECK ( hw.getNode ( "SMALL_MEM" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "SMALL_MEM" ).getMode() == uhal::defs::INCREMENTAL );
  CACTUS_CHECK ( hw.getNode ( "SMALL_MEM" ).getTags() == "" );
  CACTUS_CHECK ( hw.getNode ( "SMALL_MEM" ).getParameters().empty() );
  //LARGE_MEM
  CACTUS_CHECK ( hw.getNode ( "LARGE_MEM" ).getAddress() == 0x500000 );
  CACTUS_CHECK ( hw.getNode ( "LARGE_MEM" ).getId() == "LARGE_MEM" );
  CACTUS_CHECK ( hw.getNode ( "LARGE_MEM" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "LARGE_MEM" ).getSize() == 100*1024*1024/4 );
  CACTUS_CHECK ( hw.getNode ( "LARGE_MEM" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "LARGE_MEM" ).getMode() == uhal::defs::INCREMENTAL );
  CACTUS_CHECK ( hw.getNode ( "LARGE_MEM" ).getTags() == "" );
  CACTUS_CHECK ( hw.getNode ( "LARGE_MEM" ).getParameters().empty() );
  //SUBSYSTEM1.MEM
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.MEM" ).getAddress() == 0x210003 );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.MEM" ).getId() == "MEM" );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.MEM" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.MEM" ).getSize() == 1024*1024/4 );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.MEM" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.MEM" ).getMode() == uhal::defs::INCREMENTAL );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.MEM" ).getTags() == "test" );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM1.MEM" ).getParameters().empty() );
  //SUBSYSTEM2.MEM
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.MEM" ).getAddress() == 0x310003 );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.MEM" ).getId() == "MEM" );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.MEM" ).getPermission() == uhal::defs::READWRITE );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.MEM" ).getSize() == 1024*1024/4 );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.MEM" ).getMask() == uhal::defs::NOMASK );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.MEM" ).getMode() == uhal::defs::INCREMENTAL );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.MEM" ).getTags() == "test" );
  CACTUS_CHECK ( hw.getNode ( "SUBSYSTEM2.MEM" ).getParameters().empty() );
  //SUBSYSTEM1
  lPars = hw.getNode("SUBSYSTEM1").getParameters();
  CACTUS_CHECK( ( iPar = lPars.find( "arg0" ) ) != lPars.end() && iPar->second == "val200" );
  CACTUS_CHECK( ( iPar = lPars.find( "arg1" ) ) != lPars.end() && iPar->second == "val201" );
  CACTUS_CHECK( ( iPar = lPars.find( "arg2" ) ) != lPars.end() && iPar->second == "val202" );
  CACTUS_CHECK( lPars.size() == 3 );
  //SUBSYSTEM2
  lPars = hw.getNode("SUBSYSTEM2").getParameters();
  CACTUS_CHECK( ( iPar = lPars.find( "arg0" ) ) != lPars.end() && iPar->second == "val10000" );
  CACTUS_CHECK( ( iPar = lPars.find( "arg1" ) ) != lPars.end() && iPar->second == "val201" );
  CACTUS_CHECK( ( iPar = lPars.find( "arg2" ) ) != lPars.end() && iPar->second == "val202" );
  CACTUS_CHECK( ( iPar = lPars.find( "arg5" ) ) != lPars.end() && iPar->second == "val10005" );
  CACTUS_CHECK( lPars.size() == 4 );
  //SUBSYSTEM2.SUBMODULE
  lPars = hw.getNode("SUBSYSTEM1.SUBMODULE").getParameters();
  CACTUS_CHECK( ( iPar = lPars.find( "arg0" ) ) != lPars.end() && iPar->second == "val300" );
  CACTUS_CHECK( ( iPar = lPars.find( "arg1" ) ) != lPars.end() && iPar->second == "val301" );
  CACTUS_CHECK( ( iPar = lPars.find( "arg2" ) ) != lPars.end() && iPar->second == "val10302" );
  CACTUS_CHECK( ( iPar = lPars.find( "arg3" ) ) != lPars.end() && iPar->second == "val10303" );
  CACTUS_CHECK( lPars.size() == 4 );
  
  lFwInfo = hw.getNode("IPBUS_ENDPOINT").getFirmwareInfo();
  
  CACTUS_CHECK( ( iFwInfo = lFwInfo.find( "type" ) ) != lFwInfo.end() && iFwInfo->second == "endpoint" );  
  CACTUS_CHECK( ( iFwInfo = lFwInfo.find( "width" ) ) != lFwInfo.end() && iFwInfo->second == "0x10" );
  
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  check_meta_info ( connection_file,device_id );
  CACTUS_TEST_RESULT();
}
