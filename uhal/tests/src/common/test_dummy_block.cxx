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

#define N_4B     1
#define N_1kB    1024/4
#define N_1MB    1024*1024/4
#define N_10MB   10*1024*1024/4
#define N_200MB  100*1024*1024/4


void block_write_read ( size_t N,const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );

  std::vector<uint32_t> xx;
  xx.reserve ( N );
  for ( size_t i=0; i!= N; ++i )
  {
    xx.push_back ( static_cast<uint32_t> ( rand() ) );
  }

  hw.getNode ( "LARGE_MEM" ).writeBlock ( xx );
  ValVector< uint32_t > mem = hw.getNode ( "LARGE_MEM" ).readBlock ( N );
  CACTUS_CHECK ( !mem.valid() );
  CACTUS_CHECK ( mem.size() == N );
  if ( N > 0 )
  {
    CACTUS_TEST_THROW ( mem.at ( 0 ),uhal::exception::NonValidatedMemory );
  }
  else
  { 
    CACTUS_TEST_THROW ( mem.value(), uhal::exception::NonValidatedMemory );
  }
  CACTUS_TEST ( hw.dispatch() );
  CACTUS_CHECK ( mem.valid() );
  CACTUS_CHECK ( mem.size() == N );

  //This check will fail when DummyHardware::ADDRESS_MASK < N
  if ( N < N_10MB )
  {
    bool correct_block_write_read = true;
    std::vector< uint32_t >::const_iterator j=xx.begin();

    for ( ValVector< uint32_t >::const_iterator i ( mem.begin() ); i!=mem.end(); ++i , ++j )
    {
      correct_block_write_read = correct_block_write_read && ( *i == *j );
    }

    CACTUS_CHECK ( correct_block_write_read );
  }
}

void fifo_write_read ( size_t N,const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  // Scope the large source vector so that the memory is freed up after the call to write. The data is safe, since it is copied into the send buffers.
  std::vector<uint32_t> xx;
  xx.reserve ( N );

  for ( size_t i=0; i!= N; ++i )
  {
    xx.push_back ( static_cast<uint32_t> ( rand() ) );
  }

  hw.getNode ( "FIFO" ).writeBlock ( xx );
  ValVector< uint32_t > mem = hw.getNode ( "FIFO" ).readBlock ( N );
  CACTUS_CHECK ( !mem.valid() );
  CACTUS_CHECK ( mem.size() == N );
  if ( N > 0 )
  {
    CACTUS_TEST_THROW ( mem.at ( 0 ),uhal::exception::NonValidatedMemory );
  }
  else
  {
    CACTUS_TEST_THROW ( mem.value(), uhal::exception::NonValidatedMemory );
  }
  CACTUS_TEST ( hw.dispatch() );
  CACTUS_CHECK ( mem.valid() );
  CACTUS_CHECK ( mem.size() == N );
  //The FIFO implementation on the dummy HW is a single memory location so there is not much to check
}


void block_offset_write_read ( size_t N,const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );

  std::vector<uint32_t> xx,yy;
  xx.reserve ( N );
  yy.reserve ( N );
  for ( size_t i=0; i!= N; ++i )
  {
    xx.push_back ( static_cast<uint32_t> ( rand() ) );
    yy.push_back ( static_cast<uint32_t> ( rand() ) );
  }

  hw.getNode ( "LARGE_MEM" ).writeBlockOffset ( xx , 0 );
  ValVector< uint32_t > mem = hw.getNode ( "LARGE_MEM" ).readBlockOffset ( N , 0 );

  CACTUS_CHECK ( !mem.valid() );
  CACTUS_CHECK ( mem.size() == N );

  if ( N > 0 )
  {
    CACTUS_TEST_THROW ( mem.at ( 0 ),uhal::exception::NonValidatedMemory );
  }
  else
  { 
    CACTUS_TEST_THROW ( mem.value(), uhal::exception::NonValidatedMemory );
  }


  hw.getNode ( "LARGE_MEM" ).writeBlockOffset ( yy , N );
  ValVector< uint32_t > mem2 = hw.getNode ( "LARGE_MEM" ).readBlockOffset ( N , N );

  CACTUS_CHECK ( !mem2.valid() );
  CACTUS_CHECK ( mem2.size() == N );

  if ( N > 0 )
  {
    CACTUS_TEST_THROW ( mem2.at ( 0 ),uhal::exception::NonValidatedMemory );
  }
  else
  { 
    CACTUS_TEST_THROW ( mem2.value(), uhal::exception::NonValidatedMemory );
  }

  CACTUS_TEST ( hw.dispatch() );

  CACTUS_CHECK ( mem.valid() );
  CACTUS_CHECK ( mem.size() == N );

  CACTUS_CHECK ( mem2.valid() );
  CACTUS_CHECK ( mem2.size() == N );


  //This check will fail when DummyHardware::ADDRESS_MASK < N
  if ( N < N_10MB )
  {
    bool correct_block_write_read = true;
    std::vector< uint32_t >::const_iterator j=xx.begin();

    for ( ValVector< uint32_t >::const_iterator i ( mem.begin() ); i!=mem.end(); ++i , ++j )
    {
      correct_block_write_read = correct_block_write_read && ( *i == *j );
    }

    j=yy.begin();

    for ( ValVector< uint32_t >::const_iterator i ( mem2.begin() ); i!=mem2.end(); ++i , ++j )
    {
      correct_block_write_read = correct_block_write_read && ( *i == *j );
    }

    CACTUS_CHECK ( correct_block_write_read );
  }
}


void block_access_type_violations ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  std::vector<uint32_t> xx;

  //We allow the user to call a bulk access of size=1 to a single register
  xx.resize ( N_4B );
  CACTUS_TEST ( hw.getNode ( "REG" ).writeBlock ( xx ) );
  CACTUS_TEST ( ValVector< uint32_t > mem = hw.getNode ( "REG" ).readBlock( N_4B ) );

  //These should all throw
  CACTUS_TEST_THROW ( hw.getNode ( "REG" ).writeBlockOffset ( xx , 0 ) , uhal::exception::BulkTransferOffsetRequestedForSingleRegister );
  CACTUS_TEST_THROW ( ValVector< uint32_t > mem = hw.getNode ( "REG" ).readBlockOffset ( N_1kB , 0 ) , uhal::exception::BulkTransferOffsetRequestedForSingleRegister );

  xx.resize ( N_1kB );
  CACTUS_TEST_THROW ( hw.getNode ( "REG" ).writeBlock ( xx ) , uhal::exception::BulkTransferOnSingleRegister );
  CACTUS_TEST_THROW ( ValVector< uint32_t > mem = hw.getNode ( "REG" ).readBlock( N_1kB ) , uhal::exception::BulkTransferOnSingleRegister );

  CACTUS_TEST_THROW ( hw.getNode ( "FIFO" ).writeBlockOffset ( xx , 1 ) , uhal::exception::BulkTransferOffsetRequestedForFifo );
  CACTUS_TEST_THROW ( ValVector< uint32_t > mem = hw.getNode ( "FIFO" ).readBlockOffset ( N_1kB , 1 ) , uhal::exception::BulkTransferOffsetRequestedForFifo );

}


void block_bigger_than_size_attribute ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  std::vector<uint32_t> xx;
  xx.resize ( N_1MB );
  CACTUS_TEST_THROW ( hw.getNode ( "SMALL_MEM" ).writeBlock ( xx ) , uhal::exception::BulkTransferRequestedTooLarge );
  CACTUS_TEST_THROW ( ValVector< uint32_t > mem = hw.getNode ( "SMALL_MEM" ).readBlock ( N_1MB ) , uhal::exception::BulkTransferRequestedTooLarge );
}


void block_offset_bigger_than_size_attribute ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  std::vector<uint32_t> xx;
  // Size OK but offset too large
  xx.resize ( N_4B );
  CACTUS_TEST_THROW ( hw.getNode ( "SMALL_MEM" ).writeBlockOffset ( xx , 256 ) , uhal::exception::BulkTransferRequestedTooLarge );
  CACTUS_TEST_THROW ( ValVector< uint32_t > mem = hw.getNode ( "SMALL_MEM" ).readBlockOffset ( N_4B , 256 ) , uhal::exception::BulkTransferRequestedTooLarge );
  // Size OK, offset OK, combination too large
  xx.resize ( N_1kB );
  CACTUS_TEST_THROW ( hw.getNode ( "SMALL_MEM" ).writeBlockOffset ( xx , 1 ) , uhal::exception::BulkTransferRequestedTooLarge );
  CACTUS_TEST_THROW ( ValVector< uint32_t > mem = hw.getNode ( "SMALL_MEM" ).readBlockOffset ( N_1kB , 1 ) , uhal::exception::BulkTransferRequestedTooLarge );
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  //Memory
  CACTUS_TEST ( block_write_read ( 0,connection_file,device_id ) );
  CACTUS_TEST ( block_write_read ( N_4B,connection_file,device_id ) );
  CACTUS_TEST ( block_write_read ( N_1kB,connection_file,device_id ) );
  CACTUS_TEST ( block_write_read ( N_1MB,connection_file,device_id ) );
  CACTUS_TEST ( block_write_read ( N_10MB,connection_file,device_id ) );
  //FIFO
  CACTUS_TEST ( fifo_write_read ( 0,connection_file,device_id ) );
  CACTUS_TEST ( fifo_write_read ( N_4B,connection_file,device_id ) );
  CACTUS_TEST ( fifo_write_read ( N_1kB,connection_file,device_id ) );
  CACTUS_TEST ( fifo_write_read ( N_1MB,connection_file,device_id ) );
  CACTUS_TEST ( fifo_write_read ( N_200MB,connection_file,device_id ) );
  //Memory with offset
  CACTUS_TEST ( block_offset_write_read ( 0,connection_file,device_id ) );
  CACTUS_TEST ( block_offset_write_read ( N_4B,connection_file,device_id ) );
  CACTUS_TEST ( block_offset_write_read ( N_1kB,connection_file,device_id ) );
  CACTUS_TEST ( block_offset_write_read ( N_1MB,connection_file,device_id ) );
  CACTUS_TEST ( block_offset_write_read ( N_10MB,connection_file,device_id ) );
  //Block writes on single registers, offsets into FIFOs
  CACTUS_TEST ( block_access_type_violations ( connection_file,device_id ) );
  //Block too big
  CACTUS_TEST ( block_bigger_than_size_attribute ( connection_file,device_id ) );
  //Block offset too big
  CACTUS_TEST ( block_offset_bigger_than_size_attribute ( connection_file,device_id ) );
  CACTUS_TEST_RESULT();
}
