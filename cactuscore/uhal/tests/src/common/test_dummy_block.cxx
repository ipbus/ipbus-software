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

#define N_4B    1
#define N_1kB   1024/4
#define N_1MB   1024*1024/4
#define N_1GB   1024*1024*1024/4


void block_write_read ( size_t N,const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  std::vector<uint32_t> xx;

  for ( size_t i=0; i!= N; ++i )
  {
    xx.push_back ( static_cast<uint32_t> ( rand() ) );
  }

  hw.getNode ( "MEM" ).writeBlock ( xx );
  ValVector< uint32_t > mem = hw.getNode ( "MEM" ).readBlock ( N );
  CACTUS_CHECK ( !mem.valid() );
  CACTUS_CHECK ( mem.size() == N );
  CACTUS_TEST_THROW ( mem.at ( 0 ),uhal::exception::NonValidatedMemory );
  CACTUS_TEST ( hw.dispatch() );
  CACTUS_CHECK ( mem.valid() );
  CACTUS_CHECK ( mem.size() == N );
  bool correct_block_write_read = true;
  ValVector< uint32_t >::const_iterator i=mem.begin();
  std::vector< uint32_t >::const_iterator j=xx.begin();

  for ( ValVector< uint32_t >::const_iterator i ( mem.begin() ); i!=mem.end(); ++i , ++j )
  {
    correct_block_write_read = correct_block_write_read && ( *i == *j );
  }

  CACTUS_CHECK ( correct_block_write_read );
}

void fifo_write_read ( size_t N,const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  {
    // Scope the large source vector so that the memory is freed up after the call to write. The data is safe, since it is copied into the send buffers.
    std::vector<uint32_t> xx;

    if ( N == N_1GB )
    {
      std::cout << "Filling 1GB test vector..." << std::endl;
    }

    try
    {
      xx.reserve ( N );

      for ( size_t i=0; i!= N; ++i )
      {
        xx.push_back ( static_cast<uint32_t> ( rand() ) );
      }
    }
    catch ( ... )
    {
      std::cout << "Filling test vector failed. This is not as a uHAL error, so test will be considered to have passed." << std::endl;
      return;
    }

    if ( N == N_1GB )
    {
      std::cout << "Filling test vector done." << std::endl;
    }

    if ( N == N_1GB )
    {
      std::cout << "Doing 1GB block write" << std::endl;
    }

    hw.getNode ( "FIFO" ).writeBlock ( xx );

    if ( N == N_1GB )
    {
      std::cout << "1GB block write done" << std::endl;
    }
  }

  if ( N == N_1GB )
  {
    std::cout << "Doing 1GB block read" << std::endl;
  }

  ValVector< uint32_t > mem = hw.getNode ( "FIFO" ).readBlock ( N );

  if ( N == N_1GB )
  {
    std::cout << "1GB block read done" << std::endl;
  }

  CACTUS_CHECK ( !mem.valid() );
  CACTUS_CHECK ( mem.size() == N );
  CACTUS_TEST_THROW ( mem.at ( 0 ),uhal::exception::NonValidatedMemory );
  CACTUS_TEST ( hw.dispatch() );
  CACTUS_CHECK ( mem.valid() );
  CACTUS_CHECK ( mem.size() == N );
  //The FIFO implementation on the dummy HW is a single memory location so there is not much to check
}

void block_transfer_too_big ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  {
    // Scope the large source vector so that the memory is freed up after the call to write. The data is safe, since it is copied into the send buffers.
    std::vector<uint32_t> xx;
    std::cout << "Filling 400MB test vector..." << std::endl;

    try
    {
      xx = std::vector<uint32_t> ( 400 * N_1MB , 0x00000000 );
    }
    catch ( ... )
    {
      std::cout << "Filling test vector failed. This is not as a uHAL error, so test will be considered to have passed." << std::endl;
      return;
    }

    std::cout << "Filling test vector done." << std::endl;
    CACTUS_TEST_THROW ( hw.getNode ( "LARGE_MEM" ).writeBlock ( xx ) , uhal::exception::BulkTransferRequestedTooLarge );
  }
  CACTUS_TEST_THROW ( ValVector< uint32_t > mem = hw.getNode ( "LARGE_MEM" ).readBlock ( 400 * N_1MB ) , uhal::exception::BulkTransferRequestedTooLarge );
}

void block_bigger_than_size_attribute ( const std::string& connection, const std::string& id )
{
  ConnectionManager manager ( connection );
  HwInterface hw=manager.getDevice ( id );
  {
    // Scope the large source vector so that the memory is freed up after the call to write. The data is safe, since it is copied into the send buffers.
    std::vector<uint32_t> xx;
    std::cout << "Filling 400MB test vector..." << std::endl;

    try
    {
      xx = std::vector<uint32_t> ( 400 * N_1MB , 0x00000000 );
    }
    catch ( ... )
    {
      std::cout << "Filling test vector failed. This is not as a uHAL error, so test will be considered to have passed." << std::endl;
      return;
    }

    std::cout << "Filling test vector done." << std::endl;
    CACTUS_TEST_THROW ( hw.getNode ( "SMALL_MEM" ).writeBlock ( xx ) , uhal::exception::BulkTransferRequestedTooLarge );
  }
  CACTUS_TEST_THROW ( ValVector< uint32_t > mem = hw.getNode ( "SMALL_MEM" ).readBlock ( 400 * N_1MB ) , uhal::exception::BulkTransferRequestedTooLarge );
}

int main ( int argc,char* argv[] )
{
  std::map<std::string,std::string> params = tests::default_arg_parsing ( argc,argv );
  std::string connection_file = params["connection_file"];
  std::string device_id = params["device_id"];
  //Memory
  CACTUS_TEST ( block_write_read ( N_4B,connection_file,device_id ) );
  CACTUS_TEST ( block_write_read ( N_1kB,connection_file,device_id ) );
  CACTUS_TEST ( block_write_read ( N_1MB,connection_file,device_id ) );
  //FIFO
  CACTUS_TEST ( fifo_write_read ( N_4B,connection_file,device_id ) );
  CACTUS_TEST ( fifo_write_read ( N_1kB,connection_file,device_id ) );
  CACTUS_TEST ( fifo_write_read ( N_1MB,connection_file,device_id ) );
  CACTUS_TEST ( fifo_write_read ( N_1GB,connection_file,device_id ) );
  //Block to big
  //CACTUS_TEST_THROW(block_bigger_than_size_attribute(connection_file,device_id ),uhal::exception::BulkTransferRequestedTooLarge);
  block_transfer_too_big ( connection_file,device_id );
  block_bigger_than_size_attribute ( connection_file,device_id );
  CACTUS_TEST_RESULT();
}
