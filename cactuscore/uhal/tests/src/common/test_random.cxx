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

#include <algorithm>
#include <iostream>
#include <vector>

#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

using namespace uhal;

static const uint32_t RegisterOffset = 0x00001000;
static const uint32_t RegisterSpace  = 0x00001000;
static const uint32_t MaxSize        = std::min ( static_cast<uint32_t> ( 10000 ), RegisterSpace );





int main ( int argc, char* argv[] )
{
  //---------------------------------------------------------------------------------------------------------------
  // Commandline options
  //---------------------------------------------------------------------------------------------------------------
  std::string connection_file, device_id;
  boost::posix_time::time_duration runtime;
  boost::program_options::options_description desc ( "Allowed options" );
  desc.add_options()
  ( "help,h", "produce help message" )
  ( "connection_file,c", boost::program_options::value<std::string>()->default_value ( "", "Connection file URI" ) )
  ( "device_id,d", boost::program_options::value<std::string>()->default_value ( "", "Device identifier" ) )
  ( "time,t", boost::program_options::value<int32_t>()->default_value ( -1, "Time period for which to run" ) )
  ( "verbose,v", "Verbose output" )
  ( "very_verbose,V", "Very verbose output" )
  ;
  boost::program_options::variables_map vm;

  try
  {
    boost::program_options::store ( boost::program_options::parse_command_line ( argc, argv, desc ), vm );
    boost::program_options::notify ( vm );
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

  connection_file = vm["connection_file"].as<std::string>();
  device_id = vm["device_id"].as<std::string>();
  int32_t lTime ( vm["time"].as<int32_t>() );

  if ( lTime > 0 )
  {
    runtime = boost::posix_time::seconds ( lTime );
  }
  else
  {
    runtime = boost::posix_time::pos_infin;
  }

  if ( vm.count ( "very_verbose" ) )
  {
    uhal::setLogLevelTo ( Debug() );
  }
  else if ( vm.count ( "verbose" ) )
  {
    uhal::setLogLevelTo ( Notice() );
  }
  else
  {
    uhal::setLogLevelTo ( Warning() );
  }

  //---------------------------------------------------------------------------------------------------------------
  ConnectionManager manager ( connection_file );
  HwInterface hw = manager.getDevice ( device_id );
  ClientInterface* c = &hw.getClient();
  uint32_t lProtocol;
  std::string URI ( c->uri() );
  std::size_t found = URI.find ( "-1.3" );

  if ( found!=std::string::npos )
  {
    lProtocol = 1;
  }
  else
  {
    found = URI.find ( "-2.0" );

    if ( found!=std::string::npos )
    {
      lProtocol = 2;
    }
    else
    {
      log ( Error() , "Cannot deduce protocol from URI " , Quote ( URI ) );
      throw 0;
    }
  }

  std::vector<uint32_t> lRandom;
  lRandom.reserve ( 50000 );

  for ( uint32_t i=0; i!= 50000 ; ++i )
  {
    lRandom.push_back ( rand() );
  }

  std::vector< uint32_t > lRegisters ( RegisterSpace , 0x00000000 );
  uint32_t lType;
  uint32_t lAddress, lAddrIdx;
  uint32_t lSize , lPosition;
  uint32_t lTemp1, lTemp2;
  ValWord< uint32_t > lValWord;
  ValVector< uint32_t > lValVector;
  ValVector< uint32_t >::const_iterator lValIt;
  std::vector<uint32_t> lData;
  std::vector<uint32_t>::iterator lIt1 , lIt2;
  boost::posix_time::ptime start_time ( boost::posix_time::second_clock::local_time() );

  try
  {
    while ( boost::posix_time::second_clock::local_time() - start_time < runtime )
    {
      lType = ( rand() % 6 );
      lAddrIdx = ( rand() % RegisterSpace );
      lAddress = RegisterOffset + lAddrIdx;

      switch ( lType )
      {
        case 0:
          //ni_read
          lSize = ( rand() % MaxSize ) + 1;
          log ( Notice() , "Non-Incrementing Read, depth ", Integer ( lSize ), " @ ", Integer ( lAddress, IntFmt<hex,fixed>() ) );
          lValVector = c->readBlock ( lAddress, lSize, defs::NON_INCREMENTAL );
          c->dispatch();
          lIt1 = lRegisters.begin() + lAddrIdx;
          lValIt = lValVector.begin();

          for ( ; lValIt!=lValVector.end(); ++lValIt )
          {
            if ( *lValIt != *lIt1 )
            {
              log ( Error() , "Data expectation failure: Expected " , Integer ( *lIt1, IntFmt<hex,fixed>() ), ", Found " , Integer ( *lValIt, IntFmt<hex,fixed>() ) );
              throw 0;
            }
          }

          break;
        case 1:
          //read
          lSize = ( rand() % MaxSize ) +1;

          if ( lAddrIdx + lSize >= RegisterSpace )
          {
            lAddrIdx = RegisterSpace - lSize;
            lAddress = RegisterOffset + lAddrIdx;
          }

          log ( Notice(), "Incrementing Read, depth ", Integer ( lSize ), " @ ", Integer ( lAddress, IntFmt<hex,fixed>() ) );
          lValVector = c->readBlock ( lAddress, lSize, defs::INCREMENTAL );
          c->dispatch();
          lIt1 = lRegisters.begin() + lAddrIdx;
          lValIt = lValVector.begin();

          for ( ; lValIt!=lValVector.end(); ++lValIt , ++lIt1 )
          {
            if ( *lValIt != *lIt1 )
            {
              log ( Error() , "Data expectation failure: Expected " , Integer ( *lIt1, IntFmt<hex,fixed>() ), ", Found " , Integer ( *lValIt, IntFmt<hex,fixed>() ) );
              throw 0;
            }
          }

          break;
        case 2:
          //ni_write
          lSize = ( rand() % MaxSize ) +1;
          log ( Notice() , "Non-Incrementing Write, depth ", Integer ( lSize ), " @ ", Integer ( lAddress, IntFmt<hex,fixed>() ) );
          lPosition = ( rand() % 40000 );
          lIt1 = lRandom.begin() +lPosition ;
          lIt2 = lIt1 + lSize;
          lData.assign ( lIt1 , lIt2 );
          c->writeBlock ( lAddress, lData , defs::NON_INCREMENTAL );
          c->dispatch();
          lIt1 = lRegisters.begin() + lAddrIdx;
          lIt2 = lData.begin();

          for ( ; lIt2!=lData.end(); ++lIt2 )
          {
            *lIt1 = *lIt2;
          }

          break;
        case 3:
          //write
          lSize = ( rand() % MaxSize ) +1;
          lPosition = ( rand() % 40000 );

          if ( lAddrIdx + lSize >= RegisterSpace )
          {
            lAddrIdx = RegisterSpace - lSize;
            lAddress = RegisterOffset + lAddrIdx;
          }

          log ( Notice() , "Incrementing Write, depth ", Integer ( lSize ), " @ ", Integer ( lAddress, IntFmt<hex,fixed>() ) );
          lIt1 = lRandom.begin() +lPosition ;
          lIt2 = lIt1 + lSize;
          lData.assign ( lIt1 , lIt2 );
          c->writeBlock ( lAddress, lData , defs::INCREMENTAL );
          c->dispatch();
          lIt1 = lRegisters.begin() + lAddrIdx;
          lIt2 = lData.begin();

          for ( ; lIt2!=lData.end(); ++lIt2 , ++lIt1 )
          {
            *lIt1 = *lIt2;
          }

          break;
        case 4:
          //rmw_bits
          log ( Notice() , "Read-Modify-Write bits @ ", Integer ( lAddress, IntFmt<hex,fixed>() ) );
          lTemp1 = rand();
          lTemp2 = rand();
          lValWord = c->rmw_bits ( lAddress, lTemp1 , lTemp2 );
          c->dispatch();
          lIt1 = lRegisters.begin() + lAddrIdx;

          if ( lProtocol == 1 )
          {
            *lIt1 &= lTemp1;
            *lIt1 |= lTemp2;
          }

          if ( lValWord.value() != *lIt1 )
          {
            log ( Error() , "Data expectation failure: Expected " , Integer ( *lIt1, IntFmt<hex,fixed>() ), ", Found " , Integer ( lValWord.value(), IntFmt<hex,fixed>() ) );
            throw 0;
          }

          if ( lProtocol == 2 )
          {
            *lIt1 &= lTemp1;
            *lIt1 |= lTemp2;
          }

          break;
        case 5:
          //rmw_sum
          log ( Notice() , "Read-Modify-Write sum @ ", Integer ( lAddress, IntFmt<hex,fixed>() ) );
          lTemp1 = rand();
          lValWord = c->rmw_sum ( lAddress, lTemp1 );
          c->dispatch();
          lIt1 = lRegisters.begin() + lAddrIdx;

          if ( lProtocol == 1 )
          {
            *lIt1 += lTemp1;
          }

          if ( lValWord.value() != *lIt1 )
          {
            log ( Error() , "Data expectation failure: Expected " , Integer ( *lIt1, IntFmt<hex,fixed>() ), ", Found " , Integer ( lValWord.value(), IntFmt<hex,fixed>() ) );
            throw 0;
          }

          if ( lProtocol == 2 )
          {
            *lIt1 += lTemp1;
          }

          break;
      }
    }

    log ( Warning() , "Exited cleanly. Test Successful." );
    return 0;
  }
  catch ( ... )
  {
    log ( Warning() , "Exception caught. Test Unsuccessful." );
    return 0;
  }
}

