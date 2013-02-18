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


      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

---------------------------------------------------------------------------
*/

/**
	@file
	@author Andrew W. Rose
	@date 2013
*/

#ifndef DummyHardware_hpp
#define DummyHardware_hpp

#include "uhal/IPbusInspector.hpp"
#include <boost/program_options.hpp>
#include <boost/lexical_cast.hpp>

// Using the uhal namespace
namespace uhal
{
  static const uint32_t ADDRESSMASK = 0x000FFFFF;


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  class DummyHardware : public HostToTargetInspector< IPbus_major , IPbus_minor >
  {
      typedef HostToTargetInspector< IPbus_major , IPbus_minor > base_type;

    public:
      DummyHardware ( const uint32_t& aReplyDelay ) : HostToTargetInspector< IPbus_major , IPbus_minor >() ,
        mMemory ( ADDRESSMASK+1 , 0x00000000 ),
        mReplyDelay ( aReplyDelay ),
        mReceive ( 500 , 0x00000000 ),
        mReply ( 500 , 0x00000000 )
      {}

      virtual ~DummyHardware() {}

      virtual void run() = 0;

      void AnalyzeReceivedAndCreateReply ( const uint32_t& aByteCount )
      {
        logging();
        mReply.clear();
        std::vector<uint32_t>::const_iterator lBegin ( mReceive.begin() );
        std::vector<uint32_t>::const_iterator lEnd ( mReceive.begin() + ( aByteCount>>2 ) );
        base_type::analyze ( lBegin , lEnd );

        if ( mReplyDelay )
        {
          log ( Info() , "Sleeping for " , Integer ( mReplyDelay ) , "s" );
          sleep ( mReplyDelay );
          mReplyDelay = 0;
          log ( Info() , "Now replying " );
        }
      }

    private:
      void bot()
      {
        if ( LoggingIncludes ( Debug() ) )
        {
          base_type::bot();
        }

        mReply.push_back ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 0 , base_type::mTransactionId ) );
      }

      void ni_read ( const uint32_t& aAddress )
      {
        if ( LoggingIncludes ( Debug() ) )
        {
          base_type::ni_read ( aAddress );
        }

        uint32_t lAddress ( aAddress );
        mReply.push_back ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , base_type::mWordCounter , base_type::mTransactionId ) );

        for ( ; base_type::mWordCounter!=0 ; --base_type::mWordCounter )
        {
          mReply.push_back ( mMemory.at ( lAddress & ADDRESSMASK ) );
        }
      }

      void read ( const uint32_t& aAddress )
      {
        if ( LoggingIncludes ( Debug() ) )
        {
          base_type::read ( aAddress );
        }

        uint32_t lAddress ( aAddress );
        mReply.push_back ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , base_type::mWordCounter , base_type::mTransactionId ) );

        for ( ; base_type::mWordCounter!=0 ; --base_type::mWordCounter )
        {
          mReply.push_back ( mMemory.at ( lAddress++ & ADDRESSMASK ) );
        }
      }

      void ni_write ( const uint32_t& aAddress , std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd )
      {
        if ( LoggingIncludes ( Debug() ) )
        {
          std::vector<uint32_t>::const_iterator lIt ( aIt );
          base_type::ni_write ( aAddress , lIt , aEnd );
        }

        uint32_t lAddress ( aAddress );

        while ( aIt != aEnd )
        {
          mMemory.at ( lAddress & ADDRESSMASK ) = *aIt++;
        }

        mReply.push_back ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 0 , base_type::mTransactionId ) );
      }

      void write ( const uint32_t& aAddress , std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd )
      {
        if ( LoggingIncludes ( Debug() ) )
        {
          std::vector<uint32_t>::const_iterator lIt ( aIt );
          base_type::write ( aAddress , lIt , aEnd );
        }

        uint32_t lAddress ( aAddress );

        while ( aIt != aEnd )
        {
          mMemory.at ( lAddress++ & ADDRESSMASK ) = *aIt++;
        }

        mReply.push_back ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 0 , base_type::mTransactionId ) );
      }

      void rmw_sum ( const uint32_t& aAddress , const uint32_t& aAddend )
      {
        if ( LoggingIncludes ( Debug() ) )
        {
          base_type::rmw_sum ( aAddress , aAddend );
        }

        uint32_t lAddress ( aAddress );
        mReply.push_back ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 1 , base_type::mTransactionId ) );

        if ( IPbus_major == 1 )
        {
          //IPbus 1.x returns modified value
          mMemory.at ( lAddress & ADDRESSMASK ) += aAddend;
          mReply.push_back ( mMemory.at ( lAddress & ADDRESSMASK ) );
        }
        else
        {
          //IPbus 2.x returns pre-modified value
          mReply.push_back ( mMemory.at ( lAddress & ADDRESSMASK ) );
          mMemory.at ( lAddress & ADDRESSMASK ) += aAddend;
        }
      }

      void rmw_bits ( const uint32_t& aAddress , const uint32_t& aAndTerm , const uint32_t& aOrTerm )
      {
        if ( LoggingIncludes ( Debug() ) )
        {
          base_type::rmw_bits ( aAddress , aAndTerm ,  aOrTerm );
        }

        uint32_t lAddress ( aAddress );
        mReply.push_back ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 1 , base_type::mTransactionId ) );

        if ( IPbus_major == 1 )
        {
          //IPbus 1.x returns modified value
          mMemory.at ( lAddress & ADDRESSMASK ) &= aAndTerm;
          mMemory.at ( lAddress & ADDRESSMASK ) |= aOrTerm;
          mReply.push_back ( mMemory.at ( lAddress & ADDRESSMASK ) );
        }
        else
        {
          //IPbus 2.x returns pre-modified value
          mReply.push_back ( mMemory.at ( lAddress & ADDRESSMASK ) );
          mMemory.at ( lAddress & ADDRESSMASK ) &= aAndTerm;
          mMemory.at ( lAddress & ADDRESSMASK ) |= aOrTerm;
        }
      }

      void unknown_type()
      {
        logging();
        log ( Error() , Integer ( base_type::mHeader, IntFmt<hex,fixed>() ) , " is an unknown IPbus header. Throwing." );
        throw 0;
      }


      void packet_header ( const uint32_t& aPacketHeader )
      {
        if ( LoggingIncludes ( Debug() ) )
        {
          base_type::packet_header ( aPacketHeader );
        }

        mReply.push_back ( aPacketHeader );
      }

    private:
      std::vector< uint32_t > mMemory;
      uint32_t mReplyDelay;

    protected:
      std::vector< uint32_t > mReceive;
      std::vector< uint32_t > mReply;
  };







  struct CommandLineOptions
  {
    uint32_t delay;
    uint16_t port;
    uint32_t version;
  };

  CommandLineOptions ParseCommandLineOptions ( int argc,char* argv[] )
  {
    // Declare the supported options.
    boost::program_options::options_description desc ( "Allowed options" );
    desc.add_options()
    ( "help,h", "Produce this help message" )
    ( "delay,d", boost::program_options::value<uint32_t>()->default_value ( 0 ) , "Reply delay for first packet (in seconds) - optional" )
    ( "port,p", boost::program_options::value<uint16_t>() , "Port number to listen on - required" )
    ( "version,v", boost::program_options::value<uint32_t>() , "IPbus Major version (1 or 2) - required" )
    ( "verbose,V", "Produce verbose output" )
    ;
    boost::program_options::variables_map vm;

    try
    {
      boost::program_options::store ( boost::program_options::parse_command_line ( argc, argv, desc ), vm );
      boost::program_options::notify ( vm );

      if ( vm.count ( "help" ) )
      {
        std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
        std::cout << desc << std::endl;
        exit ( 0 );
      }

      CommandLineOptions lResult;
      lResult.delay = vm["delay"].as<uint32_t>();
      lResult.port = vm["port"].as<uint16_t>();
      lResult.version = vm["version"].as<uint32_t>();

      if ( vm.count ( "verbose" ) )
      {
        setLogLevelTo ( Debug() );
      }
      else
      {
        setLogLevelTo ( Notice() );
      }

      return lResult;
    }
    catch ( std::exception& e )
    {
      std::cerr << "ERROR: " << e.what() << std::endl << std::endl;
      std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
      std::cout << desc << std::endl;
      exit ( 1 );
    }
  }


}

#endif


