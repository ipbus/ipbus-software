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

#include "uhal/tests/DummyHardware.hpp"


#include <chrono>
#include <deque>
#include <thread>
#include <vector>

#include <arpa/inet.h>

#include "uhal/log/LogLevels.hpp"
#include "uhal/log/log_inserters.integer.hpp"
#include "uhal/log/log.hpp"
#include "uhal/ProtocolIPbus.hpp"


namespace uhal
{
  namespace tests 
  {
  
    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    DummyHardware<IPbus_major, IPbus_minor>::DummyHardware ( const uint32_t& aReplyDelay, const bool& aBigEndianHack ) :
      DummyHardwareInterface( std::chrono::seconds(aReplyDelay) ),
      HostToTargetInspector< IPbus_major , IPbus_minor >(),
      mMemory (),
      mConfigurationSpace(),
      mReceive ( BUFFER_SIZE , 0x00000000 ),
      mReply ( BUFFER_SIZE , 0x00000000 ),
      mReplyHistory ( REPLY_HISTORY_DEPTH , std::make_pair ( 0 , mReply ) ),
      mLastPacketHeader ( 0x200000f0 ),
      mTrafficHistory ( 16, 0x00 ),
      mReceivedControlPacketHeaderHistory ( 4 , 0x00000000 ),
      mSentControlPacketHeaderHistory ( 4 , 0x00000000 ),
      mBigEndianHack ( aBigEndianHack )
    {
      for (size_t i = 0; i < 10; i++)
        mConfigurationSpace.push_back( (uint16_t(getpid()) << 16) | i );
    }

    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    DummyHardware<IPbus_major, IPbus_minor>::~DummyHardware()
    {
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::AnalyzeReceivedAndCreateReply ( const uint32_t& aByteCount )
    {
      //        std::cout << aByteCount << " bytes received" << std::endl;
      if ( IPbus_major == 2 )
      {
        bool is_status_request = ( *mReceive.begin() == 0xF1000020 );
        bool is_resend_request = ( ( *mReceive.begin() & 0xFF0000FF ) == 0xF2000020 );

        if ( mBigEndianHack || is_status_request || is_resend_request )
        {
          for ( std::vector<uint32_t>::iterator lIt ( mReceive.begin() ) ; lIt != mReceive.begin() + ( aByteCount>>2 ) ; ++lIt )
          {
            *lIt = ntohl ( *lIt );
          }
        }
      }

      std::vector<uint32_t>::const_iterator lBegin, lEnd;

      //
      //-----------------------------------------------------------------------------------------------------------------------------------------------------------------
      if ( LoggingIncludes ( Debug() ) )
      {
        log ( Debug() , "\n=============================================== RECEIVED ===============================================" );
        HostToTargetInspector< IPbus_major , IPbus_minor > lHostToTargetDebugger;
        lBegin =  mReceive.begin();
        lEnd = mReceive.begin() + ( aByteCount>>2 );
        lHostToTargetDebugger.analyze ( lBegin , lEnd );
      }

      //-----------------------------------------------------------------------------------------------------------------------------------------------------------------
      //
      lBegin =  mReceive.begin();
      lEnd = mReceive.begin() + ( aByteCount>>2 );

      if ( ! base_type::analyze ( lBegin , lEnd ) ) // Cope with receiving bad headers
      {
        log ( Error() , "Found a bad header" );
        mReply.push_back ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , base_type::mWordCounter , base_type::mTransactionId , ( IPbus_major==1 ? 2 : 1 ) ) );
      }

      if ( ( base_type::mPacketType == 0 ) && ( mReply.size() != 0 ) )
      {
        mReplyHistory.push_back ( std::make_pair ( base_type::mPacketCounter , mReply ) );
        mReplyHistory.pop_front();
      }

      if ( mReplyDelay > std::chrono::microseconds(0) )
      {
        log ( Info() , "Sleeping for " , mReplyDelay.count(), "ms" );
        std::this_thread::sleep_for( mReplyDelay );
        mReplyDelay = std::chrono::microseconds(0);
        log ( Info() , "Now replying " );
      }

      //
      //-----------------------------------------------------------------------------------------------------------------------------------------------------------------
      if ( LoggingIncludes ( Debug() ) && ! mReply.empty() )
      {
        log ( Debug() , "\n=============================================== SENDING ===============================================" );
        TargetToHostInspector< IPbus_major , IPbus_minor > lTargetToHostDebugger;
        lBegin =  mReply.begin();
        lEnd = mReply.end();
        lTargetToHostDebugger.analyze ( lBegin , lEnd );
      }

      //-----------------------------------------------------------------------------------------------------------------------------------------------------------------
      //

      if ( IPbus_major == 2 )
      {
        if ( mBigEndianHack || base_type::mPacketType == 1 )
        {
          for ( auto& x: mReply )
            x = htonl ( x );
        }
      }
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::SetEndpoint( const uint32_t& aAddress , const uint32_t&  aValue )
    {
      if( ! mMemory.size() )
        mMemory.resize( ADDRESSMASK + 1 );
      mMemory.at ( aAddress & ADDRESSMASK ) = aValue;
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    uint32_t DummyHardware<IPbus_major, IPbus_minor>::GetEndpoint( const uint32_t& aAddress )
    {
      if( ! mMemory.size() )
        mMemory.resize( ADDRESSMASK + 1 );
      return mMemory.at ( aAddress & ADDRESSMASK );
    }        


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::bot()
    {
      mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
      mReceivedControlPacketHeaderHistory.pop_front();
      uint32_t lExpected ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 0 , base_type::mTransactionId ) );
      mReply.push_back ( lExpected );
      mSentControlPacketHeaderHistory.push_back ( lExpected );
      mSentControlPacketHeaderHistory.pop_front();
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::ni_read ( const uint32_t& aAddress )
    {
      mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
      mReceivedControlPacketHeaderHistory.pop_front();
      uint32_t lAddress ( aAddress );
      uint32_t lExpected ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , base_type::mWordCounter , base_type::mTransactionId ) );
      mReply.push_back ( lExpected );
      mSentControlPacketHeaderHistory.push_back ( lExpected );
      mSentControlPacketHeaderHistory.pop_front();

      for ( ; base_type::mWordCounter!=0 ; --base_type::mWordCounter )
      {
        mReply.push_back ( GetEndpoint( lAddress  ) );
      }
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::read ( const uint32_t& aAddress )
    {
      mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
      mReceivedControlPacketHeaderHistory.pop_front();
      uint32_t lAddress ( aAddress );
      uint32_t lExpected ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , base_type::mWordCounter , base_type::mTransactionId ) );
      mReply.push_back ( lExpected );
      mSentControlPacketHeaderHistory.push_back ( lExpected );
      mSentControlPacketHeaderHistory.pop_front();

      for ( ; base_type::mWordCounter!=0 ; --base_type::mWordCounter )
      {
        mReply.push_back ( GetEndpoint( lAddress++ ) );
      }
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::readConfigurationSpace ( const uint32_t& aAddress )
    {
      mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
      mReceivedControlPacketHeaderHistory.pop_front();
      uint32_t lAddress ( aAddress );
      uint32_t lExpected ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , base_type::mWordCounter , base_type::mTransactionId ) );
      mReply.push_back ( lExpected );
      mSentControlPacketHeaderHistory.push_back ( lExpected );
      mSentControlPacketHeaderHistory.pop_front();

      for ( ; base_type::mWordCounter!=0 ; --base_type::mWordCounter )
      {
        mReply.push_back ( mConfigurationSpace.at( lAddress++ ) );
      }
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::ni_write ( const uint32_t& aAddress , std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd )
    {
      mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
      mReceivedControlPacketHeaderHistory.pop_front();
      uint32_t lAddress ( aAddress );

      while ( aIt != aEnd )
      {
        SetEndpoint ( lAddress , *aIt++ );
      }

      uint32_t lExpected;

      if ( IPbus_major == 1 )
      {
        lExpected = IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 0 , base_type::mTransactionId );
      }
      else
      {
        lExpected = IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , base_type::mWordCounter , base_type::mTransactionId );
      }

      mReply.push_back ( lExpected );
      mSentControlPacketHeaderHistory.push_back ( lExpected );
      mSentControlPacketHeaderHistory.pop_front();
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::write ( const uint32_t& aAddress , std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd )
    {
      mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
      mReceivedControlPacketHeaderHistory.pop_front();
      uint32_t lAddress ( aAddress );

      while ( aIt != aEnd )
      {
        SetEndpoint ( lAddress++ , *aIt++ );
      }

      uint32_t lExpected;

      if ( IPbus_major == 1 )
      {
        lExpected = IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 0 , base_type::mTransactionId );
      }
      else
      {
        lExpected = IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , base_type::mWordCounter , base_type::mTransactionId );
      }

      mReply.push_back ( lExpected );
      mSentControlPacketHeaderHistory.push_back ( lExpected );
      mSentControlPacketHeaderHistory.pop_front();
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::rmw_sum ( const uint32_t& aAddress , const uint32_t& aAddend )
    {
      mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
      mReceivedControlPacketHeaderHistory.pop_front();
      uint32_t lAddress ( aAddress );
      uint32_t lExpected ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 1 , base_type::mTransactionId ) );
      mReply.push_back ( lExpected );
      mSentControlPacketHeaderHistory.push_back ( lExpected );
      mSentControlPacketHeaderHistory.pop_front();

      if ( IPbus_major == 1 )
      {
        //IPbus 1.x returns modified value
        uint32_t lValue( GetEndpoint( lAddress  ) );
        lValue += aAddend;
        SetEndpoint( lAddress  ,  lValue );
        mReply.push_back ( lValue );
      }
      else
      {
        //IPbus 2.x returns pre-modified value
        uint32_t lValue( GetEndpoint( lAddress  ) );
        mReply.push_back ( lValue );
        lValue += aAddend;
        SetEndpoint( lAddress  ,  lValue );
      }
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::rmw_bits ( const uint32_t& aAddress , const uint32_t& aAndTerm , const uint32_t& aOrTerm )
    {
      mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
      mReceivedControlPacketHeaderHistory.pop_front();
      uint32_t lAddress ( aAddress );
      uint32_t lExpected ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 1 , base_type::mTransactionId ) );
      mReply.push_back ( lExpected );
      mSentControlPacketHeaderHistory.push_back ( lExpected );
      mSentControlPacketHeaderHistory.pop_front();

      if ( IPbus_major == 1 )
      {
        //IPbus 1.x returns modified value
        uint32_t lValue( GetEndpoint( lAddress  ) );
        lValue &= aAndTerm;
        lValue |= aOrTerm;
        SetEndpoint( lAddress  ,  lValue );
        mReply.push_back ( lValue );
      }
      else
      {
        //IPbus 2.x returns pre-modified value
        uint32_t lValue( GetEndpoint( lAddress  ) );
        mReply.push_back ( lValue );
        lValue &= aAndTerm;
        lValue |= aOrTerm;
        SetEndpoint( lAddress  ,  lValue );
      }
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::unknown_type()
    {
      log ( Error() , Integer ( base_type::mHeader, IntFmt<hex,fixed>() ) , " is an unknown IPbus transaction header. Returning error code." );
      mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
      mReceivedControlPacketHeaderHistory.pop_front();
      uint32_t lExpected ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 0 , base_type::mTransactionId , 1 ) );
      mReply.push_back ( lExpected );
      mSentControlPacketHeaderHistory.push_back ( lExpected );
      mSentControlPacketHeaderHistory.pop_front();
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    bool DummyHardware<IPbus_major, IPbus_minor>::control_packet_header ()
    {
      if ( base_type::mPacketCounter != 0 )
      {
        uint16_t lTemp ( ( ( mLastPacketHeader>>8 ) &0x0000FFFF ) + 1 );

        if ( lTemp == 0 )
        {
          lTemp = 1;
        }

        if ( base_type::mPacketCounter != lTemp )
        {
          mTrafficHistory.push_back ( 5 );
          mTrafficHistory.pop_front();
          log(Notice(), "Dummy hardware received control packet with ID ", Integer(base_type::mPacketCounter), ", but expected ID ", Integer(lTemp));
          return false;
        }

        mLastPacketHeader = base_type::mPacketHeader;
      }

      mReply.push_back ( base_type::mPacketHeader );
      mTrafficHistory.push_back ( 2 );
      mTrafficHistory.pop_front();
      return true;
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::status_packet_header ( )
    {
      mReply.push_back ( base_type::mPacketHeader );
      mReply.push_back ( BUFFER_SIZE * sizeof ( uint32_t ) );
      mReply.push_back ( REPLY_HISTORY_DEPTH );
      uint16_t lTemp ( ( ( mLastPacketHeader>>8 ) &0x0000FFFF ) + 1 );

      if ( lTemp == 0 )
      {
        lTemp = 1;
      }

      mReply.push_back ( ( mLastPacketHeader & 0xFF0000FF ) | ( ( lTemp <<8 ) & 0x00FFFF00 ) );
      std::deque< uint8_t >::const_iterator lIt ( mTrafficHistory.begin() );

      for ( uint32_t i = 0; i != 4 ; ++i )
      {
        uint32_t lTemp ( 0x00000000 );

        for ( uint32_t j = 0; j != 4 ; ++j )
        {
          lTemp <<= 8;
          lTemp |= ( uint32_t ) ( *lIt );
          lIt++;
        }

        mReply.push_back ( lTemp );;
      }

      for ( const auto& x: mReceivedControlPacketHeaderHistory )
      {
        mReply.push_back ( x );
      }

      for ( const auto& x: mSentControlPacketHeaderHistory )
      {
        mReply.push_back ( x );
      }

      mTrafficHistory.push_back ( 3 );
      mTrafficHistory.pop_front();
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::resend_packet_header ()
    {
      std::deque< std::pair< uint32_t , std::vector< uint32_t > > >::reverse_iterator lIt = mReplyHistory.rbegin();

      for ( ; lIt!=mReplyHistory.rend() ; ++lIt )
      {
        if ( lIt->first == base_type::mPacketCounter )
        {
          mReply = lIt->second;
          break;
        }
      }

      mTrafficHistory.push_back ( 4 );
      mTrafficHistory.pop_front();
    }


    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    void DummyHardware<IPbus_major, IPbus_minor>::unknown_packet_header()
    {
      mTrafficHistory.push_back ( 5 );
      mTrafficHistory.pop_front();
    }


    template class DummyHardware<1, 3>;
    template class DummyHardware<2, 0>;
  }
}



