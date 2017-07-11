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

#ifndef _uhal_tests_DummyHardware_hpp_
#define _uhal_tests_DummyHardware_hpp_

#include "uhal/IPbusInspector.hpp"

#include <vector>
#include <deque>


// Using the uhal namespace
namespace uhal
{
  namespace tests 
  {

    //! The mask for the address space (size of the address space in one larger than this) 
    static const uint32_t ADDRESSMASK = 0x00FFFFFF;
    //! The size of the reply history for IPbus 2.0
    static const uint32_t REPLY_HISTORY_DEPTH = 5;
    //! Size of the receive and reply buffers
    static const uint32_t BUFFER_SIZE = 100000;
  
  
    //! Abstract base class to emulate IPbus hardware
    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    class DummyHardware : public HostToTargetInspector< IPbus_major , IPbus_minor >
    {
        typedef HostToTargetInspector< IPbus_major , IPbus_minor > base_type;
  
      public:
        /**
          Constructor
          @param aReplyDelay a time delay between the reply and response for the first transaction
          @param aBigEndianHack whether we are using the dummy hardware with a client which uses the big-endian hack.
        */
        DummyHardware ( const uint32_t& aReplyDelay, const bool& aBigEndianHack ) : HostToTargetInspector< IPbus_major , IPbus_minor >() ,
          mMemory (),
          mReplyDelay ( aReplyDelay ),
          mReceive ( BUFFER_SIZE , 0x00000000 ),
          mReply ( BUFFER_SIZE , 0x00000000 ),
          mReplyHistory ( REPLY_HISTORY_DEPTH , std::make_pair ( 0 , mReply ) ),
          mLastPacketHeader ( 0x200000f0 ),
          mTrafficHistory ( 16, 0x00 ),
          mReceivedControlPacketHeaderHistory ( 4 , 0x00000000 ),
          mSentControlPacketHeaderHistory ( 4 , 0x00000000 ),
          mBigEndianHack ( aBigEndianHack )
        {
        }
  
        /**
          Destructor
        */
        virtual ~DummyHardware()
        {
        }
  
        /**
          Function which "starts" the dummy hardware
        */
        virtual void run() = 0;

        virtual void SetEndpoint( const uint32_t& aAddress , const uint32_t&  aValue )
        {
          if( ! mMemory.size() ) mMemory.resize( ADDRESSMASK + 1 );
          mMemory.at ( aAddress & ADDRESSMASK ) = aValue;
        }

        virtual uint32_t GetEndpoint( const uint32_t& aAddress )
        {
          if( ! mMemory.size() ) mMemory.resize( ADDRESSMASK + 1 );
          return mMemory.at ( aAddress & ADDRESSMASK );
        }        

  
        /**
          Function which analyses the received IPbus packet and creates the suitable response
          @param aByteCount the number of bytes received
        */
        void AnalyzeReceivedAndCreateReply ( const uint32_t& aByteCount )
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
  
          if ( mReplyDelay )
          {
            log ( Info() , "Sleeping for " , Integer ( mReplyDelay ) , "s" );
            sleep ( mReplyDelay );
            mReplyDelay = 0;
            log ( Info() , "Now replying " );
          }
  
          //
          //-----------------------------------------------------------------------------------------------------------------------------------------------------------------
          if ( LoggingIncludes ( Debug() ) )
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
              for ( std::vector<uint32_t>::iterator lIt ( mReply.begin() ) ; lIt != mReply.end() ; ++lIt )
              {
                *lIt = htonl ( *lIt );
              }
            }
          }
        }
  
      private:
        /**
          Analyse request and create reply when a Byte-OrderTransaction is observed
        */    
        void bot()
        {
          mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
          mReceivedControlPacketHeaderHistory.pop_front();
          uint32_t lExpected ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 0 , base_type::mTransactionId ) );
          mReply.push_back ( lExpected );
          mSentControlPacketHeaderHistory.push_back ( lExpected );
          mSentControlPacketHeaderHistory.pop_front();
        }
        /**
          Analyse request and create reply when a non-incrementing read is observed
          @param aAddress the base address of the read
        */
        void ni_read ( const uint32_t& aAddress )
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
        /**
          Analyse request and create reply when an incrementing read is observed
          @param aAddress the base address of the read
        */
        void read ( const uint32_t& aAddress )
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
  
        /**
          Analyse request and create reply when a non-incrementing write is observed
          @param aAddress the base address of the write
          @param aIt iterator to the start of the payload
          @param aEnd iterator to the end of the payload
        */
        void ni_write ( const uint32_t& aAddress , std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd )
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
  
        /**
          Analyse request and create reply when an incrementing write is observed
          @param aAddress the base address of the write
          @param aIt iterator to the start of the payload
          @param aEnd iterator to the end of the payload
        */
        void write ( const uint32_t& aAddress , std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd )
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
  
        /**
          Analyse request and create reply when a read-modify-write sum is observed
          @param aAddress the base address of the write
          @param aAddend the value to be added
        */
        void rmw_sum ( const uint32_t& aAddress , const uint32_t& aAddend )
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
  
        /**
          Analyse request and create reply when a read-modify-write bits is observed
          @param aAddress the base address of the write
          @param aAndTerm the value to be and'ed
          @param aOrTerm the value to be or'ed
        */
        void rmw_bits ( const uint32_t& aAddress , const uint32_t& aAndTerm , const uint32_t& aOrTerm )
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
  
        /**
          Analyse request and create reply when the header is unknown
        */
        void unknown_type()
        {
          log ( Error() , Integer ( base_type::mHeader, IntFmt<hex,fixed>() ) , " is an unknown IPbus transaction header. Returning error code." );
          mReceivedControlPacketHeaderHistory.push_back ( base_type::mPacketHeader );
          mReceivedControlPacketHeaderHistory.pop_front();
          uint32_t lExpected ( IPbus< IPbus_major , IPbus_minor >::ExpectedHeader ( base_type::mType , 0 , base_type::mTransactionId , 1 ) );
          mReply.push_back ( lExpected );
          mSentControlPacketHeaderHistory.push_back ( lExpected );
          mSentControlPacketHeaderHistory.pop_front();
        }
  
        /**
          Analyse request and create reply when an IPbus 2.0 control packet header is observed
        */
        bool control_packet_header ()
        {
          //         if ( LoggingIncludes ( Debug() ) )
          //         {
          //           base_type::control_packet_header();
          //         }
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
              return false;
            }
  
            mLastPacketHeader = base_type::mPacketHeader;
          }
  
          mReply.push_back ( base_type::mPacketHeader );
          mTrafficHistory.push_back ( 2 );
          mTrafficHistory.pop_front();
          return true;
        }
  
        /**
          Analyse request and create reply when an IPbus 2.0 status packet header is observed
        */
        void status_packet_header ( )
        {
          //         if ( LoggingIncludes ( Debug() ) )
          //         {
          //           base_type::status_packet_header();
          //         }
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
  
          for ( std::deque< uint32_t >::const_iterator i = mReceivedControlPacketHeaderHistory.begin(); i != mReceivedControlPacketHeaderHistory.end() ; ++i )
          {
            mReply.push_back ( *i );
          }
  
          for ( std::deque< uint32_t >::const_iterator i = mSentControlPacketHeaderHistory.begin(); i != mSentControlPacketHeaderHistory.end() ; ++i )
          {
            mReply.push_back ( *i );
          }
  
          mTrafficHistory.push_back ( 3 );
          mTrafficHistory.pop_front();
        }
  
  
        /**
          Analyse request and create reply when an IPbus 2.0 resend packet header is observed
        */
        void resend_packet_header ()
        {
          //         if ( LoggingIncludes ( Debug() ) )
          //         {
          //           base_type::resend_packet_header();
          //         }
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
  
        /**
          Analyse request and create reply when an unknown IPbus 2.0 packet header is observed
        */
        void unknown_packet_header()
        {
          mTrafficHistory.push_back ( 5 );
          mTrafficHistory.pop_front();
        }
  
  
  
      private:
        //! The memory space of the virtual hardware
        std::vector< uint32_t > mMemory;
        //! The delay in seconds between the request and reply of the first transaction
        uint32_t mReplyDelay;
  
      protected:
        //! The buffer for the incoming IPbus packet
        std::vector< uint32_t > mReceive;
        //! The buffer for the outgoing IPbus packet
        std::vector< uint32_t > mReply;
  
        //! The history of the replies for the retry mechanism (IPbus 2.0 and above only)
        std::deque< std::pair< uint32_t , std::vector< uint32_t > > > mReplyHistory;
        
        //! The last sent packet header for the retry mechanism (IPbus 2.0 and above only)
        uint32_t mLastPacketHeader;
        
        //! History of the IPbus 2.0 packet-types received
        std::deque< uint8_t > mTrafficHistory;
  
        //! History of the received control packet headers
        std::deque< uint32_t > mReceivedControlPacketHeaderHistory;
        //! History of the sent control packet headers
        std::deque< uint32_t > mSentControlPacketHeaderHistory;
  
      private:
        //! Whether we are talking to an IPbus client which includes the big-endian hack
        bool mBigEndianHack;
  
    };
  }


}

#endif


