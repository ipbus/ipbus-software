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

#ifndef TCPDummyHardware_hpp
#define TCPDummyHardware_hpp

#include <boost/asio.hpp>
#include "uhal/tests/DummyHardware.hpp"

namespace uhal {
  namespace tests {
  
    //! Concrete implementation of emulator of hardware using TCP
    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    class TCPdummyHardware : public DummyHardware< IPbus_major , IPbus_minor >
    {
      public:
        //! Define the underlying DummyHardware type to be a more convenient label
        typedef DummyHardware< IPbus_major , IPbus_minor > base_type;
  
        /**
          Constructor
          @param aPort the port to be used by the hardware
          @param aReplyDelay a time delay between the reply and response for the first transaction
          @param aBigEndianHack whether we are using the dummy hardware with a client which uses the big-endian hack.
        */
        TCPdummyHardware ( const uint16_t& aPort , const uint32_t& aReplyDelay, const bool& aBigEndianHack ) :
          DummyHardware< IPbus_major , IPbus_minor > ( aReplyDelay , aBigEndianHack ) ,
          mIOservice(),
          mAcceptor ( mIOservice , boost::asio::ip::tcp::endpoint ( boost::asio::ip::tcp::v4() , aPort ) )
        {
          mAcceptor.listen();
        }
  
        /**
          Destructor
        */
        ~TCPdummyHardware()
        {
        }
  
        /**
          Concrete implementation of the run function
          Starts the TCP server and runs indefinitely, until exception or user kills the server
        */
        void run()
        {
          while ( true )
          {
            boost::asio::ip::tcp::socket lSocket ( mIOservice );
            mAcceptor.accept ( lSocket );
  
            while ( true )
            {
              boost::system::error_code lError;
              //           uint32_t lBytes = lSocket.read_some ( boost::asio::buffer ( & ( base_type::mReceive[0] ), base_type::mReceive.size() <<2 ) , lError );
              uint32_t lByteCountHeader ( 0 );
              boost::asio::read ( lSocket , boost::asio::buffer ( &lByteCountHeader, 4 ) ,  boost::asio::transfer_exactly ( 4 ), lError );
  
              if ( lError == boost::asio::error::eof )
              {
                break; // Connection closed cleanly by peer.
              }
              else if ( lError )
              {
                log ( Error(), "Error while reading socket: ",lError.message() );
                break;
              }
  
              lByteCountHeader = ntohl ( lByteCountHeader );
              uint32_t lBytes = boost::asio::read ( lSocket , boost::asio::buffer ( & ( base_type::mReceive[0] ), base_type::mReceive.size() <<2 ) , boost::asio::transfer_exactly ( lByteCountHeader ), lError );
  
              if ( lError == boost::asio::error::eof )
              {
                break; // Connection closed cleanly by peer.
              }
              else if ( lError )
              {
                log ( Error(), "Error while reading socket: ",lError.message() );
                break;
              }
  
              base_type::mReply.clear();
              base_type::mReply.push_back ( 0x00000000 );
              //All responsibility for understanding the contents and replying is handled by the base class
              base_type::AnalyzeReceivedAndCreateReply ( lBytes );
              uint32_t lSize ( base_type::mReply.size() << 2 );
  
              if ( lSize > 4 )
              {
                base_type::mReply.front() = htonl ( lSize - 4 );
                boost::asio::write ( lSocket , boost::asio::buffer ( & ( base_type::mReply[0] ) , lSize ) );
              }
            }
          }
        }
  
      private:
        //! The BOOST ASIO io_service used by the TCP server
        boost::asio::io_service mIOservice;
        //! The TCP acceptor which opens the TCP port and handles the connection
        boost::asio::ip::tcp::acceptor mAcceptor;
        //! The endpoint which sent the UDP datagram
        boost::asio::ip::tcp::endpoint mSenderEndpoint;
    };
  }
}

#endif