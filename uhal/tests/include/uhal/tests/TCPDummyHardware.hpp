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

#ifndef _uhal_tests_TCPDummyHardware_hpp_
#define _uhal_tests_TCPDummyHardware_hpp_


#include <boost/asio.hpp>

#include "uhal/tests/DummyHardware.hpp"


namespace uhal {
  namespace tests {
  
    //! Concrete implementation of emulator of hardware using TCP
    template< uint8_t IPbus_major , uint8_t IPbus_minor >
    class TCPDummyHardware : public DummyHardware< IPbus_major , IPbus_minor >
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
        TCPDummyHardware ( const uint16_t& aPort , const uint32_t& aReplyDelay, const bool& aBigEndianHack ) :
          DummyHardware< IPbus_major , IPbus_minor > ( aReplyDelay , aBigEndianHack ) ,
          mIOservice(),
          mAcceptor ( mIOservice , boost::asio::ip::tcp::endpoint ( boost::asio::ip::tcp::v4() , aPort ) ),
          mSocket ( mIOservice )
        {
          mAcceptor.listen();
        }
  
        //! Destructor
        ~TCPDummyHardware()
        {
        }
  
        /**
          Concrete implementation of the run function
          Starts the TCP server and runs indefinitely, until exception or user kills the server
        */
        void run();
  
        void stop();

      private:
        void handle_accept(const boost::system::error_code& aError);

        void handle_read_chunk_header(const boost::system::error_code& ec, std::size_t length);

        void handle_read_chunk_payload(const boost::system::error_code& ec, std::size_t length);

        //! The BOOST ASIO io_cpmtext used by the TCP server
        boost::asio::io_context mIOservice;
        //! The TCP acceptor which opens the TCP port and handles the connection
        boost::asio::ip::tcp::acceptor mAcceptor;
        //! The socket opened by the TCP server 
        boost::asio::ip::tcp::socket mSocket;

        //! The endpoint which sent the UDP datagram
        boost::asio::ip::tcp::endpoint mSenderEndpoint;
        //! Value of 'byte count' header at start of latest TCP chunk
        uint32_t mByteCountHeader;
    };
  }
}

#endif

