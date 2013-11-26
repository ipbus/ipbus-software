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

#include <boost/asio.hpp>

#include "uhal/tests/DummyHardware.hpp"

using boost::asio::ip::udp;
using namespace uhal;

//! Concrete implementation of emulator of hardware using UDP
template< uint8_t IPbus_major , uint8_t IPbus_minor >
class UDPdummyHardware : public DummyHardware< IPbus_major , IPbus_minor >
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
    UDPdummyHardware ( const uint16_t& aPort , const uint32_t& aReplyDelay, const bool& aBigEndianHack ) :
      DummyHardware< IPbus_major , IPbus_minor > ( aReplyDelay , aBigEndianHack ) ,
      mIOservice(),
      mSocket ( mIOservice , udp::endpoint ( udp::v4(), aPort ) )

    {
    }

    /**
      Destructor
    */
    ~UDPdummyHardware()
    {
    }

    /**
      Concrete implementation of the run function
      Starts the UDP server and runs indefinitely, until exception or user kills the server
    */
    void run()
    {
      while ( true )
      {
        uint32_t lBytes = mSocket.receive_from ( boost::asio::buffer ( & ( base_type::mReceive[0] ), base_type::mReceive.size() <<2 ) , mSenderEndpoint );
        base_type::mReply.clear();
        //All responsibility for understanding the contents and replying is handled by the base class
        base_type::AnalyzeReceivedAndCreateReply ( lBytes );

        if ( base_type::mReply.size() )
        {
          mSocket.send_to ( boost::asio::buffer ( & ( base_type::mReply[0] ) , base_type::mReply.size() <<2 ) , mSenderEndpoint );
        }
      }
    }

  private:
    //! The BOOST ASIO io_service used by the UDP server
    boost::asio::io_service mIOservice;
    //! The socket opened by the UDP server 
    udp::socket mSocket;
    //! The endpoint which sent the UDP datagram
    udp::endpoint mSenderEndpoint;


};



int main ( int argc, char* argv[] )
{
  CommandLineOptions lOptions ( ParseCommandLineOptions ( argc , argv ) );

  if ( lOptions.version == 1 )
  {
    UDPdummyHardware<1,3> lDummyHardware ( lOptions.port , lOptions.delay, false );
    lDummyHardware.run();
  }
  else if ( lOptions.version == 2 )
  {
    UDPdummyHardware<2,0> lDummyHardware ( lOptions.port , lOptions.delay, lOptions.bigendian );
    lDummyHardware.run();
  }
  else
  {
    log ( Error() , "Unknown IPbus version, " , Integer ( lOptions.version ) );
    return 1;
  }

  return 0;
}
