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

#include "uhal/IPbusPacketInfo.hpp"
#include <boost/asio.hpp>
#include <boost/lexical_cast.hpp>

#include "uhal/tests/DummyHardware.hpp"

using boost::asio::ip::udp;
using namespace uhal;

static const uint32_t ADDRESSMASK = 0x000FFFFF;

class UDPdummyHardware : public DummyHardware< 1 , 3 >
{
  public:

    UDPdummyHardware ( const uint16_t& aPort , const uint32_t& aReplyDelay ) :
      DummyHardware< 1 , 3 > ( aReplyDelay ) ,
      mIOservice(),
      mSocket ( mIOservice , udp::endpoint ( udp::v4(), aPort ) )

    {
      logging();
    }


    ~UDPdummyHardware()
    {
      logging();
    }

    void run()
    {
      logging();

      while ( true )
      {
        uint32_t lBytes = mSocket.receive_from ( boost::asio::buffer ( & ( mReceive[0] ), mReceive.size() <<2 ) , mSenderEndpoint );
        AnalyzeReceivedAndCreateReply ( lBytes );
        mSocket.send_to ( boost::asio::buffer ( & ( mReply[0] ) , mReply.size() <<2 ) , mSenderEndpoint );
      }
    }

  private:
    boost::asio::io_service mIOservice;
    udp::socket mSocket;
    udp::endpoint mSenderEndpoint;


};



int main ( int argc, char* argv[] )
{
  logging();
  setLogLevelTo ( Debug() );

  if ( argc < 2 || argc > 3 )
  {
    log ( Error() , "Usage: " , ( const char* ) ( argv[0] ) , " <port> <optional reply delay for first packet in seconds>" );
    return 1;
  }

  uint32_t lReplyDelay ( 0 );

  if ( argc == 3 )
  {
    lReplyDelay = boost::lexical_cast<uint16_t> ( argv[2] );
  }

  UDPdummyHardware lDummyHardware ( boost::lexical_cast<uint16_t> ( argv[1] ) , lReplyDelay );
  lDummyHardware.run();
  return 0;
}
