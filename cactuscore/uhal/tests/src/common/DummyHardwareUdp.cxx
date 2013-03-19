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

static const uint32_t ADDRESSMASK = 0x000FFFFF;

template< uint8_t IPbus_major , uint8_t IPbus_minor >
class UDPdummyHardware : public DummyHardware< IPbus_major , IPbus_minor >
{
  public:
    typedef DummyHardware< IPbus_major , IPbus_minor > base_type;

    UDPdummyHardware ( const uint16_t& aPort , const uint32_t& aReplyDelay ) :
      DummyHardware< IPbus_major , IPbus_minor > ( aReplyDelay ) ,
      mIOservice(),
      mSocket ( mIOservice , udp::endpoint ( udp::v4(), aPort ) )

    {
    }


    ~UDPdummyHardware()
    {
    }

    void run()
    {
      while ( true )
      {
        uint32_t lBytes = mSocket.receive_from ( boost::asio::buffer ( & ( base_type::mReceive[0] ), base_type::mReceive.size() <<2 ) , mSenderEndpoint );
        base_type::AnalyzeReceivedAndCreateReply ( lBytes );
        if( base_type::mReply.size() )
        {
          mSocket.send_to ( boost::asio::buffer ( & ( base_type::mReply[0] ) , base_type::mReply.size() <<2 ) , mSenderEndpoint );
        }
      }
    }

  private:
    boost::asio::io_service mIOservice;
    udp::socket mSocket;
    udp::endpoint mSenderEndpoint;


};



int main ( int argc, char* argv[] )
{
  CommandLineOptions lOptions ( ParseCommandLineOptions ( argc , argv ) );

  if ( lOptions.version == 1 )
  {
    UDPdummyHardware<1,3> lDummyHardware ( lOptions.port , lOptions.delay );
    lDummyHardware.run();
  }
  else if ( lOptions.version == 2 )
  {
    UDPdummyHardware<2,0> lDummyHardware ( lOptions.port , lOptions.delay );
    lDummyHardware.run();
  }
  else
  {
    log ( Error() , "Unknown IPbus version, " , Integer ( lOptions.version ) );
    return 1;
  }

  return 0;
}
