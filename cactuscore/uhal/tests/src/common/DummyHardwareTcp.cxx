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

using boost::asio::ip::tcp;
using namespace uhal;


template< uint8_t IPbus_major , uint8_t IPbus_minor >
class TCPdummyHardware : public DummyHardware< IPbus_major , IPbus_minor >
{
  public:
    typedef DummyHardware< IPbus_major , IPbus_minor > base_type;

    TCPdummyHardware ( const uint16_t& aPort , const uint32_t& aReplyDelay ) :
      DummyHardware< IPbus_major , IPbus_minor > ( aReplyDelay ) ,
      mIOservice(),
      mAcceptor ( mIOservice , tcp::endpoint ( tcp::v4() , aPort ) )
    {
      mAcceptor.listen();
    }


    ~TCPdummyHardware()
    {
    }

    void run()
    {
      while ( true )
      {
        tcp::socket lSocket ( mIOservice );
        mAcceptor.accept ( lSocket );

        while ( true )
        {
          boost::system::error_code lError;
          uint32_t lBytes = lSocket.read_some ( boost::asio::buffer ( & ( base_type::mReceive[0] ), base_type::mReceive.size() <<2 ) , lError );

          if ( lError == boost::asio::error::eof )
          {
            break; // Connection closed cleanly by peer.
          }
          else if ( lError )
          {
            log ( Error(), "Error while reading socket: ",lError.message() );
            break;
          }

          base_type::AnalyzeReceivedAndCreateReply ( lBytes );
          boost::asio::write ( lSocket , boost::asio::buffer ( & ( base_type::mReply[0] ) , base_type::mReply.size() <<2 ) );
        }
      }
    }

  private:
    boost::asio::io_service mIOservice;
    tcp::acceptor mAcceptor;
    tcp::endpoint mSenderEndpoint;
};



int main ( int argc, char* argv[] )
{
  CommandLineOptions lOptions ( ParseCommandLineOptions ( argc , argv ) );

  if ( lOptions.version == 1 )
  {
    TCPdummyHardware<1,3> lDummyHardware ( lOptions.port , lOptions.delay );

    while ( true )
    {
      lDummyHardware.run();
    }
  }
  else if ( lOptions.version == 2 )
  {
    TCPdummyHardware<2,0> lDummyHardware ( lOptions.port , lOptions.delay );

    while ( true )
    {
      lDummyHardware.run();
    }
  }
  else
  {
    log ( Error() , "Unknown IPbus version, " , Integer ( lOptions.version ) );
    return 1;
  }

  return 0;
}
