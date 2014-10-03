
#include "uhal/tests/TCPDummyHardware.hpp"


template< uint8_t IPbus_major , uint8_t IPbus_minor >
void uhal::tests::TCPDummyHardware< IPbus_major , IPbus_minor >::run()
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


template class uhal::tests::TCPDummyHardware<1,3>;
template class uhal::tests::TCPDummyHardware<2,0>;


