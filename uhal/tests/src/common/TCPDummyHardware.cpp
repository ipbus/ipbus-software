
#include "uhal/tests/TCPDummyHardware.hpp"


#include "uhal/log/LogLevels.hpp"
#include "uhal/log/log.hpp"


template< uint8_t IPbus_major , uint8_t IPbus_minor >
void uhal::tests::TCPDummyHardware< IPbus_major , IPbus_minor >::run()
{
    mSocket.close();
    mAcceptor.async_accept ( mSocket, [&] (const boost::system::error_code& e) {this->handle_accept(e);} );

    mIOservice.run();  
    mIOservice.reset();
}


template< uint8_t IPbus_major, uint8_t IPbus_minor>
void uhal::tests::TCPDummyHardware<IPbus_major,IPbus_minor>::stop()
{
  mIOservice.stop();
}


template< uint8_t IPbus_major , uint8_t IPbus_minor >
void uhal::tests::TCPDummyHardware< IPbus_major , IPbus_minor >::handle_accept(const boost::system::error_code&)
{
  boost::asio::async_read ( mSocket , 
          boost::asio::buffer ( &mByteCountHeader, 4 ) ,
          boost::asio::transfer_exactly ( 4 ),
          [&] (const boost::system::error_code& e, std::size_t n) { this->handle_read_chunk_header(e, n);} );
}


template< uint8_t IPbus_major , uint8_t IPbus_minor >
void uhal::tests::TCPDummyHardware< IPbus_major , IPbus_minor >::handle_read_chunk_header(const boost::system::error_code& aError, std::size_t)
{
  if ( aError == boost::asio::error::eof )
  {
    // Connection closed cleanly by peer.
    mSocket.close();
    mAcceptor.async_accept ( mSocket, [&] (const boost::system::error_code& e) {this->handle_accept(e);} );
    return; 
  }
  else if ( aError )
  {
    log ( Error(), "Error while reading socket: ", aError.message() );
    return;
  }

  mByteCountHeader = ntohl ( mByteCountHeader );
  boost::asio::async_read ( mSocket ,
          boost::asio::buffer ( & ( base_type::mReceive[0] ), base_type::mReceive.size() <<2 ) , 
          boost::asio::transfer_exactly ( mByteCountHeader ),
          [&] (const boost::system::error_code& e, std::size_t n) { this->handle_read_chunk_payload(e, n);} );
}


template< uint8_t IPbus_major , uint8_t IPbus_minor >
void uhal::tests::TCPDummyHardware< IPbus_major , IPbus_minor >::handle_read_chunk_payload(const boost::system::error_code& aError, std::size_t aNrBytes)
{
  if ( aError == boost::asio::error::eof )
  {
    // Connection closed cleanly by peer.
    mSocket.close();
    mAcceptor.async_accept ( mSocket, [&] (const boost::system::error_code& e) {this->handle_accept(e);} );
    return;
  }
  else if ( aError )
  {
    log ( Error(), "Error while reading socket: ", aError.message() );
    return;
  }

  base_type::mReply.clear();
  base_type::mReply.push_back ( 0x00000000 );
  //All responsibility for understanding the contents and replying is handled by the base class
  base_type::AnalyzeReceivedAndCreateReply ( aNrBytes );
  uint32_t lSize ( base_type::mReply.size() << 2 );

  if ( lSize > 4 )
  {
    base_type::mReply.front() = htonl ( lSize - 4 );
    boost::asio::write ( mSocket , boost::asio::buffer ( & ( base_type::mReply[0] ) , lSize ) );
  }

  boost::asio::async_read ( mSocket , 
          boost::asio::buffer ( &mByteCountHeader, 4 ) ,
          boost::asio::transfer_exactly ( 4 ),
          [&] (const boost::system::error_code& e, std::size_t n) { this->handle_read_chunk_header(e, n);} );
}



template class uhal::tests::TCPDummyHardware<1,3>;
template class uhal::tests::TCPDummyHardware<2,0>;


