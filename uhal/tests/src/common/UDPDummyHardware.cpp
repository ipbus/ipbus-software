
#include "uhal/tests/UDPDummyHardware.hpp"


template< uint8_t IPbus_major, uint8_t IPbus_minor>
void uhal::tests::UDPDummyHardware<IPbus_major,IPbus_minor>::run()
{
  mSocket.async_receive_from(boost::asio::buffer ( & ( base_type::mReceive[0] ), base_type::mReceive.size() <<2 ),
      mSenderEndpoint,
      [&] (const boost::system::error_code& e, std::size_t n) { this->handle_receive(e, n); });

  mIOservice.run();
}


template< uint8_t IPbus_major, uint8_t IPbus_minor>
void uhal::tests::UDPDummyHardware<IPbus_major,IPbus_minor>::stop()
{
  mIOservice.stop();
}


template< uint8_t IPbus_major, uint8_t IPbus_minor>
void uhal::tests::UDPDummyHardware<IPbus_major,IPbus_minor>::handle_receive(const boost::system::error_code&, std::size_t length)
{
  // std::cout << "> Dummy HW entering handle_receive" << std::endl;

  base_type::mReply.clear();

  base_type::AnalyzeReceivedAndCreateReply ( length );

  if ( base_type::mReply.size() )
  {
    mSocket.send_to ( boost::asio::buffer ( & ( base_type::mReply[0] ) , base_type::mReply.size() <<2 ) , mSenderEndpoint );
  }

  mSocket.async_receive_from(boost::asio::buffer ( & ( base_type::mReceive[0] ), base_type::mReceive.size() <<2 ),
      mSenderEndpoint,
      [&] (const boost::system::error_code& e, std::size_t n) { this->handle_receive(e, n); });

  // std::cout << "> Dummy HW exiting handle_receive" << std::endl;
}

template class uhal::tests::UDPDummyHardware<1,3>;
template class uhal::tests::UDPDummyHardware<2,0>;
