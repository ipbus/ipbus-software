
#include "uhal/tests/UDPDummyHardware.hpp"


template< uint8_t IPbus_major, uint8_t IPbus_minor>
void uhal::tests::UDPDummyHardware<IPbus_major,IPbus_minor>::run()
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


template class uhal::tests::UDPDummyHardware<1,3>;
template class uhal::tests::UDPDummyHardware<2,0>;

