
#include "uhal/tests/DummyClient.hpp"


#include "uhal/ClientFactory.hpp"


namespace uhal {
namespace tests {


DummyClient::DummyClient ( const std::string& aId, const URI& aUri ) :
  ClientInterface ( aId, aUri, boost::posix_time::seconds ( 1 ) )
{
}

DummyClient::~DummyClient()
{
}

void DummyClient::implementDispatch ( std::shared_ptr< Buffers > )
{
}

ValHeader DummyClient::implementBOT( )
{
  return ValHeader();
}

ValHeader DummyClient::implementWrite ( const uint32_t&, const uint32_t& )
{
  return ValHeader();
}

ValHeader DummyClient::implementWriteBlock ( const uint32_t&, const std::vector< uint32_t >&, const defs::BlockReadWriteMode& )
{
  return ValHeader();
}

ValWord< uint32_t > DummyClient::implementRead ( const uint32_t&, const uint32_t& )
{
  return ValWord<uint32_t>();
}

ValVector< uint32_t > DummyClient::implementReadBlock ( const uint32_t&, const uint32_t&, const defs::BlockReadWriteMode& )
{
  return ValVector< uint32_t >();
}

ValWord< uint32_t > DummyClient::implementRMWbits ( const uint32_t& , const uint32_t& , const uint32_t& )
{
  return ValWord<uint32_t>();
}

ValWord< uint32_t > DummyClient::implementRMWsum ( const uint32_t& , const int32_t& )
{
  return ValWord<uint32_t>();
}

exception::exception* DummyClient::validate ( uint8_t* ,
  uint8_t* ,
  std::deque< std::pair< uint8_t* , uint32_t > >::iterator ,
  std::deque< std::pair< uint8_t* , uint32_t > >::iterator )
{
  return NULL;
}

uint32_t DummyClient::getMaxNumberOfBuffers()
{
  return 0;
}

uint32_t DummyClient::getMaxSendSize()
{
  return 0;
}

uint32_t DummyClient::getMaxReplySize()
{
  return 0;
}

UHAL_REGISTER_EXTERNAL_CLIENT(uhal::tests::DummyClient, "__test__", "A dummy client description")

}
}

