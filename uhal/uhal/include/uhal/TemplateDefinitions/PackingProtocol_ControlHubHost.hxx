#include <uhal/performance.hpp>

namespace uhal
{

	template< eIPbusProtocolVersion IPbusProtocolVersion >

ControlHubHostPackingProtocol<  IPbusProtocolVersion >::ControlHubHostPackingProtocol ( const uint32_t& aDeviceIPaddr , const uint16_t& aDevicePort , const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize ) try :
		PackingProtocol ( aMaxSendSize<<2 , aMaxReplySize<<2 ),
						mDeviceIPaddress ( htonl ( aDeviceIPaddr ) ),
						mDevicePort ( htons ( aDevicePort ) ),
						mTransactionCounter ( 0 )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	ControlHubHostPackingProtocol<  IPbusProtocolVersion >::~ControlHubHostPackingProtocol() {}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	uint32_t ControlHubHostPackingProtocol<  IPbusProtocolVersion >::IPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount )
	{
		return IPbusHeaderHelper<IPbusProtocolVersion>::calculate ( aType , aWordCount , mTransactionCounter++ );
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol<  IPbusProtocolVersion >::Preamble( )
	{
				PERFORMANCE ( "Add preamble" ,
					  // 12 bytes form the preamble:
					  // Byte-count (4 bytes) will be updated before transmission in Predispatch
					  // Device IP address (4 bytes)
					  // Device Port number (2 bytes)
					  // Word-count (2 bytes) will be updated before transmission in Predispatch
					  // 12 bytes form the preamble reply:
					  // Byte-count (4 bytes)
					  // Device IP address (4 bytes)
					  // Device Port number (2 bytes)
					  // Error code (2 bytes)
					  mPreambles.push_back ( tPreamble() );
					  tPreamble& lPreamble ( mPreambles.back() );
					  lPreamble.mSendByteCountPtr = ( uint32_t* ) ( mCurrentBuffers->send ( ( uint32_t ) ( 0 ) ) );
					  mCurrentBuffers->send ( mDeviceIPaddress );
					  mCurrentBuffers->send ( mDevicePort );
					  lPreamble.mSendWordCountPtr = ( uint16_t* ) ( mCurrentBuffers->send ( ( uint16_t ) ( 0 ) ) );
					  mCurrentBuffers->receive ( lPreamble.mReplyTotalByteCounter );
					  mCurrentBuffers->receive ( lPreamble.mReplyChunkByteCounter );
					  mCurrentBuffers->receive ( lPreamble.mReplyDeviceIPaddress );
					  mCurrentBuffers->receive ( lPreamble.mReplyDevicePort );
					  mCurrentBuffers->receive ( lPreamble.mReplyErrorCode );
					  PackingProtocol::Preamble();
					)
	}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol<  IPbusProtocolVersion >::Predispatch( )
	{
				PERFORMANCE ( "Complete Preamble" ,
					  tPreamble& lPreamble ( mPreambles.back() );
					  uint32_t lByteCount ( mCurrentBuffers->sendCounter() );
					  *lPreamble.mSendByteCountPtr = htonl ( lByteCount-4 );
					  *lPreamble.mSendWordCountPtr = htons ( ( lByteCount-12 ) >>2 );
					)
	}


}
