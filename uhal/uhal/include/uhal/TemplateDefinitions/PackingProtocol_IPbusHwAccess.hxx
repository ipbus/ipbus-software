#include <uhal/performance.hpp>

namespace uhal
{

	template< eIPbusProtocolVersion IPbusProtocolVersion >

IPbusHwAccessPackingProtocol<  IPbusProtocolVersion >::IPbusHwAccessPackingProtocol ( const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize ) try :
		PackingProtocol ( aMaxSendSize<<2 , aMaxReplySize<<2 ),
						mTransactionCounter ( 0 )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	IPbusHwAccessPackingProtocol<  IPbusProtocolVersion >::~IPbusHwAccessPackingProtocol() {}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	uint32_t IPbusHwAccessPackingProtocol<  IPbusProtocolVersion >::calculateIPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount )
	{
		return IPbusHeaderHelper<IPbusProtocolVersion>::calculate ( aType , aWordCount , mTransactionCounter++ );
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	bool IPbusHwAccessPackingProtocol<  IPbusProtocolVersion >::extractIPbusHeader (
		const uint32_t& aHeader ,
		eIPbusTransactionType& aType ,
		uint32_t& aWordCount ,
		uint32_t& aTransactionId ,
		uint8_t& aResponseGood )
	{
		return IPbusHeaderHelper<IPbusProtocolVersion>::extract ( aHeader , aType , aWordCount , aTransactionId , aResponseGood );
	}

}
