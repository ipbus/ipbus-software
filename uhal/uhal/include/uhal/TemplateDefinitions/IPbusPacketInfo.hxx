

namespace uhal
{

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	uint32_t IPbusHeaderHelper< IPbusProtocolVersion >::calculate ( const eIPbusTransactionType& aType , const uint32_t& aWordCount , const uint32_t& aTransactionId )
	{
		return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( aWordCount&0x1ff ) <<8 ) | aType;
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	bool IPbusHeaderHelper< IPbusProtocolVersion >::extract ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood )
	{
		try
		{
			aType = eIPbusTransactionType ( aHeader & 0xF8 );
			aWordCount = ( aHeader >> 8 ) & 0x1ff;
			aTransactionId = ( aHeader >> 17 ) & 0x7ff;
			aResponseGood = aHeader & 0x3;
			return true;
		}
		catch ( const std::exception& aExc )
		{
			return false;
		}
	}

}

