/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef IPbusPacketInfo_hpp
#define IPbusPacketInfo_hpp

// define statements

// C++ includes
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <vector>
#include <deque>
#include <map>

// Custom includes
#include "uhal/ValMem.hpp"
#include "log/log.hpp"

// Using the uhal namespace
namespace uhal
{

	class IPbusValidationError: public uhal::exception {  };


	/**
		Enumerated type to define the IPbus transaction type.
		Note that they are stored here as (raw_type << 3) so that the LSL operation does not need to be performed every time a new transaction is created
	*/
	enum eIPbusTransactionType
	{
		B_O_T = 0xF8,
		READ = 0x18,
		WRITE = 0x20,
		RMW_BITS = 0x28,
		RMW_SUM = 0x30,
		R_A_I = 0xF0,
		NI_READ = 0x40,
		NI_WRITE = 0x48
	};

	/**
		Enumerated type to define the IPbus protocol version.
	*/
	enum eIPbusProtocolVersion
	{
		IPbus_1_2,
		IPbus_1_3,
		IPbus_1_4,
		IPbus_2_0
	};


	std::string toString ( const eIPbusProtocolVersion& aIPbusProtocolVersion );


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	struct IPbusHeaderHelper
	{
		static uint32_t calculate ( const eIPbusTransactionType& aType , const uint32_t& aWordCount , const uint32_t& aTransactionId );
		static bool extract ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood );
	};

}

#include "uhal/TemplateDefinitions/IPbusPacketInfo.hxx"

#endif
