/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_PackingProtocol_IPbusHwAccess_hpp_
#define _uhal_PackingProtocol_IPbusHwAccess_hpp_

#include "uhal/ProtocolInterfaces.hpp"

namespace uhal
{

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	class IPbusHwAccessPackingProtocol : public PackingProtocol
	{
		public:
			/**
				Constructor
			*/
			IPbusHwAccessPackingProtocol ( const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize );

			/**
				Destructor
			*/
			virtual ~IPbusHwAccessPackingProtocol();

			virtual uint32_t calculateIPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount );

			virtual bool extractIPbusHeader ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood );

		private:
			uint32_t mTransactionCounter;

	};

}

#include "uhal/TemplateDefinitions/PackingProtocol_IPbusHwAccess.hxx"


#endif
