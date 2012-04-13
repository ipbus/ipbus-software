#ifndef _uhal_ProtocolInterfaces_hpp_
#define _uhal_ProtocolInterfaces_hpp_

#include "uhal/IPbusPacketInfo.hpp"

namespace uhal
{

	class TransportProtocol
	{
		public:
			TransportProtocol() {}

			virtual ~TransportProtocol() {}

			virtual void Dispatch() = 0;

	};


	class PackingProtocol
	{
		public:

			PackingProtocol() {}

			virtual ~PackingProtocol() {}

			virtual void pack ( IPbusPacketInfo& aIPbusPacketInfo , const uint32_t& aId = 0 ) = 0;

			virtual void PreDispatch() {}

			virtual void PostDispatch() {}

	};



}

#endif
