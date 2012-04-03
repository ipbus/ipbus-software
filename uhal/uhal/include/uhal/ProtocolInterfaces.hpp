#ifndef _uhal_ProtocolInterfaces_hpp_
#define _uhal_ProtocolInterfaces_hpp_

#include "uhal/IPbusPacketInfo.hpp"

namespace uhal
{

	class TransportProtocol
	{
		public:
			TransportProtocol(){}

			virtual ~TransportProtocol(){}

			virtual bool Dispatch() = 0;
			
	};
	
	
	class PackingProtocol
	{
		public:
		
			PackingProtocol(){}

			virtual ~PackingProtocol(){}

			virtual void pack( IPbusPacketInfo& aInfo ) = 0;
			
			virtual void PreDispatch(){}

			virtual bool PostDispatch(){ return true; }
			
	};	

	
	
}

#endif
