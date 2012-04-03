#ifndef _uhal_ClientInterface_hpp_
#define _uhal_ClientInterface_hpp_

#include "uhal/definitions.hpp"
#include "uhal/ValMem.hpp"

#include "uhal/log.hpp"

#include "uhal/ProtocolInterfaces.hpp"

#include "BoostSpiritGrammars/URLGrammar.hpp"

#include <vector>
#include <deque>
#include <iostream>

namespace uhal
{
	class AtomicTransactionSize: public std::exception {};
	class CalledBaseDispatchMethod: public std::exception {};

	class ClientInterface
	{
		public:
			ClientInterface ( const std::string& aId, const URI& aUri );
			
			virtual ~ClientInterface();

			const std::string& id();
			
			bool ping();
			
			std::string url();

			virtual void write ( const uint32_t aAddr, const uint32_t& aValue );

			virtual void write ( const uint32_t& aAddr, const uint32_t& aValue, const uint32_t& aMask );

			virtual void writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& values, const defs::BlockReadWriteMode aMode=defs::INCREMENTAL );

			virtual ValWord< uint32_t > read ( const uint32_t& aAddr );
			
			virtual ValWord< uint32_t > read ( const uint32_t& aAddr, const uint32_t& aMask );

			virtual ValVector< uint32_t > readBlock ( const uint32_t& aAddr, const uint32_t& size, const defs::BlockReadWriteMode aMode=defs::INCREMENTAL );

			void dispatch ( defs::DispatchMode aMode = defs::NON_ATOMIC );
		
		private:
		
			virtual PackingProtocol& getPackingProtocol() = 0;

			virtual TransportProtocol& getTransportProtocol() = 0;		
			
		private:
			static const size_t MAX_REQUEST_PER_PACKET = 1500/8/2;
			
		protected:
			std::string mId;
			URI mUri;
						
	};



}

#endif

