/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_PackingProtocol_ControlHubHost_hpp_
#define _uhal_PackingProtocol_ControlHubHost_hpp_

#include "uhal/ProtocolInterfaces.hpp"

#include <boost/thread/thread.hpp>

namespace uhal
{

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	class ControlHubHostPackingProtocol : public PackingProtocol
	{
		public:
			/**
				Constructor
			*/
			ControlHubHostPackingProtocol ( const uint32_t& aDeviceIPaddr , const uint16_t& aDevicePort , const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize );

			/**
				Destructor
			*/
			virtual ~ControlHubHostPackingProtocol();

			virtual uint32_t calculateIPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount );

			virtual bool extractIPbusHeader ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood );

			virtual void Preamble( );

			virtual void Predispatch( );

			// ----------------------------------------------------------------------------------------------------------------------------------------------------------------
			// NOTE! THIS FUNCTION MUST BE THREAD SAFE: THAT IS:
			// IT MUST ONLY USE LOCAL VARIABLES
			//            --- OR ---
			// IT MUST MUTEX PROTECT ACCESS TO MEMBER VARIABLES!
			// ----------------------------------------------------------------------------------------------------------------------------------------------------------------
			virtual bool Validate ( Buffers* aBuffers );

		private:
			uint32_t mDeviceIPaddress;
			uint16_t mDevicePort;
			uint32_t mTransactionCounter;

			struct tPreamble
			{
				uint32_t* mSendByteCountPtr;
				uint16_t* mSendWordCountPtr;

				uint32_t mReplyTotalByteCounter;
				uint32_t mReplyChunkByteCounter;
				uint32_t mReplyDeviceIPaddress;
				uint16_t mReplyDevicePort;
				uint16_t mReplyErrorCode;
			};

			boost::mutex mMutex;
			std::deque< tPreamble > mPreambles;

	};

}

#include "uhal/TemplateDefinitions/PackingProtocol_ControlHubHost.hxx"


#endif
