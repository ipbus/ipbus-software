#ifndef _uhal_PackingProtocol_IPbusHwAccess_hpp_
#define _uhal_PackingProtocol_IPbusHwAccess_hpp_

#include "uhal/exception.hpp"
#include "uhal/ProtocolInterfaces.hpp"
#include "boost/system/error_code.hpp"
#include "uhal/AsioAccumulatedPacket.hpp"

namespace uhal
{
	class ReplyHeaderExpectationFailure: public uhal::exception {};
	class UnknownIPbusProtocolVersion1: public uhal::exception { };

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	class IPbusHwAccessPackingProtocol : public PackingProtocol
	{
		public:
			IPbusHwAccessPackingProtocol ( const uint32_t& aMaxPacketLength );

			virtual ~IPbusHwAccessPackingProtocol();

			void pack ( IPbusPacketInfo& aIPbusPacketInfo , const uint32_t& aId = 0 );

			void PreDispatch();

			void PostDispatch();

			void ReceiveHandler ( const boost::system::error_code& aErrorCode, std::size_t aReplyLength, std::size_t& aReplyLengthRef , bool& aAwaitingCallBackRef , bool& aErrorRef );

			inline const tAccumulatedPackets& getAccumulatedPackets();

		private:
			tAccumulatedPackets mAccumulatedPackets;

			uint32_t mMaxPacketLength;

			/** A register from which to issue byte order transactions
				@note We need to pad out the packets to compensate for the bug in the firmware
				If we didn't then the mByteOrderTransaction could be a single 32bit uint (as it was previously)
			*/
			uint32_t mByteOrderTransaction[8];

			//! A register into which to write the transaction reply header
			uint32_t mBOTReplyHeader[8];

			//! A register into which to write the transaction reply header
			uint32_t mTransactionId;

			//! Instance of the deque storing the data to be sent and associated packet header information
			tIPbusPacketInfoStorage mPacketInfo;

	};

}

#include "TemplateDefinitions/PackingProtocol_IPbusHwAccess.hxx"

#endif
