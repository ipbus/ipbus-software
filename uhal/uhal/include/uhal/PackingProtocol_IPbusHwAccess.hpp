/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_PackingProtocol_IPbusHwAccess_hpp_
#define _uhal_PackingProtocol_IPbusHwAccess_hpp_

#include "uhal/exception.hpp"
#include "uhal/ProtocolInterfaces.hpp"
#include "boost/system/error_code.hpp"
#include "uhal/AsioAccumulatedPacket.hpp"

namespace uhal
{
	//! Exception class to handle the case where the received header does not match the expected header. Uses the base uhal::exception implementation of what()
	class ReplyHeaderExpectationFailure: public uhal::exception {};

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	class IPbusHwAccessPackingProtocol : public PackingProtocol
	{
		public:
			/**
				Constructor
			*/
			IPbusHwAccessPackingProtocol ( const uint32_t& aMaxPacketLength );

			/**
				Destructor
			*/
			virtual ~IPbusHwAccessPackingProtocol();

			/**
				A function which adds an IPbusPacketInfo to the queue of pending IPbus transactions
				@param aIPbusPacketInfo an IPbusPacketInfo to be added to the queue of pending IPbus transactions
				@param aId not used in this class
			*/
			void pack ( IPbusPacketInfo& aIPbusPacketInfo , const uint32_t& aId = 0 );

			/**
				A function called immediately prior to a call to the Transport Protocol's Dispatch() function. Used for finalizing the queue for dispatch.
			*/
			void PreDispatch();

			/**
				A function called immediately after to a call to the Transport Protocol's Dispatch() function. Used for testing that the replys are as expected.
			*/
			void PostDispatch();

			void ReceiveHandler ( const boost::system::error_code& aErrorCode, std::size_t aReplyLength, std::size_t& aReplyLengthRef , bool& aAwaitingCallBackRef , bool& aErrorRef );

			inline const tAccumulatedPackets& getAccumulatedPackets();

			template< int LogLevel >
			void debug ( const pantheios::level< LogLevel >& aLogLevel );

		private:
			//! The raw stream packets which will be sent by the transport protocol
			tAccumulatedPackets mAccumulatedPackets;

			/**
				The maximum allowed size of a chunk
				@todo Now that the chunking is templated, could this be moved into the IPbusPacketInfo?
			*/
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
