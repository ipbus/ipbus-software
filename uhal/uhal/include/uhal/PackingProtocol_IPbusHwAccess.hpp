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

	//! A concrete class implementing the packing necessary for a direct connection to IPbus hardware
	template< eIPbusProtocolVersion IPbusProtocolVersion >
	class IPbusHwAccessPackingProtocol : public PackingProtocol
	{
		public:
			/**
				Constructor
				@param aMaxSendSize The size of the buffer in the target device for receiving IPbus data packets from uhal
				@param aMaxReplySize The size of the buffer in the target device for sending IPbus data packets to uhal
			*/
			IPbusHwAccessPackingProtocol ( const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize );

			/**
				Destructor
			*/
			virtual ~IPbusHwAccessPackingProtocol();

			/**
				Concrete implementaion of function to calculate the IPbus header for a particular protocol version
				@param aType the type of the IPbus transaction
				@param aWordCount the word count field of the IPbus header
				@return an IPbus header
			*/
			virtual uint32_t calculateIPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount );


			/**
				Concrete implementaion of function to parse an IPbus header for a particular protocol version
				@param aHeader an IPbus header to be parsed
				@param aType return the type of the IPbus transaction
				@param aWordCount return the word count field of the IPbus header
				@param aTransactionId return the TransactionId of the IPbus header
				@param aResponseGood return the response status of the IPbus header
				@return whether extraction succeeded
			*/
			virtual bool extractIPbusHeader ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood );

			void Predispatch( );

		private:
			//! The transaction counter which will be incremented in the sent IPbus headers
			uint32_t mTransactionCounter;

	};

}

#include "uhal/TemplateDefinitions/PackingProtocol_IPbusHwAccess.hxx"


#endif
