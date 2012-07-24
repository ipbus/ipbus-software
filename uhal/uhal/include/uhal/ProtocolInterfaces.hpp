/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_ProtocolInterfaces_hpp_
#define _uhal_ProtocolInterfaces_hpp_

#include <deque>
#include "uhal/IPbusPacketInfo.hpp"

namespace uhal
{
	//! Forward declaration to be friendly
	class TransportProtocol;
	//! Forward declaration to be friendly
	class PackingProtocol;

	/**
		Function to mutually associate an instance of a transport protocol with an instance of a packing protocol
		@param aTransportProtocol a pointer to an instance of a transport protocol to mutually associate with the specified packing protocol instance
		@param aPackingProtocol a pointer to an instance of a packing protocol to mutually associate with the specified transport protocol instance
	*/
	void Link ( TransportProtocol& aTransportProtocol , PackingProtocol& aPackingProtocol );

	//! A class wrapping the send and recieve buffers that are to be filled and transported and the validated memories associated with them
	class Buffers
	{
		public:
			/**
				Constructor
				@param aMaxSendSize The size of the buffer (in bytes) in the target device for receiving IPbus data packets from uhal.
				@warning Used to set internal buffer size, not for checking
			*/
			Buffers ( const uint32_t& aMaxSendSize );

			/**
				Destructor
			*/
			virtual ~Buffers();

			/**
				Get the number of bytes that are currently in the send buffer
				@return the number of bytes that are currently in the send buffer
			*/
			const uint32_t& sendCounter();

			/**
				Get the number of bytes that are currently expected by the reply buffer
				@return the number of bytes that are currently expected by the reply buffer
			*/
			const uint32_t& replyCounter();

			/**
				Helper function to copy an object to the send buffer
				@param aPtr a pointer to an object to be copied
				@return a pointer to the copy of the object in the send buffer
			*/
			template< typename T >
			uint8_t* send ( const T* aPtr );

			/**
				Helper function to copy an object to the send buffer
				@param aRef a reference to an object to be copied
				@return a pointer to the copy of the object in the send buffer
			*/
			template< typename T >
			uint8_t* send ( const T& aRef );

			/**
				Helper function to copy a block of memory to the send buffer
				@param aPtr a pointer to the start of the memory to be copied
				@param aSize the number of bytes to be copied
				@return a pointer to the copy of the memory block in the send buffer
			*/
			uint8_t* send ( const uint8_t* aPtr , const uint32_t& aSize );

			/**
				Helper function to add a destination object to the reply queue
				@param aPtr a pointer to some persistent object which can be written to when the transaction is performed
			*/
			template< typename T >
			void receive ( T* aPtr );

			/**
				Helper function to add a destination object to the reply queue
				@param aRef a reference to some persistent object which can be written to when the transaction is performed
			*/
			template< typename T >
			void receive ( T& aRef );

			/**
				Helper function to add a destination memory location to the reply queue
				@param aPtr a pointer to some persistent memory which can be written to when the transaction is performed
				@param aSize the number of bytes which can be safely written
			*/
			void receive ( uint8_t* aPtr , const uint32_t& aSize );

			/**
				Helper function to associate a validated memory with this buffer so that it is guaranteed to exist when the transaction is performed
				@param aValMem a validated memory to be associated with this buffer
			*/
			void add ( const ValHeader& aValMem );

			/**
				Helper function to associate a validated memory with this buffer so that it is guaranteed to exist when the transaction is performed
				@param aValMem a validated memory to be associated with this buffer
			*/
			void add ( const ValWord< uint32_t >& aValMem );

			/**
				Helper function to associate a validated memory with this buffer so that it is guaranteed to exist when the transaction is performed
				@param aValMem a validated memory to be associated with this buffer
			*/
			void add ( const ValWord< int32_t >& aValMem );

			/**
				Helper function to associate a validated memory with this buffer so that it is guaranteed to exist when the transaction is performed
				@param aValMem a validated memory to be associated with this buffer
			*/
			void add ( const ValVector< uint32_t >& aValMem );

			/**
				Helper function to associate a validated memory with this buffer so that it is guaranteed to exist when the transaction is performed
				@param aValMem a validated memory to be associated with this buffer
			*/
			void add ( const ValVector< int32_t >& aValMem );

			/**
				Get a pointer to the start of the send buffer
				@return a pointer to the start of the send buffer
			*/
			uint8_t* getSendBuffer();

			/**
				Get a reference to the reply queue
				@return a reference to the reply queue
			*/
			std::deque< std::pair< uint8_t* , uint32_t > >& getReplyBuffer();

			/**
				Helper function to mark all validated memories associated with this buffer as valid
			*/
			void validate();


		private:
			//! The number of bytes that are currently in the send buffer
			uint32_t mSendCounter;
			//! The number of bytes that are currently expected by the reply buffer
			uint32_t mReplyCounter;

			//! The start location of the memory buffer
			uint8_t* mSendBuffer;
			//! The queue of reply destinations
			std::deque< std::pair< uint8_t* , uint32_t > > mReplyBuffer;

			//! Deque holding validated memories so that they are guaranteed to exist when the transaction is performed
			std::deque< ValHeader > mValHeaders;
			//! Deque holding validated memories so that they are guaranteed to exist when the transaction is performed
			std::deque< ValWord< uint32_t > > mUnsignedValWords;
			//! Deque holding validated memories so that they are guaranteed to exist when the transaction is performed
			std::deque< ValWord< int32_t > > mSignedValWords;
			//! Deque holding validated memories so that they are guaranteed to exist when the transaction is performed
			std::deque< ValVector< uint32_t > > mUnsignedValVectors;
			//! Deque holding validated memories so that they are guaranteed to exist when the transaction is performed
			std::deque< ValVector< int32_t > > mSignedValVectors;
	};



	//! An abstract base class defining the interface to a Transport Protocol
	class TransportProtocol
	{
		private:
			friend void Link ( TransportProtocol& aTransportProtocol , PackingProtocol& aPackingProtocol );

		public:
			/**
				Default constructor
			*/
			TransportProtocol ( const uint32_t& aTimeoutPeriod );

			/**
				Destructor
			*/
			virtual ~TransportProtocol();

			/**
				Abstract interface to send the IPbus buffer to the target, read back the response and call the packing-protocol's validate function
				@param aBuffers the buffer object wrapping the send and recieve buffers that are to be transported
			*/
			virtual void Dispatch ( Buffers* aBuffers ) = 0;

			/**
				Abstract interface of a synchronization function to block until all buffers have been sent, all replies received and all data validated
			*/
			virtual void Flush() = 0;

			/**
				Set the timeout period
				@param aTimeoutPeriod a timeout period in seconds
			*/
			virtual void setTimeoutPeriod ( const uint32_t& aTimeoutPeriod );

			/**
				Get the currently set timeout period
				@return aTimeoutPeriod a timeout period in seconds
			*/
			virtual const uint32_t& getTimeoutPeriod();

		protected:
			//! A pointer to an instance of a Packing Protocol which was used to pack the data
			PackingProtocol* mPackingProtocol;

			//! Timeout period for transactions
			uint32_t mTimeoutPeriod;


	};




	//! An abstract base class defining the interface to a Packing Protocol
	class PackingProtocol
	{
		private:
			friend void Link ( TransportProtocol& aTransportProtocol , PackingProtocol& aPackingProtocol );

		public:
			/**
				Default constructor
				@param aMaxSendSize The size of the buffer in the target device for receiving IPbus data packets from uhal
				@param aMaxReplySize The size of the buffer in the target device for sending IPbus data packets to uhal
			*/
			PackingProtocol ( const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize );

			/**
				Destructor
			*/
			virtual ~PackingProtocol();

			/**
				Send a byte order transaction
			*/
			virtual void ByteOrderTransaction( );

			/**
				Write a single, unmasked word to a register
				@param aAddr the address of the register to write
				@param aValue the value to write to the register
			*/
			virtual void write ( const uint32_t& aAddr, const uint32_t& aValue );

			/**
				Write a block of data to a block of registers or a block-write port
				@param aAddr the address of the register to write
				@param aValues the values to write to the registers or a block-write port
				@param aMode whether we are writing to a block of registers (INCREMENTAL) or a block-write port (NON_INCREMENTAL)
			*/
			virtual void writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aValues, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

			/**
				Read a single, masked, unsigned word
				@param aAddr the address of the register to read
				@param aMask the mask to apply to the value after reading
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			virtual ValWord< uint32_t > read ( const uint32_t& aAddr, const uint32_t& aMask = defs::NOMASK );

			/**
				Read a block of unsigned data from a block of registers or a block-read port
				@param aAddr the lowest address in the block of registers or the address of the block-read port
				@param aSize the number of words to read
				@param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			virtual ValVector< uint32_t > readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

			// /**
				// Read a single word, mask it and interpret it as being signed
				// @param aAddr the address of the register to read
				// @param aMask the mask to apply to the value after reading
				// @return a Validated Memory which wraps the location to which the reply data is to be written
			// */
			// virtual ValWord< int32_t > readSigned ( const uint32_t& aAddr, const uint32_t& aMask = defs::NOMASK );

			// /**
				// Read a block of data from a block of registers or a block-read port and interpret it as being signed data
				// @param aAddr the lowest address in the block of registers or the address of the block-read port
				// @param aSize the number of words to read
				// @param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
				// @return a Validated Memory which wraps the location to which the reply data is to be written
			// */
			// virtual ValVector< int32_t > readBlockSigned ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

			// /**
			// Retrieve the reserved address information from the target
			// @return a Validated Memory which wraps the location to which the reserved address information will be written
			// */
			// virtual ValVector< uint32_t > readReservedAddressInfo ();

			/**
				Read the value of a register, apply the AND-term, apply the OR-term, set the register to this new value and return a copy of the new value to the user
				@param aAddr the address of the register to read, modify, write
				@param aANDterm the AND-term to apply to existing value in the target register
				@param aORterm the OR-term to apply to existing value in the target register
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			virtual ValWord< uint32_t > rmw_bits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm );

			/**
				Read the value of a register, add the addend, set the register to this new value and return a copy of the new value to the user
				@param aAddr the address of the register to read, modify, write
				@param aAddend the addend to add to the existing value in the target register
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			virtual ValWord< int32_t > rmw_sum ( const uint32_t& aAddr , const int32_t& aAddend );

			/**
				Finalize the currently filling packet and pass it to the transport protocol
			*/
			virtual void Dispatch( );

			/**
				Function which the transport protocol calls when the IPbus reply is received to check that the headers are as expected
				This base implementation assumes the null preamble has been used (i.e. no preamble) and all functionality is implemented in the protected version
				@param aBuffers the buffer object wrapping the send and recieve buffers that have been transported and are to be validated
				@return whether the returned IPbus packet is valid
				@warning ----------------------------------------------------------------------------------------------------------------------------------------------------------------
				@warning NOTE! THIS FUNCTION MUST BE THREAD SAFE: THAT IS:
				@warning IT MUST ONLY USE LOCAL VARIABLES
				@warning 		   --- OR ---
				@warning IT MUST MUTEX PROTECT ACCESS TO MEMBER VARIABLES!
				@warning ----------------------------------------------------------------------------------------------------------------------------------------------------------------
			*/
			virtual bool Validate ( Buffers* aBuffers );

		protected:

			/**
				Append padding to the end of a packet
				@note This is part of the dirty hack to bypass a limitation in the v1.3 firmware and should not really be required
			*/
			virtual void Padding( );

			/**
				Function which the transport protocol calls when the IPbus reply is received to check that the headers are as expected
				@param aSendBufferStart a pointer to the start of the first word of IPbus data which was sent (i.e. with no preamble)
				@param aSendBufferEnd a pointer to the end of the last word of IPbus data which was sent
				@param aReplyStartIt an iterator to the start of the list of memory locations in to which the reply was written
				@param aReplyEndIt an iterator to the end (one past last valid entry) of the list of memory locations in to which the reply was written
				@return whether the returned IPbus packet is valid
				@warning ----------------------------------------------------------------------------------------------------------------------------------------------------------------
				@warning NOTE! THIS FUNCTION MUST BE THREAD SAFE: THAT IS:
				@warning IT MUST ONLY USE LOCAL VARIABLES
				@warning 		   --- OR ---
				@warning IT MUST MUTEX PROTECT ACCESS TO MEMBER VARIABLES!
				@warning ----------------------------------------------------------------------------------------------------------------------------------------------------------------
			*/
			virtual bool Validate ( uint8_t* aSendBufferStart ,
									uint8_t* aSendBufferEnd ,
									std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt ,
									std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt );

			/**
				Function which checks the available space in the currently filling buffer against requested send and receive sizes and, if there is insufficient space in the currently filling buffer, then dispatch it and create a new buffer
				@param aSendSize the amount of data that the current instruction wishes to send
				@param aReplySize the amount of data that the current instruction expects to receive
				@param aAvailableSendSize return the amount of space available for outgoing IPbus packets
				@param aAvailableReplySize return the amount of space available for incoming IPbus packets
			*/
			virtual void checkBufferSpace ( const uint32_t& aSendSize , const uint32_t& aReplySize , uint32_t& aAvailableSendSize , uint32_t& aAvailableReplySize );

			/**
				Abstract interface of function to calculate the IPbus header for a particular protocol version
				@param aType the type of the IPbus transaction
				@param aWordCount the word count field of the IPbus header
				@return an IPbus header
			*/
			virtual uint32_t calculateIPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount ) = 0;

			/**
				Abstract interface of function to parse an IPbus header for a particular protocol version
				@param aHeader an IPbus header to be parsed
				@param aType return the type of the IPbus transaction
				@param aWordCount return the word count field of the IPbus header
				@param aTransactionId return the TransactionId of the IPbus header
				@param aResponseGood return the response status of the IPbus header
				@return whether extraction succeeded
			*/
			virtual bool extractIPbusHeader ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood ) = 0;

			/**
				Add a preamble to an IPbus buffer (by defualt this does nothing)
			*/
			virtual void Preamble( );

			/**
				Finalize an IPbus buffer before it is transmitted (by defualt this does nothing)
			*/
			virtual void Predispatch( );

			//! A pointer to an instance of a Transport Protocol which will be used to transport the data
			TransportProtocol* mTransportProtocol;

			//! A pointer to a buffer-wrapper object containing the send and recieve buffers that are the next to be transported
			Buffers* mCurrentBuffers;

			//! The size of the buffer in the target device for receiving IPbus data packets from uhal
			uint32_t mMaxSendSize;
			//! The size of the buffer in the target device for sending IPbus data packets to uhal
			uint32_t mMaxReplySize;

	};



}

#include "uhal/TemplateDefinitions/ProtocolInterfaces.hxx"

#endif
