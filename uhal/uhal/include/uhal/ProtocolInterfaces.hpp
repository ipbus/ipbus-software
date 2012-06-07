/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_ProtocolInterfaces_hpp_
#define _uhal_ProtocolInterfaces_hpp_

//#include <boost/any.hpp>
#include <deque>
#include "uhal/IPbusPacketInfo.hpp"
#include "uhal/performance.hpp"

namespace uhal
{
	class TransportProtocol;
	class PackingProtocol;
	void Link ( TransportProtocol& aTransportProtocol , PackingProtocol& aPackingProtocol );

	class Buffers
	{
		public:
			Buffers ( const uint32_t& aMaxSendSize );

			virtual ~Buffers();

			const uint32_t& sendCounter();
			const uint32_t& replyCounter();

			template< typename T >
			uint8_t* send ( const T* aPtr );

			template< typename T >
			uint8_t* send ( const T& aRef );

			uint8_t* send ( const uint8_t* aPtr , const uint32_t& aSize );

			template< typename T >
			void receive ( T* aPtr );

			template< typename T >
			void receive ( T& aRef );

			void receive ( uint8_t* aPtr , const uint32_t& aSize );

			void add ( const ValHeader& aValMem );

			void add ( const ValWord< uint32_t >& aValMem );

			void add ( const ValWord< int32_t >& aValMem );

			void add ( const ValVector< uint32_t >& aValMem );

			void add ( const ValVector< int32_t >& aValMem );

			uint8_t* getSendBuffer();

			std::deque< std::pair< uint8_t* , uint32_t > >& getReplyBuffer();

			void validate();


		private:
			uint32_t mSendCounter;
			uint32_t mReplyCounter;

			uint8_t* mSendBuffer;
			std::deque< std::pair< uint8_t* , uint32_t > > mReplyBuffer;

			//std::deque< boost::any > mValMems;
			std::deque< ValHeader > mValHeaders;
			std::deque< ValWord< uint32_t > > mUnsignedValWords;
			std::deque< ValWord< int32_t > > mSignedValWords;
			std::deque< ValVector< uint32_t > > mUnsignedValVectors;
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
			TransportProtocol() {}

			/**
				Destructor
			*/
			virtual ~TransportProtocol() {}

			/**
				Pure virtual function in which the concrete implementations should flush the queue of pending IPbus transactions
			*/
			virtual void Dispatch ( Buffers* aBuffers ) = 0;

			virtual void Flush() = 0;
			
		protected:
			PackingProtocol* mPackingProtocol;



	};




	//! An abstract base class defining the interface to a Packing Protocol
	class PackingProtocol
	{
		private:
			friend void Link ( TransportProtocol& aTransportProtocol , PackingProtocol& aPackingProtocol );

		public:
			/**
				Default constructor
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
			virtual ValWord< uint32_t > read ( const uint32_t& aAddr, const uint32_t& aMask = 0xFFFFFFFF );

			/**
				Read a block of unsigned data from a block of registers or a block-read port
				@param aAddr the lowest address in the block of registers or the address of the block-read port
				@param aSize the number of words to read
				@param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			virtual ValVector< uint32_t > readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

			/**
				Read a single word, mask it and interpret it as being signed
				@param aAddr the address of the register to read
				@param aMask the mask to apply to the value after reading
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			virtual ValWord< int32_t > readSigned ( const uint32_t& aAddr, const uint32_t& aMask = 0xFFFFFFFF );

			/**
				Read a block of data from a block of registers or a block-read port and interpret it as being signed data
				@param aAddr the lowest address in the block of registers or the address of the block-read port
				@param aSize the number of words to read
				@param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			virtual ValVector< int32_t > readBlockSigned ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

			/**
				Retrieve the reserved address information from the target
				@return a Validated Memory which wraps the location to which the reserved address information will be written
			*/
			virtual ValVector< uint32_t > readReservedAddressInfo ();

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

			virtual void Dispatch( );

// ----------------------------------------------------------------------------------------------------------------------------------------------------------------
// NOTE! THIS FUNCTION MUST BE THREAD SAFE: THAT IS:
// IT MUST ONLY USE LOCAL VARIABLES
//            --- OR ---
// IT MUST MUTEX PROTECT ACCESS TO MEMBER VARIABLES!
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------
			virtual bool Validate( uint8_t* aSendBufferStart , 
									uint8_t* aSendBufferEnd , 
									std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt , 
									std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt );

// ----------------------------------------------------------------------------------------------------------------------------------------------------------------
// NOTE! THIS FUNCTION MUST BE THREAD SAFE: THAT IS:
// IT MUST ONLY USE LOCAL VARIABLES
//            --- OR ---
// IT MUST MUTEX PROTECT ACCESS TO MEMBER VARIABLES!
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------
			virtual bool Validate( Buffers* aBuffers );

		protected:

			virtual void checkBufferSpace ( const uint32_t& aSendSize , const uint32_t& aReplySize , uint32_t& aAvailableSendSize , uint32_t& aAvailableReplySize );

			virtual uint32_t calculateIPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount ) = 0;

			virtual bool extractIPbusHeader( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood ) = 0;


			virtual void Preamble( );

			virtual void Predispatch( );


			TransportProtocol* mTransportProtocol;

			Buffers* mCurrentBuffers;

			uint32_t mMaxSendSize;
			uint32_t mMaxReplySize;

	};



}

#include "uhal/TemplateDefinitions/ProtocolInterfaces.hxx"

#endif
