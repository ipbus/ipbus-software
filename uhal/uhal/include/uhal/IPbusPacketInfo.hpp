/**
	@file
	@author Andrew W. Rose
	@date 2010
*/

#ifndef IPbusPacketInfo_hpp
#define IPbusPacketInfo_hpp

// define statements

// C++ includes
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <vector>
#include <deque>
#include <map>

// Custom includes
#include "uhal/ValMem.hpp"
#include "uhal/log.hpp"

// Forward declare the class so we can declare the friend outside the namespace
namespace uhal
{
	class IPbusPacketInfo;
}

//	Declare the friend streaming class for aIPbusPacketInfo outside the uhal namespace.
/**
	A function to format the IPbusPacket info and place it on a stream
	@return a stream for further appending
	@param aStream a stream to output the time onto
	@param aIPbusPacketInfo a IPbusPacketInfo to output
*/
std::ostream& operator<< ( std::ostream& aStream, const uhal::IPbusPacketInfo& aIPbusPacketInfo );

/**
	A function to determine whether two PacketInfos are identical (excluding device list)
	@return a stream for further appending
	@param aL a IPbusPacketInfo to compare
	@param aR a IPbusPacketInfo to compare
*/
bool operator== ( const uhal::IPbusPacketInfo& a1 , const uhal::IPbusPacketInfo& a2 );



// Using the uhal namespace
namespace uhal
{

	/** Enumerated type to define the IPbus transaction type.
		Note that they are stored here as (raw_type << 3) so that the
		LSL operation does not need to be performed every time a new transaction
		is created
		@author Andrew W. Rose
		@date 2010
	*/
	enum eIPbusTransactionType
	{
		B_O_T = 0xF8,
		READ = 0x18,
		WRITE = 0x20,
		RMW_BITS = 0x28,
		RMW_SUM = 0x30,
		R_A_I = 0xF0,
		NI_READ = 0x40,
		NI_WRITE = 0x48
	};

	enum eIPbusProtocolVersion
	{
		IPbus_1_2,
		IPbus_1_3,
		IPbus_1_4,
		IPbus_2_0
	};

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	std::string DebugIPbusHeader ( const uint32_t& aHeader );

	/**
	IPbusPacketInfo is a class for the management of IPbusClient data before transmission via the Udp or Tcp Client

		@author Andrew W. Rose
		@date 2010
	*/
	class IPbusPacketInfo
	{
			/**
				friend streaming class for aIPbusPacketInfo
				@param aStream			The stream to which to write
				@param aIPbusPacketInfo The IPbusPacketInfo to write to the stream
			*/
			friend std::ostream& ( ::operator<< ) ( std::ostream& aStream, const IPbusPacketInfo& aIPbusPacketInfo );

			/**
				friend function to determine whether two PacketInfos are identical (excluding device list)
				@return a stream for further appending
				@param aL a IPbusPacketInfo to compare
				@param aR a IPbusPacketInfo to compare
			*/
			friend bool ( ::operator== ) ( const IPbusPacketInfo& a1 , const IPbusPacketInfo& a2 );

		public:
			struct tChunks
			{
				/**
					The transaction header associated with the payload
					mTransactionHeader is always the IPbus transaction header as defined in \n
						http://indico.cern.ch/getFile.py/access?contribId=3&resId=2&materialId=slides&confId=94579 \n
				*/
				uint32_t mTransactionHeader;

				//!	mTransactionHeader is the base address for the transaction, if defined/required
				uint32_t mBaseAddress;

				//! The header of the expected reply
				uint32_t mExpectedReplyHeader;

				//! The Send Size of the chunk
				uint32_t mSendSize;

				//! The Return Size of the chunk
				uint32_t mReturnSize;

				//! A set of registers into which to write the transaction reply headers from each device
				std::vector< uint32_t > mReplyHeaders;

				//! A pointer to the start of a block of memory containing the data to send
				uint32_t* mSendPtr;

				//! A set of pointers to the start of a block of memory into which to write the reply from each device
				std::vector< uint32_t* > mValMemPtr;

			};


		public:

			//! Default Constructor
			IPbusPacketInfo();

			//! Default Destructor
			virtual ~IPbusPacketInfo();

			/** A method to create the transaction header for instruction packets with no base address
				@param aType			The enumerated type of the transaction
				@param aWordCount		The number of words in the transaction payload
			*/
			void setHeader ( const eIPbusTransactionType& aType,
							 const uint32_t& aWordCount
						   );

			/** A method to create the transaction header for instruction packets with a base address
				@param aType			The enumerated type of the transaction
				@param aWordCount		The number of words in the transaction payload
				@param aBaseAddress		The base-address of the device register to access
			*/
			void setHeader ( const eIPbusTransactionType& aType ,
							 const uint32_t& aWordCount ,
							 const uint32_t& aBaseAddress
						   );

			/** A method to set the data payload to be sent, in this case a single word
				@param aPayload			The single word to be sent
			*/
			void setPayload ( const uint32_t& aPayload );

			/** A method to set the data payload to be sent
				@param aWordCount		The number of words in the transaction payload
				@param aPayload			A vector containing the payload to be sent. By definition, size must be greater than aWordCount.
			*/
			void setPayload ( /*const uint32_t& aWordCount ,*/ const std::vector<uint32_t>& aPayload );

			// /** A method to set the data payload to be sent
			// @param aBegin			A starting iterator
			// @param aEnd				An ending iterator
			// */
			// void setPayload( const std::vector<uint32_t>::const_iterator& aBegin , const std::vector<uint32_t>::const_iterator& aEnd );

			/** A method to add a device ID to the list of devices which will receive the payload
				@param aDeviceID		The unique ID of the device
			*/
			void setDeviceID ( const uint32_t& aDeviceID );


			/** A method to add a device ID to the list of devices which will receive the payload
				@param aDeviceID		The unique ID of the device
			*/
			template< typename T >
			void setValMem ( ValWord< T >& aValWord );

			/** A method to add a device ID to the list of devices which will receive the payload
				@param aDeviceID		The unique ID of the device
			*/
			template< typename T >
			void setValMem ( ValVector< T >& aValVector );

			void setAllValMemsValid();

			//! @return the number of 32-bit words that will be sent by the current packet
			inline std::size_t SendSize() const;

			//! @return the number of 32-bit words that will be sent by the current packet
			inline std::size_t SendHeaderSize() const;

			//! @return the number of 32-bit words that will be sent by the current packet
			inline std::size_t SendPayloadSize() const;

			//! @return the number of 32-bit words that are expected to be returned by the current packet
			inline std::size_t ReturnSize() const;

			//! @return the number of 32-bit words that are expected to be returned by the current packet
			inline std::size_t ReturnHeaderSize() const;

			//! @return the number of 32-bit words that are expected to be returned by the current packet
			inline std::size_t ReturnPayloadSize() const;


			template< eIPbusProtocolVersion IPbusProtocolVersion >
			uint32_t calculateHeader ( const uint32_t& aTransactionId , const uint32_t& aWordCount );

			template< eIPbusProtocolVersion IPbusProtocolVersion >
			uint32_t calculateReplyHeader ( const uint32_t& aTransactionId , const uint32_t& aWordCount );

			template< eIPbusProtocolVersion IPbusProtocolVersion >
			void splitChunks ( const uint32_t& aMaxChunkSize , uint32_t& aTransactionId );


			inline std::deque< tChunks >& getChunks();

			inline const bool& hasBaseAddress();

			inline const std::vector<uint32_t>& getDeviceIDs();


			/** A method to merge a list of device IDs to the list of devices which will receive the payload
				@param aIPbusPacketInfo		A vector of unique device IDs
			*/
			void merge ( const IPbusPacketInfo& aIPbusPacketInfo );

		private:

			eIPbusTransactionType mType;
			uint32_t mWordCount;
			uint32_t mBaseAddress;
			bool mHasBaseAddress;

			//! A vector of device IDs to which the payload is to be sent
			std::vector<uint32_t> mDeviceIDs;

			/**
				A local copy of the payload to be sent.
				Stored as a local copy so that if user sets the value by temporary variable, the data will be valid when sent
			*/
			std::vector<uint32_t> mPayload;

			//! Store the location and size of the ValMem space allocated for each device
			std::vector< std::pair< uint32_t* , bool* > > mValMemPtr;


			//! A set of managable size chunks
			std::deque< tChunks > mChunks;

	};


	//! Typedef for a deque storing the data to be sent and associated packet header information
	typedef std::deque< IPbusPacketInfo > tIPbusPacketInfoStorage;

}

#include "TemplateDefinitions/IPbusPacketInfo.hxx"

#endif
