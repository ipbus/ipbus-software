/**
	@file
	@author Andrew W. Rose
	@date 2012
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
// namespace uhal
// {
// class IPbusPacketInfo;
// }

//	Declare the friend streaming class for aIPbusPacketInfo outside the uhal namespace.
/**
	A function to format the IPbusPacket info and place it on a stream
	@param aStream a stream to output the time onto
	@param aIPbusPacketInfo a IPbusPacketInfo to output
	@return a stream for further appending
*/
// std::ostream& operator<< ( std::ostream& aStream, const uhal::IPbusPacketInfo& aIPbusPacketInfo );


// Using the uhal namespace
namespace uhal
{

	class IPbusValidationError: public uhal::exception {  };


	/**
		Enumerated type to define the IPbus transaction type.
		Note that they are stored here as (raw_type << 3) so that the LSL operation does not need to be performed every time a new transaction is created
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

	/**
		Enumerated type to define the IPbus protocol version.
	*/
	enum eIPbusProtocolVersion
	{
		IPbus_1_2,
		IPbus_1_3,
		IPbus_1_4,
		IPbus_2_0
	};


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	struct IPbusHeaderHelper
	{
		static uint32_t calculate ( const eIPbusTransactionType& aType , const uint32_t& aWordCount , const uint32_t& aTransactionId );

		static bool extract ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood );

	};



	// /**
	// Function to unpack a raw IPbus header and format it in a readable fashion
	// @param aHeader a raw IPbus header to be unpacked
	// @return a human readable string representing the contents of the IPbus header
	// */
	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// std::string DebugIPbusHeader ( const uint32_t& aHeader );

	// /**
	// IPbusPacketInfo is a class for the management of IPbusClient data before transmission via the Udp or Tcp Client

	// @author Andrew W. Rose
	// @date 2012
	// */
	// class IPbusPacketInfo
	// {
	// /**
	// friend streaming class for aIPbusPacketInfo
	// @param aStream			The stream to which to write
	// @param aIPbusPacketInfo The IPbusPacketInfo to write to the stream
	// @return a stream for further appending
	// */
	// // friend std::ostream& ( ::operator<< ) ( std::ostream& aStream, const IPbusPacketInfo& aIPbusPacketInfo );

	// public:
	// //! A struct to store a chunk of an IPbus transaction
	// struct tChunks
	// {
	// /**
	// The transaction header associated with the payload
	// mTransactionHeader is always the IPbus transaction header as defined in \n
	// http://indico.cern.ch/getFile.py/access?contribId=3&resId=2&materialId=slides&confId=94579 \n
	// */
	// uint32_t mTransactionHeader;

	// //!	mTransactionHeader is the base address for the transaction, if defined/required
	// uint32_t mBaseAddress;

	// //! The header of the expected reply
	// uint32_t mExpectedReplyHeader;

	// //! The Send Size of the chunk
	// uint32_t mSendSize;

	// //! The Return Size of the chunk
	// uint32_t mReturnSize;

	// //! A set of registers into which to write the transaction reply headers from each device
	// std::vector< uint32_t > mReplyHeaders;

	// //! A pointer to the start of a block of memory containing the data to send
	// uint32_t* mSendPtr;

	// //! A set of pointers to the start of a block of memory into which to write the reply from each device
	// std::vector< uint32_t* > mValMemPtr;

	// };


	// public:

	// //! Default Constructor
	// IPbusPacketInfo();

	// //! Destructor
	// virtual ~IPbusPacketInfo();


	// /**
	// A function to determine whether two PacketInfos are identical (excluding device list)
	// @param aIPbusPacketInfo a IPbusPacketInfo to compare
	// @return whether two PacketInfos are identical
	// */
	// bool operator== ( const IPbusPacketInfo& aIPbusPacketInfo );


	// /**
	// A method to create the transaction header for instruction packets with no base address
	// @param aType			The enumerated type of the transaction
	// @param aWordCount		The number of words in the transaction payload
	// */
	// void setHeader ( const eIPbusTransactionType& aType,
	// const uint32_t& aWordCount
	// );

	// /**
	// A method to create the transaction header for instruction packets with a base address
	// @param aType			The enumerated type of the transaction
	// @param aWordCount		The number of words in the transaction payload
	// @param aBaseAddress		The base-address of the device register to access
	// */
	// void setHeader ( const eIPbusTransactionType& aType ,
	// const uint32_t& aWordCount ,
	// const uint32_t& aBaseAddress
	// );

	// /**
	// A method to set the data payload to be sent, in this case a single word
	// @param aPayload			The single word to be sent
	// */
	// void setPayload ( const uint32_t& aPayload );

	// /**
	// A method to set the data payload to be sent
	// @param aPayload			A vector containing the payload to be sent.
	// */
	// void setPayload ( const std::vector<uint32_t>& aPayload );


	// /**
	// A method to add a device ID to the list of devices which will receive the payload
	// @param aDeviceID		The unique ID of the device
	// */
	// void setDeviceID ( const uint64_t& aDeviceID );


	// /**
	// A method to add a Validated Memory which will receive the payload of the current transaction
	// @param aValWord a Validated Memory which will receive the payload of the current transaction
	// */
	// template< typename T >
	// void setValMem ( ValWord< T >& aValWord );

	// /**
	// A method to add a Validated Memory which will receive the payload of the current transaction
	// @param aValVector a Validated Memory which will receive the payload of the current transaction
	// */
	// template< typename T >
	// void setValMem ( ValVector< T >& aValVector );


	// /**
	// A method to set all Validated memories associated with this transaction as valid
	// */
	// void setAllValMemsValid();

	// /**
	// A method to retrieve the number of 32-bit words that will be sent by the current packet
	// @return the number of 32-bit words that will be sent by the current packet
	// */
	// inline std::size_t SendSize() const;

	// /**
	// A method to retrieve the number of 32-bit words that will be sent by the current packet
	// @return the number of 32-bit words that will be sent by the current packet
	// */
	// inline std::size_t SendHeaderSize() const;

	// /**
	// A method to retrieve the number of 32-bit words that will be sent by the current packet
	// @return the number of 32-bit words that will be sent by the current packet
	// */
	// inline std::size_t SendPayloadSize() const;

	// /**
	// A method to retrieve the number of 32-bit words that are expected to be returned by the current packet
	// @return the number of 32-bit words that are expected to be returned by the current packet
	// */
	// inline std::size_t ReturnSize() const;

	// /**
	// A method to retrieve the number of 32-bit words that are expected to be returned by the current packet
	// @return the number of 32-bit words that are expected to be returned by the current packet
	// */
	// inline std::size_t ReturnHeaderSize() const;

	// /**
	// A method to retrieve the number of 32-bit words that are expected to be returned by the current packet
	// @return the number of 32-bit words that are expected to be returned by the current packet
	// */
	// inline std::size_t ReturnPayloadSize() const;

	// /**
	// A method to calculate the IPbus header for a given IPbus Protocol Version, given a wordcount and transaction ID
	// It is calculated in this function because the process of chunking requires that each chunk has a different header
	// @param aTransactionId a Transaction ID for a chunk of the packet
	// @param aWordCount a Word count for a chunk of the packet
	// @return the IPbus header
	// */
	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// uint32_t calculateHeader ( const uint32_t& aTransactionId , const uint32_t& aWordCount );

	// /**
	// A method to calculate the expected IPbus reply header for a given IPbus Protocol Version, given a wordcount and transaction ID
	// It is calculated in this function because the process of chunking requires that each chunk has a different header
	// @param aTransactionId a Transaction ID for a chunk of the packet
	// @param aWordCount a Word count for a chunk of the packet
	// @return the expected IPbus reply header
	// */
	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// uint32_t calculateReplyHeader ( const uint32_t& aTransactionId , const uint32_t& aWordCount );

	// /**
	// A method to split oversized IPbus transactions into bite-sized chunks
	// @param aMaxChunkSize the maximum size of a chunk
	// @param aTransactionId the Transaction ID of the first chunk
	// */
	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// void splitChunks ( const uint32_t& aMaxChunkSize , uint32_t& aTransactionId );

	// /**
	// A method to retrieve the list of chunks associated with the current IPbus packet
	// @return the list of chunks associated with the current IPbus packet
	// */
	// inline std::deque< tChunks >& getChunks();

	// /**
	// Return whether the current IPbus packet is one which sends an address field
	// @return whether the current IPbus packet is one which sends an address field
	// */
	// inline const bool& hasBaseAddress();

	// /**
	// Return a reference to the list of device IDs that this instruction should be be sent to
	// @return a reference to the list of device IDs that this instruction should be sent to
	// */
	// inline const std::vector<uint64_t>& getDeviceIDs();


	// /** A method to merge a list of device IDs to the list of devices which will receive the payload
	// @param aIPbusPacketInfo		A vector of unique device IDs
	// */
	// void merge ( const IPbusPacketInfo& aIPbusPacketInfo );

	// private:

	// //! The enumerated type of this transaction
	// eIPbusTransactionType mType;
	// //! The word count of this transaction
	// uint32_t mWordCount;
	// //! The register address to which this transaction is to be sent (if there is one)
	// uint32_t mBaseAddress;
	// //! A flag to indicate whether the current instruction is expected to send an address field
	// bool mHasBaseAddress;

	// //! A vector of device IDs to which the payload is to be sent
	// std::vector<uint64_t> mDeviceIDs;

	// /**
	// A local copy of the payload to be sent.
	// Stored as a local copy so that if user sets the value by temporary variable, the data will be valid when sent
	// */
	// std::vector<uint32_t> mPayload;

	// //! Store the location and size of the ValMem space allocated for each device
	// std::vector< std::pair< uint32_t* , bool* > > mValMemPtr;


	// //! A set of managable size chunks
	// std::deque< tChunks > mChunks;

	// };


	// //! Typedef for a deque storing the data to be sent and associated packet header information
	// typedef std::deque< IPbusPacketInfo > tIPbusPacketInfoStorage;

}

#include "uhal/TemplateDefinitions/IPbusPacketInfo.hxx"

#endif
