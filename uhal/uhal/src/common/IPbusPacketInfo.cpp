/*
	@file
	@author Andrew W. Rose
	@date 2010
*/

#include "uhal/IPbusPacketInfo.hpp"
#include "uhal/log.hpp"
#include <arpa/inet.h>


std::ostream& operator<< ( std::ostream& aStream, const uhal::IPbusPacketInfo& aIPbusPacketInfo )
{
	try
	{
		/*	if( aIPbusPacketInfo.mTransactionHeader.size()==0 ){
				aStream << "'IPbusPacketInfo with no type'" << std::endl;
				return aStream;
			}

			aStream << std::hex << std::uppercase << std::setfill('0') << "Header : 0x" << std::setw(8) << aIPbusPacketInfo.mTransactionHeader.at(0) << std::endl;
			if( aIPbusPacketInfo.mTransactionHeader.size()==2 )
				aStream << std::hex << std::uppercase << std::setfill('0') << "Base Address : 0x" << std::setw(8) << aIPbusPacketInfo.mTransactionHeader.at(1) << std::endl;

			if( aIPbusPacketInfo.mPayload.size() ){
				aStream << "Payload :\n" ;

				std::vector<uint32_t>::const_iterator lIt;
				int i;

				for( lIt = aIPbusPacketInfo.mPayload.begin() , i = 0 ; lIt != aIPbusPacketInfo.mPayload.end(); ++lIt , ++i )
					aStream << "\t" << std::dec << i << "\t" << std::hex << std::setw(8) << *lIt << "\n";
			}

			aStream << std::hex << std::uppercase << std::setfill('0')
					<< "aTransactionId = 0x" << std::setw(8) << aIPbusPacketInfo.transactionId()
					<< ", mType = 0x" << std::setw(8) << (aIPbusPacketInfo.type()>>3)
					<< std::dec
					<< ", mWordCount = " << aIPbusPacketInfo.wordCount()
					<< ", #DeviceIDs = " << aIPbusPacketInfo.mDeviceIDs.size()
					<< ", Payload size = " << aIPbusPacketInfo.mPayload.size()
					<< ".";
		*/
		aStream << aIPbusPacketInfo.mType << " " <<  aIPbusPacketInfo.mWordCount << " " <<  aIPbusPacketInfo.mBaseAddress << " " <<  aIPbusPacketInfo.mHasBaseAddress
				<< " Returning : " <<  aIPbusPacketInfo.ReturnSize() << " (" << aIPbusPacketInfo.ReturnHeaderSize() << " , " <<  aIPbusPacketInfo.ReturnPayloadSize() << ")";
		return aStream;
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}
}


bool operator== ( const uhal::IPbusPacketInfo& a1 , const uhal::IPbusPacketInfo& a2 )
{
	try
	{
		if ( a1.mType != a2.mType )
		{
			return false;
		}

		if ( a1.mWordCount != a2.mWordCount )
		{
			return false;
		}

		if ( a1.mBaseAddress != a2.mBaseAddress )
		{
			return false;
		}

		if ( a1.mPayload != a2.mPayload )
		{
			return false;
		}

		/*
				//we cannot have two identical device IDs in the same IPbus Packet as this causes merry hell.
				for ( std::vector<uint32_t>::const_iterator lIt1 = a1.mDeviceIDs.begin() ; lIt1 != a1.mDeviceIDs.end() ; ++lIt1 )
				{
					for ( std::vector<uint32_t>::const_iterator lIt2 = a2.mDeviceIDs.begin() ; lIt2 != a2.mDeviceIDs.end() ; ++lIt2 )
					{
						if ( *lIt1 == *lIt2 )
						{
							return false;
						}
					}
				}
		*/
		//so must be identical...
		return true;
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}
}


// Using the uhal namespace
namespace uhal
{


	std::string DebugIPbusHeader ( const uint32_t& aHeader )
	{
		std::stringstream lStr;

		switch ( aHeader & 0x000000F8 )
		{
			case B_O_T:
				lStr << "Byte Order Transaction";
				break;
			case R_A_I:
				lStr << "Reserved Address Information";
				break;
			case NI_READ:
				lStr << "Non-incrementing Read";
				break;
			case READ:
				lStr << "Incrementing Read";
				break;
			case NI_WRITE:
				lStr << "Non-incrementing Write";
				break;
			case WRITE:
				lStr << "Incrementing Write";
				break;
			case RMW_SUM:
				lStr << "Read-Modify-Write Sum";
				break;
			case RMW_BITS:
				lStr << "Read-Modify-Write Bits";
				break;
			default:
				lStr << "Unknown IPbus transaction type";
		}

		lStr << " : Transaction ID = " << ( ( aHeader>>17 ) & 0x7ff );
		lStr << ", WordCount = " << ( ( aHeader>>8 ) & 0x1ff );
		lStr << ", IPbus Version = " << ( ( aHeader>>28 ) & 0xf );
		return lStr.str();
	}




IPbusPacketInfo::IPbusPacketInfo() try :
		mType ( B_O_T ),
			  mWordCount ( 0 ),
			  mBaseAddress ( 0 ),
			  mHasBaseAddress ( 0 ),
			  mVersion ( 1 )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	IPbusPacketInfo::~IPbusPacketInfo() {}

	void IPbusPacketInfo::setHeader ( const eIPbusTransactionType& aType,
									  const uint32_t& aWordCount
									)
	{
		try
		{
			mType = aType;
			mWordCount = aWordCount;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void IPbusPacketInfo::setHeader ( const eIPbusTransactionType& aType ,
									  const uint32_t& aWordCount ,
									  const uint32_t& aBaseAddress
									)
	{
		try
		{
			mType = aType;
			mWordCount = aWordCount;
			mBaseAddress = aBaseAddress ;
			mHasBaseAddress = true;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	void IPbusPacketInfo::setPayload ( const uint32_t& aPayload )
	{
		try
		{
			mPayload.push_back ( aPayload );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void IPbusPacketInfo::setPayload ( const std::vector<uint32_t>& aPayload )
	{
		try
		{
			mPayload.insert ( mPayload.end() , aPayload.begin() , aPayload.end() );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	// void IPbusPacketInfo::setPayload( const std::vector<uint32_t>::const_iterator& aBegin , const std::vector<uint32_t>::const_iterator& aEnd )
	// {
	// mPayload.insert( mPayload.end() , aBegin , aEnd );
	// }


	void IPbusPacketInfo::setDeviceID ( const uint32_t& aDeviceID )
	{
		try
		{
			mDeviceIDs.push_back ( htonl ( aDeviceID ) );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void IPbusPacketInfo::setAllValMemsValid()
	{
		try
		{
			for ( std::vector< std::pair< uint32_t* , bool* > >::iterator lIt = mValMemPtr.begin(); lIt != mValMemPtr.end(); ++lIt )
			{
				* ( lIt->second ) = true;
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void IPbusPacketInfo::merge ( const IPbusPacketInfo& aIPbusPacketInfo )
	{
		try
		{
			mDeviceIDs.insert ( mDeviceIDs.end() , aIPbusPacketInfo.mDeviceIDs.begin() , aIPbusPacketInfo.mDeviceIDs.end() );
			mValMemPtr.insert ( mValMemPtr.end() , aIPbusPacketInfo.mValMemPtr.begin() , aIPbusPacketInfo.mValMemPtr.end() );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	uint32_t IPbusPacketInfo::calculateHeader ( const uint32_t& aTransactionId )
	{
		try
		{
			return calculateHeader ( aTransactionId , mWordCount );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	uint32_t IPbusPacketInfo::calculateReplyHeader ( const uint32_t& aTransactionId )
	{
		try
		{
			return calculateReplyHeader ( aTransactionId , mWordCount );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	uint32_t IPbusPacketInfo::calculateHeader ( const uint32_t& aTransactionId , const uint32_t& aWordCount	)
	{
		try
		{
			if ( mType==RMW_BITS )
			{
				return ( ( mVersion&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( 1&0x1ff ) <<8 ) | mType;
			}
			else
			{
				return ( ( ( mVersion&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( aWordCount&0x1ff ) <<8 ) | mType );
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	uint32_t IPbusPacketInfo::calculateReplyHeader ( const uint32_t& aTransactionId , const uint32_t& aWordCount )
	{
		try
		{
			/*if(mType==WRITE || mType==NI_WRITE) return((mVersion&0xF)<<28) | ((aTransactionId&0x7ff)<<17) | ((0&0x1ff)<<8) | mType | 0x4;
			else*/ if ( mType==RMW_BITS )
			{
				return ( ( mVersion&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( 1&0x1ff ) <<8 ) | mType | 0x4;
			}
			else if ( mType==R_A_I )
			{
				return ( ( mVersion&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( 2&0x1ff ) <<8 ) | mType | 0x4;
			}
			else
			{
				return ( ( mVersion&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( aWordCount&0x1ff ) <<8 ) | mType | 0x4;
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}



	void IPbusPacketInfo::splitChunks ( const uint32_t& aMaxChunkSize , uint32_t& aTransactionId )
	{
		try
		{
			//calculate how the packets should be split and calculate the headers + base addresses
			bool lSendSizeOk ( SendSize() <= aMaxChunkSize );
			bool lReturnSizeOk ( ReturnSize() <= aMaxChunkSize );

			if ( lSendSizeOk && lReturnSizeOk )
			{
				mChunks.push_back ( tChunks() );
				tChunks& lChunk = mChunks.back();
				lChunk.mTransactionHeader = calculateHeader ( aTransactionId );
				lChunk.mExpectedReplyHeader = calculateReplyHeader ( aTransactionId );
				lChunk.mBaseAddress = mBaseAddress;
				lChunk.mSendSize = SendSize();
				lChunk.mReturnSize = ReturnSize();
				lChunk.mReplyHeaders.resize ( mDeviceIDs.size() );
				lChunk.mValMemPtr.resize ( mDeviceIDs.size() );

				if ( ReturnPayloadSize() != 0 )
				{
					std::vector<uint32_t*>::iterator lChunkValMemIt = lChunk.mValMemPtr.begin();
					std::vector< std::pair< uint32_t* , bool* > >::iterator lMasterValMemIt = mValMemPtr.begin();

					for ( ; lChunkValMemIt != lChunk.mValMemPtr.end() && lMasterValMemIt != mValMemPtr.end() ; ++lChunkValMemIt , ++lMasterValMemIt )
					{
						*lChunkValMemIt = lMasterValMemIt->first;
					}
				}

				lChunk.mSendPtr = & ( mPayload[0] );
				aTransactionId++;
			}
			else if ( lReturnSizeOk )   // so SendSize is too big (can only happen on a write/ni-write)
			{
				uint32_t lHeaderSize = SendHeaderSize();
				uint32_t lSendSize = SendPayloadSize();
				uint32_t lBaseAddress = mBaseAddress;
				uint32_t lOffset ( 0 );
				uint32_t* lSendPtr = & ( mPayload[0] );

				do
				{
					mChunks.push_back ( tChunks() );
					tChunks& lChunk = mChunks.back();
					lChunk.mBaseAddress = lBaseAddress;
					lChunk.mReturnSize = ReturnSize();	//This condition can only happen on a write/ni-write so the ReturnSize for each chunk is just the header size
					lChunk.mReplyHeaders.resize ( mDeviceIDs.size() );
					lChunk.mValMemPtr.resize ( mDeviceIDs.size() );
					// can only happen on a write/ni-write which must never return a payload
					// if( ReturnPayloadSize() != 0 ){
					// ...
					// }
					lChunk.mSendPtr = lSendPtr;

					if ( lSendSize <= aMaxChunkSize )
					{
						lChunk.mTransactionHeader = calculateHeader ( aTransactionId , lSendSize );
						lChunk.mExpectedReplyHeader = calculateReplyHeader ( aTransactionId , lSendSize );
						lChunk.mSendSize = lSendSize + lHeaderSize;
						aTransactionId++;
						break;
					}
					else
					{
						lChunk.mTransactionHeader = calculateHeader ( aTransactionId , aMaxChunkSize );
						lChunk.mExpectedReplyHeader = calculateReplyHeader ( aTransactionId , aMaxChunkSize );
						lChunk.mSendSize = aMaxChunkSize + lHeaderSize;
						aTransactionId++;
						lSendSize -= aMaxChunkSize;

						if ( mType==WRITE )
						{
							lBaseAddress += aMaxChunkSize;
						}
					}

					lOffset += lChunk.mReturnSize - ReturnHeaderSize();
					lSendPtr += lChunk.mSendSize - lHeaderSize;
				}
				while ( true );
			}
			else     // so ReturnSize is too big (can only happen on a read/ni-read)
			{
				uint32_t lReturnHeaderSize = ReturnHeaderSize();
				uint32_t lReturnSize = ReturnPayloadSize();
				uint32_t lBaseAddress = mBaseAddress;
				uint32_t lOffset ( 0 );
				uint32_t* lSendPtr = & ( mPayload[0] );

				do
				{
					mChunks.push_back ( tChunks() );
					tChunks& lChunk = mChunks.back();
					lChunk.mBaseAddress = lBaseAddress;
					lChunk.mSendSize = SendSize();	//This condition can only happen on a read/ni-read so the ReturnSize for each chunk is just the header size
					lChunk.mReplyHeaders.resize ( mDeviceIDs.size() );
					lChunk.mValMemPtr.resize ( mDeviceIDs.size() );
					// can only happen on a read/ni-read which must return a payload
					// if( ReturnPayloadSize() != 0 ){
					std::vector<uint32_t*>::iterator lChunkValMemIt = lChunk.mValMemPtr.begin();
					std::vector< std::pair< uint32_t* , bool* > >::iterator lMasterValMemIt = mValMemPtr.begin();

					for ( ; lChunkValMemIt != lChunk.mValMemPtr.end() && lMasterValMemIt != mValMemPtr.end() ; ++lChunkValMemIt , ++lMasterValMemIt )
					{
						*lChunkValMemIt = 	lMasterValMemIt->first + lOffset;
					}

					// }
					lChunk.mSendPtr = lSendPtr;

					if ( lReturnSize <= aMaxChunkSize )
					{
						lChunk.mTransactionHeader = calculateHeader ( aTransactionId , lReturnSize );
						lChunk.mExpectedReplyHeader = calculateReplyHeader ( aTransactionId , lReturnSize );
						lChunk.mReturnSize = lReturnSize + lReturnHeaderSize;
						aTransactionId++;
						break;
					}
					else
					{
						lChunk.mTransactionHeader = calculateHeader ( aTransactionId , aMaxChunkSize );
						lChunk.mExpectedReplyHeader = calculateReplyHeader ( aTransactionId , aMaxChunkSize );
						lChunk.mReturnSize = aMaxChunkSize + lReturnHeaderSize;
						aTransactionId++;
						lReturnSize -= aMaxChunkSize;

						if ( mType==READ )
						{
							lBaseAddress += aMaxChunkSize;
						}
					}

					lOffset += lChunk.mReturnSize - lReturnHeaderSize;
					// lSendPtr += lChunk.mSendSize - SendHeaderSize();
				}
				while ( true );
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


}




