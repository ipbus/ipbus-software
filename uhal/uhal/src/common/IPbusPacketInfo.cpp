/*
	@file
	@author Andrew W. Rose
	@date 2010
*/

#include "uhal/IPbusPacketInfo.hpp"
#include <arpa/inet.h>


std::ostream &operator<<( std::ostream &aStream, const uhal::IPbusPacketInfo &aIPbusPacketInfo){
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


bool operator==( const uhal::IPbusPacketInfo &a1 , const uhal::IPbusPacketInfo &a2 ){

	if( a1.mType != a2.mType ) return false;
	if( a1.mWordCount != a2.mWordCount ) return false;
	if( a1.mBaseAddress != a2.mBaseAddress ) return false;
	if( a1.mPayload != a2.mPayload ) return false;

	//so must be identical...
	return true;
}

		
// Using the uhal namespace
namespace uhal{

	IPbusPacketInfo::IPbusPacketInfo() :
		mType(B_O_T),
		mWordCount(0),
		mBaseAddress(0),
		mHasBaseAddress(0),	
		mVersion(1)
		{}
			
	IPbusPacketInfo::~IPbusPacketInfo(){}

	void IPbusPacketInfo::setHeader( const eIPbusTransactionType& aType,
			const uint32_t& aWordCount
			)
		{
			mType = aType;
			mWordCount = aWordCount;		
		}

	void IPbusPacketInfo::setHeader( const eIPbusTransactionType& aType ,
			const uint32_t& aWordCount , 
			const uint32_t& aBaseAddress 
			)
		{
			mType = aType;
			mWordCount = aWordCount;
			mBaseAddress = aBaseAddress ;
			mHasBaseAddress = true;			
		}

		
	void IPbusPacketInfo::setPayload( const uint32_t& aPayload )
		{
			mPayload.push_back( aPayload );
		}

	void IPbusPacketInfo::setPayload( const std::vector<uint32_t>& aPayload )
		{
			mPayload.insert( mPayload.end() , aPayload.begin() , aPayload.end() );
		}		
		
	// void IPbusPacketInfo::setPayload( const std::vector<uint32_t>::const_iterator& aBegin , const std::vector<uint32_t>::const_iterator& aEnd )
		// {
			// mPayload.insert( mPayload.end() , aBegin , aEnd );
		// }
		
		
	void IPbusPacketInfo::setDeviceID( const uint32_t& aDeviceID )
		{
			mDeviceIDs.push_back( htonl(aDeviceID) );
		}

	void IPbusPacketInfo::setAllValMemsValid()
		{
			for( std::vector< std::pair< uint32_t* , bool* > >::iterator lIt = mValMemPtr.begin(); lIt != mValMemPtr.end(); ++lIt ){
				*(lIt->second) = true;
			}
		}

	void IPbusPacketInfo::merge( const IPbusPacketInfo & aIPbusPacketInfo )
		{
			mDeviceIDs.insert( mDeviceIDs.end() , aIPbusPacketInfo.mDeviceIDs.begin() , aIPbusPacketInfo.mDeviceIDs.end() );
			mValMemPtr.insert( mValMemPtr.end() , aIPbusPacketInfo.mValMemPtr.begin() , aIPbusPacketInfo.mValMemPtr.end() );
		}		
		
		
	uint32_t IPbusPacketInfo::calculateHeader( const uint32_t& aTransactionId ){
		return calculateHeader( aTransactionId , mWordCount );
	}
	
	
	uint32_t IPbusPacketInfo::calculateReplyHeader( const uint32_t& aTransactionId ){
		return calculateReplyHeader( aTransactionId , mWordCount );
	}
		
	uint32_t IPbusPacketInfo::calculateHeader( const uint32_t& aTransactionId , const uint32_t& aWordCount	){	
		if(mType==RMW_BITS) return((mVersion&0xF)<<28) | ((aTransactionId&0x7ff)<<17) | ((1&0x1ff)<<8) | mType;
		else return( ((mVersion&0xF)<<28) | ((aTransactionId&0x7ff)<<17) | ((aWordCount&0x1ff)<<8) | mType );	
	}
		

	uint32_t IPbusPacketInfo::calculateReplyHeader( const uint32_t& aTransactionId , const uint32_t& aWordCount ){		
		/*if(mType==WRITE || mType==NI_WRITE) return((mVersion&0xF)<<28) | ((aTransactionId&0x7ff)<<17) | ((0&0x1ff)<<8) | mType | 0x4;
		else*/ if(mType==RMW_BITS) return((mVersion&0xF)<<28) | ((aTransactionId&0x7ff)<<17) | ((1&0x1ff)<<8) | mType | 0x4;
		else if(mType==R_A_I) return((mVersion&0xF)<<28) | ((aTransactionId&0x7ff)<<17) | ((2&0x1ff)<<8) | mType | 0x4;
		else return((mVersion&0xF)<<28) | ((aTransactionId&0x7ff)<<17) | ((aWordCount&0x1ff)<<8) | mType | 0x4;
	}



	void IPbusPacketInfo::splitChunks( const uint32_t& aMaxChunkSize , uint32_t& aTransactionId ){
		
		//calculate how the packets should be split and calculate the headers + base addresses
		bool lSendSizeOk( SendSize() <= aMaxChunkSize );
		bool lReturnSizeOk( ReturnSize() <= aMaxChunkSize );
			
		if( lSendSizeOk && lReturnSizeOk ){
			mChunks.push_back( tChunks() );
			tChunks& lChunk = mChunks.back();
			lChunk.mTransactionHeader = calculateHeader(aTransactionId);
			lChunk.mExpectedReplyHeader = calculateReplyHeader(aTransactionId);
			lChunk.mBaseAddress = mBaseAddress;
			lChunk.mSendSize = SendSize();
			lChunk.mReturnSize = ReturnSize();
			
			lChunk.mReplyHeaders.resize( mDeviceIDs.size() );
			lChunk.mValMemPtr.resize( mDeviceIDs.size() );
			
			if( ReturnPayloadSize() != 0 ){
				std::vector<uint32_t*>::iterator lChunkValMemIt = lChunk.mValMemPtr.begin();
				std::vector< std::pair< uint32_t* , bool* > >::iterator lMasterValMemIt = mValMemPtr.begin();
				
				for ( ; lChunkValMemIt != lChunk.mValMemPtr.end() && lMasterValMemIt != mValMemPtr.end() ; ++lChunkValMemIt , ++lMasterValMemIt ){
					*lChunkValMemIt = lMasterValMemIt->first;			
				}
			}
			
			lChunk.mSendPtr = &(mPayload[0]);
			
			aTransactionId++;
			
		}else if ( lReturnSizeOk ){ // so SendSize is too big (can only happen on a write/ni-write)
			
			uint32_t lHeaderSize = SendHeaderSize(); 
			uint32_t lSendSize = SendPayloadSize();
			uint32_t lBaseAddress = mBaseAddress;
			
			uint32_t lOffset(0);
			uint32_t* lSendPtr = &(mPayload[0]);
			
			do{
				mChunks.push_back( tChunks() );
				tChunks& lChunk = mChunks.back();

				lChunk.mBaseAddress = lBaseAddress;
				lChunk.mReturnSize = ReturnSize();	//This condition can only happen on a write/ni-write so the ReturnSize for each chunk is just the header size					
				lChunk.mReplyHeaders.resize( mDeviceIDs.size() );
				lChunk.mValMemPtr.resize( mDeviceIDs.size() );

				// can only happen on a write/ni-write which must never return a payload
				// if( ReturnPayloadSize() != 0 ){
					// ...
				// }
				
				lChunk.mSendPtr = lSendPtr;
				
				if( lSendSize <= aMaxChunkSize ){
					lChunk.mTransactionHeader = calculateHeader(aTransactionId , lSendSize );
					lChunk.mExpectedReplyHeader = calculateReplyHeader(aTransactionId , lSendSize );
					lChunk.mSendSize = lSendSize + lHeaderSize;

					aTransactionId++;
					break;
				}else{
					lChunk.mTransactionHeader = calculateHeader(aTransactionId , aMaxChunkSize );
					lChunk.mExpectedReplyHeader = calculateReplyHeader(aTransactionId , aMaxChunkSize );
					lChunk.mSendSize = aMaxChunkSize + lHeaderSize;

					aTransactionId++;
					lSendSize -= aMaxChunkSize;
					if ( mType==WRITE ){
						lBaseAddress += aMaxChunkSize;
					}
				}
				
				lOffset += lChunk.mReturnSize - ReturnHeaderSize();
				lSendPtr += lChunk.mSendSize - lHeaderSize;
				
			}while( true );

		} else { // so ReturnSize is too big (can only happen on a read/ni-read)

			uint32_t lReturnHeaderSize = ReturnHeaderSize(); 
			uint32_t lReturnSize = ReturnPayloadSize();
			uint32_t lBaseAddress = mBaseAddress;

			uint32_t lOffset(0);
			uint32_t* lSendPtr = &(mPayload[0]);
			
			do{
				mChunks.push_back( tChunks() );
				tChunks& lChunk = mChunks.back();

				lChunk.mBaseAddress = lBaseAddress;
				lChunk.mSendSize = SendSize();	//This condition can only happen on a read/ni-read so the ReturnSize for each chunk is just the header size					
				lChunk.mReplyHeaders.resize( mDeviceIDs.size() );
				lChunk.mValMemPtr.resize( mDeviceIDs.size() );

				// can only happen on a read/ni-read which must return a payload
				// if( ReturnPayloadSize() != 0 ){
					std::vector<uint32_t*>::iterator lChunkValMemIt = lChunk.mValMemPtr.begin();
					std::vector< std::pair< uint32_t* , bool* > >::iterator lMasterValMemIt = mValMemPtr.begin();
					
					for ( ; lChunkValMemIt != lChunk.mValMemPtr.end() && lMasterValMemIt != mValMemPtr.end() ; ++lChunkValMemIt , ++lMasterValMemIt ){
						*lChunkValMemIt = 	lMasterValMemIt->first + lOffset;			
					}
				// }
				
				lChunk.mSendPtr = lSendPtr;
				
				if( lReturnSize <= aMaxChunkSize ){
					lChunk.mTransactionHeader = calculateHeader(aTransactionId , lReturnSize );
					lChunk.mExpectedReplyHeader = calculateReplyHeader(aTransactionId , lReturnSize );
					lChunk.mReturnSize = lReturnSize + lReturnHeaderSize;

					aTransactionId++;
					break;
				}else{
					lChunk.mTransactionHeader = calculateHeader(aTransactionId , aMaxChunkSize );
					lChunk.mExpectedReplyHeader = calculateReplyHeader(aTransactionId , aMaxChunkSize );
					lChunk.mReturnSize = aMaxChunkSize + lReturnHeaderSize;

					aTransactionId++;
					lReturnSize -= aMaxChunkSize;
					if ( mType==READ ){
						lBaseAddress += aMaxChunkSize;
					}
				}
				
				lOffset += lChunk.mReturnSize - lReturnHeaderSize;
				// lSendPtr += lChunk.mSendSize - SendHeaderSize();
				
			}while( true );
					
		}
			

			
	}
		
	
}


	

