

#include "uhal/log.hpp"
#include "uhal/PackingProtocol_IPbusHwAccess.hpp"

namespace uhal
{

	IPbusHwAccessPackingProtocol::IPbusHwAccessPackingProtocol( const uint32_t& aMaxPacketLength ) : 
		PackingProtocol(),
		mMaxPacketLength( aMaxPacketLength ),
		mTransactionId( 0 )
	{
		//We need to pad out the packets to compensate for the bug in the firmware
		//If we didn't then the mByteOrderTransaction could be a single 32bit uint (as it was previously)
		mByteOrderTransaction[0] = 0x10FE00F8;
		mByteOrderTransaction[1] = 0x10FE00F8;
		mByteOrderTransaction[2] = 0x10FE00F8;
		mByteOrderTransaction[3] = 0x10FE00F8;
		mByteOrderTransaction[4] = 0x10FE00F8;
		mByteOrderTransaction[5] = 0x10FE00F8;
		mByteOrderTransaction[6] = 0x10FE00F8;
		mByteOrderTransaction[7] = 0x10FE00F8;	
	}

	IPbusHwAccessPackingProtocol::~IPbusHwAccessPackingProtocol(){}

	void IPbusHwAccessPackingProtocol::pack( IPbusPacketInfo& aIPbusPacketInfo , const uint32_t &aId ){
	
		//for local access we don't care if it already exists since we don't do any optimization, we just store it and add it to the queue
		mPacketInfo.push_back( aIPbusPacketInfo );
		IPbusPacketInfo& lIPbusPacketInfo = mPacketInfo.back();
		lIPbusPacketInfo.setDeviceID( aId ); //just a dummy device ID
		lIPbusPacketInfo.splitChunks( mMaxPacketLength , mTransactionId );
		
		if( mAccumulatedPackets.size() == 0 ){
			mAccumulatedPackets.push_back( tAccumulatedPacket() );
		}
		
		for( std::deque< IPbusPacketInfo::tChunks >::iterator lChunksIt = lIPbusPacketInfo.getChunks().begin() ; lChunksIt != lIPbusPacketInfo.getChunks().end() ; ++lChunksIt ){
			//if the chunk will push the UDP packet over size, then add a new packet into the queue
			bool lSendSizeBad( mAccumulatedPackets.back().mCumulativeSendSize + lChunksIt->mSendSize > mMaxPacketLength );
			bool lReturnSizeBad( mAccumulatedPackets.back().mCumulativeReturnSize +  lChunksIt->mReturnSize > mMaxPacketLength );	
			if ( lSendSizeBad || lReturnSizeBad ){
				mAccumulatedPackets.push_back( tAccumulatedPacket() );
			}

			//if the packet is empty, add a leading BOT
			if( mAccumulatedPackets.back().mCumulativeSendSize == 0 ){
				mAccumulatedPackets.back().mSendBuffers.push_back( boost::asio::buffer( mByteOrderTransaction , 4 ) ) ; //note this needs to be &mByteOrderTransaction if using a single 32bit uint, rather than an array
				mAccumulatedPackets.back().mReplyBuffers.push_back( boost::asio::mutable_buffer( mBOTReplyHeader , 4 ) ) ;//note this needs to be &mBOTReplyHeader if using a single 32bit uint, rather than an array
				mAccumulatedPackets.back().mCumulativeSendSize=1;						
				mAccumulatedPackets.back().mCumulativeReturnSize=1;
			}

			std::size_t lSendPayloadSize = lChunksIt->mSendSize - lIPbusPacketInfo.SendHeaderSize();					
			mAccumulatedPackets.back().mSendBuffers.push_back( boost::asio::buffer( &lChunksIt->mTransactionHeader , 4 ) ) ;
			if(lIPbusPacketInfo.hasBaseAddress())	mAccumulatedPackets.back().mSendBuffers.push_back( boost::asio::buffer( &lChunksIt->mBaseAddress , 4 ) ) ;
			if( lSendPayloadSize )		mAccumulatedPackets.back().mSendBuffers.push_back( boost::asio::buffer( lChunksIt->mSendPtr , lSendPayloadSize<<2 ) ) ;
			mAccumulatedPackets.back().mCumulativeSendSize+=lChunksIt->mSendSize;					
			
			std::size_t lReturnPayloadSize = lChunksIt->mReturnSize - lIPbusPacketInfo.ReturnHeaderSize();
			mAccumulatedPackets.back().mReplyBuffers.push_back( boost::asio::mutable_buffer( &lChunksIt->mReplyHeaders.at(0) , 4 ) ) ; //IPbus HW access clients only talk to a single board, so always at 0
			if( lReturnPayloadSize ){
				mAccumulatedPackets.back().mReplyBuffers.push_back( boost::asio::mutable_buffer( lChunksIt->mValMemPtr.at(0) , lReturnPayloadSize<<2 ) ) ; //IPbus HW access clients only talk to a single board, so always at 0
			}
			mAccumulatedPackets.back().mCumulativeReturnSize+=lChunksIt->mReturnSize;	
		}		
		
	}
	
	void IPbusHwAccessPackingProtocol::PreDispatch(){
	
		for( tAccumulatedPackets::iterator lAccumulatedPacketIt = mAccumulatedPackets.begin() ; lAccumulatedPacketIt != mAccumulatedPackets.end() ; ++lAccumulatedPacketIt ){
			if( lAccumulatedPacketIt->mCumulativeSendSize < 8 ){
				uint32_t lSize(8-lAccumulatedPacketIt->mCumulativeSendSize);
				lAccumulatedPacketIt->mSendBuffers.push_back( boost::asio::buffer( mByteOrderTransaction , lSize<<2 ) );
				lAccumulatedPacketIt->mCumulativeSendSize+=lSize;					

				lAccumulatedPacketIt->mReplyBuffers.push_back( boost::asio::buffer( mBOTReplyHeader , lSize<<2 ) );							
				lAccumulatedPacketIt->mCumulativeReturnSize+=lSize;
			}
		}
		
	}

	bool IPbusHwAccessPackingProtocol::PostDispatch(){

		for( tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() ; lPacketInfoIt != mPacketInfo.end() ; ++lPacketInfoIt ){
		
			//check that, for each chunk, the reply header from each board matches the expected header
			for( std::deque< IPbusPacketInfo::tChunks >::const_iterator lChunkIt = lPacketInfoIt->getChunks().begin() ; lChunkIt != lPacketInfoIt->getChunks().end() ; ++lChunkIt ){
				//for( std::vector< boost::uint32_t >::const_iterator lReplyHeaderIt = lChunkIt->mReplyHeaders.begin() ; lReplyHeaderIt != lChunkIt->mReplyHeaders.end() ; ++lReplyHeaderIt ){
					if( lChunkIt->mReplyHeaders[0] != lChunkIt->mExpectedReplyHeader ){
						pantheios::log_ERROR( "Reply header (" , 
												pantheios::integer( lChunkIt->mReplyHeaders[0] , pantheios::fmt::fullHex | 10 ),
												") does not match expected (0x", 
												pantheios::integer( lChunkIt->mExpectedReplyHeader , pantheios::fmt::fullHex | 10 ),
												")!" );
						return false;
					}
				//}					
			}	
		
			//if the headers match, then mark the ReplyMemory as valid
			lPacketInfoIt->setAllValMemsValid();
		
		}							

		//clear everything ahead of the next fill cycle...
		mPacketInfo.clear();
		mAccumulatedPackets.clear();
		return true;
	}
			

	void IPbusHwAccessPackingProtocol::ReceiveHandler( const boost::system::error_code& aErrorCode , std::size_t aReplyLength , std::size_t& aReplyLengthRef , bool& aAwaitingCallBackRef , bool& aErrorRef ){
		//For direct hardware access the callback is trivial
		if( aErrorCode ){
			pantheios::log_ALERT( "Asynchronous call-back found BOOST ASIO error code with message \"" , aErrorCode.message() , "\"" );
			aErrorRef = true;
			return;
		}
				
		aReplyLengthRef = aReplyLength;
		aAwaitingCallBackRef = false;
	}
			
}
