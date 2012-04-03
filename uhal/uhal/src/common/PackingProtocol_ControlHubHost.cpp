

#include "uhal/log.hpp"
#include "uhal/PackingProtocol_ControlHubHost.hpp"

namespace uhal
{

	ControlHubHostPackingProtocol::ControlHubHostPackingProtocol( const uint32_t& aMaxPacketLength ) : 
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

	ControlHubHostPackingProtocol::~ControlHubHostPackingProtocol(){}

	void ControlHubHostPackingProtocol::pack( IPbusPacketInfo& aIPbusPacketInfo , const uint32_t &aId ){
		aIPbusPacketInfo.setDeviceID( aId );
	
		//In the list of current instructions, find the device that was last accessed.
		uint32_t lLastInstruction( mLastInstruction[ aId ] );
		
		tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() + lLastInstruction;
	
		unsigned lCount=mPacketInfo.size();
		//determine whether two PacketInfos are identical (excluding device list)
		for ( ; lLastInstruction != lCount ; ++lLastInstruction, ++lPacketInfoIt )
			if( *lPacketInfoIt == aIPbusPacketInfo ) break;

		mLastInstruction[ aId ] = lLastInstruction;
			
		if(lLastInstruction != lCount) {
			lPacketInfoIt->merge( aIPbusPacketInfo );					
		} else {
			mPacketInfo.push_back( aIPbusPacketInfo );		
		}	
	}


	
	void ControlHubHostPackingProtocol::PreDispatch(){
		for( tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() ; lPacketInfoIt != mPacketInfo.end() ; ++lPacketInfoIt ){
			lPacketInfoIt->splitChunks( mMaxPacketLength , mTransactionId );
			// std::cout << "PacketInfo Addr = " << &(*lPacketInfoIt) << std::endl;
			// for( std::deque< IPbusPacketInfo::tChunks >::iterator lChunksIt = lPacketInfoIt->getChunks().begin() ; lChunksIt != lPacketInfoIt->getChunks().end() ; ++lChunksIt ){	
				// std::cout << "\tChunk Addr = " << &(*lChunksIt);
				// std::cout << std::hex << "\tmExpectedReplyHeader = " << std::setfill('0') << std::setw(8) << lChunksIt->mExpectedReplyHeader;
				// // std::cout << "\tmReplyHeaders.size() = " << lChunksIt->mReplyHeaders.size();
				// std::cout << std::endl;		
			// }
		}
	
		//some local variables to help
		uint32_t lSendCounter(0);	
		uint32_t lReplyCounter(0);		
		
		//for the control hub, everything goes in one Accumulated Packet
		mAccumulatedPackets.push_back( tAccumulatedPacket() );
		tAccumulatedPacket& lAccumulatedPacket = mAccumulatedPackets.front();

		
		//iterators for data to be put into packet
		tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() , lPacketInfoLastIt = mPacketInfo.begin();	
		std::deque< IPbusPacketInfo::tChunks >::iterator lChunksIt = lPacketInfoIt->getChunks().begin();
		
		bool lAddHeader( true );
		bool lPacketInfoItUpdated( false );
					
		lAccumulatedPacket.mSendBuffers.push_back( boost::asio::buffer( &(lAccumulatedPacket.mCumulativeSendSize) , 4 ) );
		
		//send buffers
		do{	
	
			if( lAddHeader ){
				//We need a persistent counter word...
				mCounters.push_back(0);
				lAccumulatedPacket.mSendBuffers.push_back( boost::asio::buffer( &mCounters.back() , 4 ) );
				lAccumulatedPacket.mSendBuffers.push_back( boost::asio::buffer( lPacketInfoIt->getDeviceIDs() ) );
				lAccumulatedPacket.mCumulativeSendSize += lPacketInfoIt->getDeviceIDs().size() + 1;
				
				//Add leading BOT and initialize counters (always 1 for BOTs)
				lAccumulatedPacket.mSendBuffers.push_back( boost::asio::buffer( mByteOrderTransaction , 4 ) ) ; //note this needs to be &mByteOrderTransaction if using a single 32bit uint, rather than an array
				lSendCounter = 1;
				lReplyCounter = 1;

				lAddHeader = false;
			}
			
			std::size_t lSendPayloadSize = lChunksIt->mSendSize - lPacketInfoIt->SendHeaderSize();
			lAccumulatedPacket.mSendBuffers.push_back( boost::asio::buffer( &lChunksIt->mTransactionHeader , 4 ) ) ;
			if(lPacketInfoIt->hasBaseAddress())	lAccumulatedPacket.mSendBuffers.push_back( boost::asio::buffer( &lChunksIt->mBaseAddress , 4 ) ) ;
			if( lSendPayloadSize )				lAccumulatedPacket.mSendBuffers.push_back( boost::asio::buffer( lChunksIt->mSendPtr , lSendPayloadSize<<2 ) ) ;
			lSendCounter+=lChunksIt->mSendSize;	
			
			lReplyCounter+=lChunksIt->mReturnSize;	
							
							
			if( (++lChunksIt) == lPacketInfoIt->getChunks().end() ){
				//move to first chunk of next lPacketInfo
				//note that lChunksIt will be invalid if lPacketInfoIt == mPacketInfo.end() and will segfault if you try to use it :o)
				lChunksIt = (++lPacketInfoIt)->getChunks().begin();
				lPacketInfoItUpdated = true;
			}
			
			
			
			if( lPacketInfoIt == mPacketInfo.end() ){
				// we will really exit if this condition occurs, we just use "lAddHeader" to make sure that we finalize this part of the packet
				lAddHeader = true;
			}else{

				// if the next send packet will push the outgoing udp packet over size, add a new header/udp packet instead
				lAddHeader |= ( lSendCounter + lChunksIt->mSendSize > mMaxPacketLength );
				// if the next reply packet will push the reply udp packet over size, add a new header/udp packet instead
				lAddHeader |= ( lReplyCounter +  lChunksIt->mReturnSize > mMaxPacketLength );	

				// if the device lists are different then add a new header
				if( lPacketInfoItUpdated ) //if the lists are independent then test to see if the device lists differ
					lAddHeader |= (lPacketInfoIt->getDeviceIDs() != lPacketInfoLastIt->getDeviceIDs());

			}

			
			// finalize packets before we add the next header block or before we exit the loop
			if( lAddHeader ){
				if( lSendCounter < 8 ){
					uint32_t lSize(8-lSendCounter);
					//Add BOTs as padding
					lAccumulatedPacket.mSendBuffers.push_back( boost::asio::buffer( mByteOrderTransaction , lSize<<2 ) );
					lSendCounter+=lSize;
					lReplyCounter+=lSize;
				}

				//fill the 16bit counters we send to the control-hub host.
				//note this must always be big-endian, so use htonl!
				mCounters.back() = htonl(((lSendCounter<<16)&0xffff0000) | (lPacketInfoLastIt->getDeviceIDs().size()&0x0000ffff));		
				
				lAccumulatedPacket.mCumulativeSendSize += lSendCounter;
				lAccumulatedPacket.mCumulativeReturnSize += (lReplyCounter*lPacketInfoLastIt->getDeviceIDs().size());
			}
			
			
			if( lPacketInfoItUpdated ){
				lPacketInfoLastIt=lPacketInfoIt;
				lPacketInfoItUpdated = false;
			}
			
		} while( lPacketInfoIt != mPacketInfo.end() );

		//store list of packetinfo relevant for each device
		for( lPacketInfoIt = mPacketInfo.begin() ; lPacketInfoIt != mPacketInfo.end() ; ++lPacketInfoIt ){
			uint32_t lDeviceIndex = 0;
			for( std::vector<uint32_t>::const_iterator lIt = lPacketInfoIt->getDeviceIDs().begin() ; lIt != lPacketInfoIt->getDeviceIDs().end() ; ++lIt , ++lDeviceIndex ){	
				tChunkList* lReplyMapPtr = &mReplyMap[ *lIt ];
				for( std::deque< IPbusPacketInfo::tChunks >::iterator lChunksIt = lPacketInfoIt->getChunks().begin() ; lChunksIt != lPacketInfoIt->getChunks().end() ; ++lChunksIt ){	
					lReplyMapPtr->push_back( std::make_pair( lDeviceIndex , &(*lChunksIt) ) );					
				}
			}			
		}
		
		//convert the send size into bytes instead of words and make it Big-Endian, since we are sending this value to the CHH 
		lAccumulatedPacket.mCumulativeSendSize = htonl( lAccumulatedPacket.mCumulativeSendSize<<2 );
						
		//simply put the registers for the 1st preamble into the default reply buffer
		lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( &mReplyByteCount , 4 ) ) ;
		lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( &mDeviceIDheader , 4 ) ) ;
		mExpectedReplyType = ExpectPreamble;
		
		// std::cout << std::dec << "lAccumulatedPacket.mCumulativeSendSize (words) = " << (ntohl(lAccumulatedPacket.mCumulativeSendSize)>>2) << std::endl;
		// std::cout << std::dec << "lAccumulatedPacket.mCumulativeReturnSize (words) = " << lAccumulatedPacket.mCumulativeReturnSize << std::endl;
	}

	bool ControlHubHostPackingProtocol::PostDispatch(){

		for( tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() ; lPacketInfoIt != mPacketInfo.end() ; ++lPacketInfoIt ){
			lPacketInfoIt->setAllValMemsValid();
		}							

		//clear everything ahead of the next fill cycle...
		mPacketInfo.clear();
		mAccumulatedPackets.clear();
		
		mReplyMap.clear();
		mLastInstruction.clear();
		mCounters.clear();	

		return true;
	}
			

	void ControlHubHostPackingProtocol::ReceiveHandler( const boost::system::error_code& aErrorCode , std::size_t aReplyLength , std::size_t& aReplyLengthRef , bool& aAwaitingCallBackRef , bool& aErrorRef ){

		tAccumulatedPacket& lAccumulatedPacket = mAccumulatedPackets.front();

		//For direct hardware access the callback is trivial
		if( aErrorCode ){
			pantheios::log_ALERT( "Asynchronous call-back found BOOST ASIO error code with message \"" , aErrorCode.message() , "\"" );
			aErrorRef = true;
			return;
		}

		
// #ifdef DEBUGGING
		for( std::deque< boost::asio::mutable_buffer >::iterator lBufIt = lAccumulatedPacket.mReplyBuffers.begin() ; lBufIt != lAccumulatedPacket.mReplyBuffers.end() ; ++lBufIt ){
			pantheios::log_DEBUG( ">>> ----------------" );
			std::size_t s1 = boost::asio::buffer_size( *lBufIt );
			const uint32_t* p1 = boost::asio::buffer_cast<const uint32_t*>( *lBufIt );
			for( unsigned int y=0; y!=s1>>2; ++y ){
				pantheios::log_DEBUG( "RECEIVED " , pantheios::integer( *(p1+y) , pantheios::fmt::fullHex | 10 ) );
			}
		}	
		pantheios::log_DEBUG( ">>> ----------------" );
// #endif // DEBUGGING

		lAccumulatedPacket.mReplyBuffers.clear();
		
		switch( mExpectedReplyType ){
			case ExpectPreamble :	
				GotPreamble( aReplyLength , aErrorRef );
				break;				
			case ExpectHeader :	
				aReplyLengthRef += aReplyLength;
				GotHeader( aReplyLength , aErrorRef );
				break;						
			case ExpectPayload :
				aReplyLengthRef += aReplyLength;			
				GotPayload( aReplyLength , aErrorRef );
				break;
		}
					
		aAwaitingCallBackRef = false;

	}
	
	

	void ControlHubHostPackingProtocol::GotPreamble( std::size_t aReplyLength , bool& aError ){
		tAccumulatedPacket& lAccumulatedPacket = mAccumulatedPackets.front();
		mReplyByteCount = ntohl( mReplyByteCount ) - 4 ;
				
		mReplyMapIt = mReplyMap.find( mDeviceIDheader );
		if( mReplyMapIt == mReplyMap.end() ){
			pantheios::log_ALERT( "DeviceID " ,  pantheios::integer( ntohl(mDeviceIDheader) , pantheios::fmt::fullHex | 10 ) , " returned was not found in reply map" );
			aError = true;
			return;
		}

		tChunkList* lChunkListPtr = &( mReplyMapIt->second );
		uint32_t lDeviceIndex = lChunkListPtr->front().first;
		IPbusPacketInfo::tChunks* lChunkPtr = lChunkListPtr->front().second;

		mTransactionHeader = &( lChunkPtr->mReplyHeaders.at(lDeviceIndex) );
		lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( mTransactionHeader , 4 ) ) ;					

		// std::cout << "Next is Header" << std::endl;
		mExpectedReplyType = ExpectHeader;
			
	}
	
	
	
	void ControlHubHostPackingProtocol::GotHeader( std::size_t aReplyLength , bool& aError ){
		tAccumulatedPacket& lAccumulatedPacket = mAccumulatedPackets.front();
	
		mReplyByteCount -= aReplyLength;
		
		if( (*mTransactionHeader&0xF00000FF) != 0x100000FC ){ //BOT check
			
			if ( *mTransactionHeader==0x00000000 ) {
				pantheios::log_ALERT( "Control-Hub Host reported timeout" );
				aError = true;
				return;
			}

			if ( *mTransactionHeader&0x00000003 ) {
				pantheios::log_ALERT( "Received result = bad" );
				aError = true;
				return;
			}				

			tChunkList* lChunkListPtr = &( mReplyMapIt->second );
			uint32_t lDeviceIndex = lChunkListPtr->front().first;
			IPbusPacketInfo::tChunks* lChunkPtr = lChunkListPtr->front().second;
			
			if ( *mTransactionHeader!=lChunkPtr->mExpectedReplyHeader ) {
				pantheios::log_ALERT( "Reply header ", 
										pantheios::integer( *mTransactionHeader , pantheios::fmt::fullHex | 10 ), 
										" does not match expected " ,
										pantheios::integer( lChunkPtr->mExpectedReplyHeader , pantheios::fmt::fullHex | 10 ) , 
										"!" );
				aError = true;
				return;
			}					
			
			if( lChunkPtr->mReturnSize > 1 ){
			
				//there is a payload expected
				lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( lChunkPtr->mValMemPtr.at(lDeviceIndex) , (lChunkPtr->mReturnSize-1)<<2 ) ) ;

				// std::cout << "Next is Payload" << std::endl;
				mExpectedReplyType = ExpectPayload;
			}else{						
				//there is no payload expected						
				lChunkListPtr->pop_front();
				
				if( lChunkListPtr->size() == 0 ) {
					mReplyMap.erase( mReplyMapIt );
				}
				
				//are we done with this set of chained instructions?
				if ( mReplyByteCount == 0 ) {
					//new preamble follows
					lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( &mReplyByteCount , 4 ) ) ;
					lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( &mDeviceIDheader , 4 ) ) ;

					// std::cout << "Next is Preamble" << std::endl;
					mExpectedReplyType = ExpectPreamble;
				} else {
					//new header follows and this must be padding or from the next IPbusPacketInfo since we have already established that there is no payload
					
					if( lChunkListPtr->size() == 0 ) {
						mReplyMemory = &mTemp1;
						mTransactionHeader = &mTemp2;
					}else{
						lDeviceIndex = lChunkListPtr->front().first;
						lChunkPtr = lChunkListPtr->front().second;
						
						mReplyMemory = lChunkPtr->mValMemPtr.at(lDeviceIndex);			
						mTransactionHeader = &( lChunkPtr->mReplyHeaders.at(lDeviceIndex) );
					}
					
					lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( mTransactionHeader , 4 ) ) ;
					// std::cout << "Next is Header" << std::endl;
					mExpectedReplyType = ExpectHeader; //unnecessary here, since we are already in this state, but included for clarity
					
				}
			}		
			
		}else{
		
			if ( mReplyByteCount == 0 ) {
				//new preamble follows
				lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( &mReplyByteCount , 4 ) ) ;
				lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( &mDeviceIDheader , 4 ) ) ;
				// std::cout << "Next is Preamble" << std::endl;
				mExpectedReplyType = ExpectPreamble;
			} else {
				//new header follows and as this wasn't what we were expecting, we use the same Transaction header
				lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( mTransactionHeader , 4 ) ) ;
				// std::cout << "Next is Header" << std::endl;
				mExpectedReplyType = ExpectHeader; //unnecessary here, since we are already in this state, but included for clarity
			}
		}
				
	}
	
	
	void ControlHubHostPackingProtocol::GotPayload( std::size_t aReplyLength , bool& aError ){
		tAccumulatedPacket& lAccumulatedPacket = mAccumulatedPackets.front();

		mReplyByteCount -= aReplyLength;

		tChunkList* lChunkListPtr = &( mReplyMapIt->second );
		uint32_t lDeviceIndex = lChunkListPtr->front().first;
		IPbusPacketInfo::tChunks* lChunkPtr = lChunkListPtr->front().second;
		
		if ( aReplyLength != (lChunkPtr->mReturnSize-1)<<2 ) {
			pantheios::log_ALERT( "Wrong payload size returned" );
			aError = true;
			return;
		}	
		
		if ( mReplyByteCount == 0 ) {
			//new preamble follows
			lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( &mReplyByteCount , 4 ) ) ;
			lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( &mDeviceIDheader , 4 ) ) ;
			// std::cout << "Next is Preamble" << std::endl;
			mExpectedReplyType = ExpectPreamble;
		} else {
			//new header follows

			lChunkListPtr->pop_front();
			
			if( lChunkListPtr->size() == 0 ) {
				mReplyMap.erase( mReplyMapIt );						
				//all that is left is BOTs and they will be ignored anyway...							
				mReplyMemory = &mTemp1;
				mTransactionHeader = &mTemp2;
			}else{
				lDeviceIndex = lChunkListPtr->front().first;
				lChunkPtr = lChunkListPtr->front().second;

				mReplyMemory = lChunkPtr->mValMemPtr.at(lDeviceIndex);			
				mTransactionHeader = &( lChunkPtr->mReplyHeaders.at(lDeviceIndex) );
			}

			lAccumulatedPacket.mReplyBuffers.push_back( boost::asio::mutable_buffer( mTransactionHeader , 4 ) ) ;
			// std::cout << "Next is Header" << std::endl;
			mExpectedReplyType = ExpectHeader; 

		}	
					
	}
			
			
}
