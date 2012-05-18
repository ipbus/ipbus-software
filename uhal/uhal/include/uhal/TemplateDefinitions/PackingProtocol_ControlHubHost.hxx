

namespace uhal
{

	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >

ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion ,  IPbusProtocolVersion >::ControlHubHostPackingProtocol ( const uint32_t& aMaxPacketLength ) try :
		PackingProtocol(),
						mMaxPacketLength ( aMaxPacketLength ),
						mDeviceIDheader( 0 ),
						mErrorCode( 0 ),
						mTransactionId ( 0 )
	{
		int i;
		//We need to pad out the packets to compensate for the bug in the firmware
		//If we didn't then the mByteOrderTransaction could be a single 32bit uint (as it was previously)
		switch ( IPbusProtocolVersion )
		{
			case IPbus_1_2:
			case IPbus_1_3:
				for( i = 0 ; i != 8 ; ++i ){
					mByteOrderTransaction[i] = 0x10FE00F8;
				}
				break;
			case IPbus_1_4:
			case IPbus_2_0:
				for( i = 0 ; i != 8 ; ++i ){
					mByteOrderTransaction[i] = 0x200000FF;
				}
				break;
		}
		
		switch ( ControlHubHostPackingProtocolVersion )
		{
			case CHH_1:
				mDeviceIDsize = 4;
				break;
			case CHH_2:
			case CHH_3:
				mDeviceIDsize = 6;
				break;		
		}
		
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion ,  IPbusProtocolVersion >::~ControlHubHostPackingProtocol() {}

	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion ,  IPbusProtocolVersion >::pack ( IPbusPacketInfo& aIPbusPacketInfo , const uint64_t& aId )
	{
		try
		{
			aIPbusPacketInfo.setDeviceID ( aId );
			//In the list of current instructions, find the device that was last accessed.
			uint32_t lLastInstruction ( mLastInstruction[ aId ] );
			tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() + lLastInstruction;
			unsigned lCount=mPacketInfo.size();

			//determine whether two PacketInfos are identical (excluding device list)
			for ( ; lLastInstruction != lCount ; ++lLastInstruction, ++lPacketInfoIt )
			{
				if ( *lPacketInfoIt == aIPbusPacketInfo )
				{
					break;
				}
			}

			mLastInstruction[ aId ] = lLastInstruction+1;

			if ( lLastInstruction != lCount )
			{
				lPacketInfoIt->merge ( aIPbusPacketInfo );
			}
			else
			{
				mPacketInfo.push_back ( aIPbusPacketInfo );
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}



	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion ,  IPbusProtocolVersion >::PreDispatch()
	{
		try
		{
			for ( tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() ; lPacketInfoIt != mPacketInfo.end() ; ++lPacketInfoIt )
			{
				lPacketInfoIt->splitChunks< IPbusProtocolVersion > ( mMaxPacketLength , mTransactionId );
				// std::cout << "PacketInfo Addr = " << &(*lPacketInfoIt) << std::endl;
				// for( std::deque< IPbusPacketInfo::tChunks >::iterator lChunksIt = lPacketInfoIt->getChunks().begin() ; lChunksIt != lPacketInfoIt->getChunks().end() ; ++lChunksIt ){
				// std::cout << "\tChunk Addr = " << &(*lChunksIt);
				// std::cout << std::hex << "\tmExpectedReplyHeader = " << std::setfill('0') << std::setw(8) << lChunksIt->mExpectedReplyHeader;
				// // std::cout << "\tmReplyHeaders.size() = " << lChunksIt->mReplyHeaders.size();
				// std::cout << std::endl;
				// }
			}



			
			if( ControlHubHostPackingProtocolVersion == CHH_3 ){
			
			
				std::map< uint64_t , std::pair< tAccumulatedPacket* , uint32_t > > lAccumulatedPacketMap;
				tAccumulatedPacket* lAccumulatedPacket( NULL );
				
				std::vector< uint32_t* > lSendCounterPtrs;
				std::vector<uint32_t> lReturnCounters;
				
				uint32_t*  lSendCounterPtr;
				uint32_t*  lReturnCounterPtr;				
				
				
				for ( tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() ; lPacketInfoIt != mPacketInfo.end() ; ++lPacketInfoIt )
				{
					uint64_t lDeviceIndex = 0;
					for ( std::vector<uint64_t>::const_iterator lIt = lPacketInfoIt->getDeviceIDs().begin() ; lIt != lPacketInfoIt->getDeviceIDs().end() ; ++lIt , ++lDeviceIndex )
					{
						tChunkList* lReplyMapPtr = &mReplyMap[ *lIt ];
						std::map< uint64_t , std::pair< tAccumulatedPacket* , uint32_t > >::iterator lAccumulatedPacketIt = lAccumulatedPacketMap.find( *lIt );
						
						for ( std::deque< IPbusPacketInfo::tChunks >::iterator lChunksIt = lPacketInfoIt->getChunks().begin() ; lChunksIt != lPacketInfoIt->getChunks().end() ; ++lChunksIt )
						{					
							lReplyMapPtr->push_back ( std::make_pair ( lDeviceIndex , & ( *lChunksIt ) ) );

							bool lNewAccumulatedPacket( false );
						
							if( lAccumulatedPacketIt == lAccumulatedPacketMap.end() ){
								lNewAccumulatedPacket = true;
							}else{
								lAccumulatedPacket = lAccumulatedPacketIt->second.first;
								lSendCounterPtr = lSendCounterPtrs.at( lAccumulatedPacketIt->second.second );
								lReturnCounterPtr = &( lReturnCounters.at( lAccumulatedPacketIt->second.second ) );
								lNewAccumulatedPacket |= *lSendCounterPtr + lChunksIt->mSendSize > mMaxPacketLength;
								lNewAccumulatedPacket |= *lReturnCounterPtr + lChunksIt->mReturnSize > mMaxPacketLength;
								
								if( lNewAccumulatedPacket ){
									if ( *lSendCounterPtr < 8 )
									{
										uint32_t lSize ( 8-*lSendCounterPtr );
										//Add BOTs as padding
										lAccumulatedPacket->mSendBuffers.push_back ( boost::asio::buffer ( mByteOrderTransaction , lSize<<2 ) );
										*lSendCounterPtr+=lSize;
										*lReturnCounterPtr+=lSize;
									}
									lAccumulatedPacket->mCumulativeSendSize += (*lSendCounterPtr << 2);
									lAccumulatedPacket->mCumulativeReturnSize += (*lReturnCounterPtr << 2);
									*lSendCounterPtr = htons( *lSendCounterPtr );
								}								
							}
							
							if( lNewAccumulatedPacket ){
								mAccumulatedPackets.push_back ( tAccumulatedPacket() );
								lAccumulatedPacket = &mAccumulatedPackets.back();
								lAccumulatedPacket->mSendBuffers.push_back ( boost::asio::buffer ( & ( lAccumulatedPacket->mCumulativeSendSize ) , 4 ) );		
								
								lAccumulatedPacketMap[ *lIt ] = std::make_pair( lAccumulatedPacket , mCounters.size() );
								lAccumulatedPacketIt = lAccumulatedPacketMap.find( *lIt );

								mCounters.push_back(0);
								lSendCounterPtr = &mCounters.back();
								lSendCounterPtrs.push_back( lSendCounterPtr );
								
								lReturnCounters.push_back( 0 );
								lReturnCounterPtr = &lReturnCounters.back();
								
								lAccumulatedPacket->mSendBuffers.push_back ( boost::asio::buffer ( &(*lIt) , mDeviceIDsize ) );
								lAccumulatedPacket->mSendBuffers.push_back ( boost::asio::buffer ( lSendCounterPtr , 2 ) );	
								lAccumulatedPacket->mCumulativeSendSize += 8;
								//lAccumulatedPacket->mCumulativeReturnSize += 8;								

								lAccumulatedPacket->mSendBuffers.push_back ( boost::asio::buffer ( mByteOrderTransaction , 4 ) ) ; //note this needs to be &mByteOrderTransaction if using a single 32bit uint, rather than an array
								*lSendCounterPtr += 1;	
								*lReturnCounterPtr  += 1;	
							}
						
							lAccumulatedPacket->mSendBuffers.push_back ( boost::asio::buffer ( &lChunksIt->mTransactionHeader , 4 ) ) ;

							if ( lPacketInfoIt->hasBaseAddress() )
							{
								lAccumulatedPacket->mSendBuffers.push_back ( boost::asio::buffer ( &lChunksIt->mBaseAddress , 4 ) ) ;
							}

							std::size_t lSendPayloadSize = lChunksIt->mSendSize - lPacketInfoIt->SendHeaderSize();
							if ( lSendPayloadSize )
							{
								lAccumulatedPacket->mSendBuffers.push_back ( boost::asio::buffer ( lChunksIt->mSendPtr , lSendPayloadSize<<2 ) ) ;
							}

							*lSendCounterPtr += lChunksIt->mSendSize;	
							*lReturnCounterPtr += lChunksIt->mReturnSize;							

						}
					}
				}
				
					
				for( std::map< uint64_t , std::pair< tAccumulatedPacket* , uint32_t > >::iterator lAccumulatedPacketIt = lAccumulatedPacketMap.begin() ; 
																								lAccumulatedPacketIt != lAccumulatedPacketMap.end() ; 
																								++lAccumulatedPacketIt ){
					lAccumulatedPacket = lAccumulatedPacketIt->second.first;
					lSendCounterPtr = lSendCounterPtrs.at( lAccumulatedPacketIt->second.second );
					lReturnCounterPtr = &( lReturnCounters.at( lAccumulatedPacketIt->second.second ) );
					if ( *lSendCounterPtr < 8 )
					{
						uint32_t lSize ( 8-*lSendCounterPtr );
						//Add BOTs as padding
						lAccumulatedPacket->mSendBuffers.push_back ( boost::asio::buffer ( mByteOrderTransaction , lSize<<2 ) );
						*lSendCounterPtr+=lSize;
						*lReturnCounterPtr+=lSize;
					}
					lAccumulatedPacket->mCumulativeSendSize += (*lSendCounterPtr << 2);
					lAccumulatedPacket->mCumulativeReturnSize += (*lReturnCounterPtr << 2);
					
					*lSendCounterPtr = htons( *lSendCounterPtr );
				}	
			
			
				for ( tAccumulatedPackets::iterator lIt = mAccumulatedPackets.begin() ; lIt != mAccumulatedPackets.end() ; ++lIt ){
					lIt->mCumulativeSendSize = htonl ( lIt->mCumulativeSendSize );
					//simply put the registers for the 1st preamble into the default reply buffer
					lIt->mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mReplyByteCount , 4 ) ) ;
					lIt->mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mDeviceIDheader , mDeviceIDsize ) ) ;
					lIt->mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mErrorCode , 2 ) ) ;
				}			
			
				// tAccumulatedPackets::iterator lIt = mAccumulatedPackets.begin();
				// tAccumulatedPackets::iterator lIt2 = mAccumulatedPackets.begin();
				// lIt2++;
			
				// for ( tAccumulatedPackets::iterator lIt = mAccumulatedPackets.begin() ; lIt != mAccumulatedPackets.end() ; ++lIt ){
					// lIt->mCumulativeSendSize += lIt2->mCumulativeSendSize;
					// lIt->mCumulativeReturnSize += lIt2->mCumulativeReturnSize;
					// lIt->mSendBuffers.insert( lIt->mSendBuffers.end() , lIt2->mSendBuffers.begin() , lIt2->mSendBuffers.end() );
					// lIt->mReplyBuffers.insert( lIt->mReplyBuffers.end() , lIt2->mReplyBuffers.begin() , lIt2->mReplyBuffers.end() );
				// }
				
				// lIt->mCumulativeSendSize = htonl ( lIt->mCumulativeSendSize );
				// //simply put the registers for the 1st preamble into the default reply buffer
				// lIt->mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mReplyByteCount , 4 ) ) ;
				// lIt->mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mDeviceIDheader , mDeviceIDsize ) ) ;
				// lIt->mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mErrorCode , 2 ) ) ;
					
			}else{
			
				//some local variables to help
				uint32_t lSendCounter ( 0 );
				uint32_t lReplyCounter ( 0 );
				//for the control hub, everything goes in one Accumulated Packet
				mAccumulatedPackets.push_back ( tAccumulatedPacket() );
				tAccumulatedPacket& lAccumulatedPacket = mAccumulatedPackets.front();
				//iterators for data to be put into packet
				tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() , lPacketInfoLastIt = mPacketInfo.begin();
				std::deque< IPbusPacketInfo::tChunks >::iterator lChunksIt = lPacketInfoIt->getChunks().begin();
				bool lAddHeader ( true );
				bool lPacketInfoItUpdated ( false );
				lAccumulatedPacket.mSendBuffers.push_back ( boost::asio::buffer ( & ( lAccumulatedPacket.mCumulativeSendSize ) , 4 ) );					

				//send buffers
				do
				{
					if ( lAddHeader )
					{
						//We need a persistent counter word...
						mCounters.push_back ( 0 );
						lAccumulatedPacket.mSendBuffers.push_back ( boost::asio::buffer ( &mCounters.back() , 4 ) );
						lAccumulatedPacket.mCumulativeSendSize += 4;
					
						for( std::vector<uint64_t>::const_iterator lIt = lPacketInfoIt->getDeviceIDs().begin() ; lIt != lPacketInfoIt->getDeviceIDs().end() ; ++lIt ){
							lAccumulatedPacket.mSendBuffers.push_back ( boost::asio::buffer ( &(*lIt) , mDeviceIDsize ) );
						}
						lAccumulatedPacket.mCumulativeSendSize += (lPacketInfoIt->getDeviceIDs().size()*mDeviceIDsize);
						
						//Add leading BOT and initialize counters (always 1 for BOTs)
						lAccumulatedPacket.mSendBuffers.push_back ( boost::asio::buffer ( mByteOrderTransaction , 4 ) ) ; //note this needs to be &mByteOrderTransaction if using a single 32bit uint, rather than an array
						lSendCounter = 1;
						lReplyCounter = 1;
						lAddHeader = false;
					}

					std::size_t lSendPayloadSize = lChunksIt->mSendSize - lPacketInfoIt->SendHeaderSize();
					lAccumulatedPacket.mSendBuffers.push_back ( boost::asio::buffer ( &lChunksIt->mTransactionHeader , 4 ) ) ;

					if ( lPacketInfoIt->hasBaseAddress() )
					{
						lAccumulatedPacket.mSendBuffers.push_back ( boost::asio::buffer ( &lChunksIt->mBaseAddress , 4 ) ) ;
					}

					if ( lSendPayloadSize )
					{
						lAccumulatedPacket.mSendBuffers.push_back ( boost::asio::buffer ( lChunksIt->mSendPtr , lSendPayloadSize<<2 ) ) ;
					}

					lSendCounter+=lChunksIt->mSendSize;
					lReplyCounter+=lChunksIt->mReturnSize;

					if ( ( ++lChunksIt ) == lPacketInfoIt->getChunks().end() )
					{
						//move to first chunk of next lPacketInfo
						//note that lChunksIt will be invalid if lPacketInfoIt == mPacketInfo.end() and will segfault if you try to use it :o)
						lChunksIt = ( ++lPacketInfoIt )->getChunks().begin();
						lPacketInfoItUpdated = true;
					}

					if ( lPacketInfoIt == mPacketInfo.end() )
					{
						// we will really exit if this condition occurs, we just use "lAddHeader" to make sure that we finalize this part of the packet
						lAddHeader = true;
					}
					else
					{
						// if the next send packet will push the outgoing udp packet over size, add a new header/udp packet instead
						lAddHeader |= ( lSendCounter + lChunksIt->mSendSize > mMaxPacketLength );
						// if the next reply packet will push the reply udp packet over size, add a new header/udp packet instead
						lAddHeader |= ( lReplyCounter +  lChunksIt->mReturnSize > mMaxPacketLength );

						// if the device lists are different then add a new header
						if ( lPacketInfoItUpdated ) //if the lists are independent then test to see if the device lists differ
						{
							lAddHeader |= ( lPacketInfoIt->getDeviceIDs() != lPacketInfoLastIt->getDeviceIDs() );
						}
					}

					// finalize packets before we add the next header block or before we exit the loop
					if ( lAddHeader )
					{
						if ( lSendCounter < 8 )
						{
							uint32_t lSize ( 8-lSendCounter );
							//Add BOTs as padding
							lAccumulatedPacket.mSendBuffers.push_back ( boost::asio::buffer ( mByteOrderTransaction , lSize<<2 ) );
							lSendCounter+=lSize;
							lReplyCounter+=lSize;
						}

						//fill the 16bit counters we send to the control-hub host.
						//note this must always be big-endian, so use htonl!
						mCounters.back() = htonl ( ( ( lSendCounter<<16 ) &0xffff0000 ) | ( lPacketInfoLastIt->getDeviceIDs().size() &0x0000ffff ) );
						lAccumulatedPacket.mCumulativeSendSize += (lSendCounter*4);
						lAccumulatedPacket.mCumulativeReturnSize += ( lReplyCounter*lPacketInfoLastIt->getDeviceIDs().size()*4 );
					}

					if ( lPacketInfoItUpdated )
					{
						lPacketInfoLastIt=lPacketInfoIt;
						lPacketInfoItUpdated = false;
					}
				}
				while ( lPacketInfoIt != mPacketInfo.end() );

				//store list of packetinfo relevant for each device
				for ( lPacketInfoIt = mPacketInfo.begin() ; lPacketInfoIt != mPacketInfo.end() ; ++lPacketInfoIt )
				{
					uint64_t lDeviceIndex = 0;

					for ( std::vector<uint64_t>::const_iterator lIt = lPacketInfoIt->getDeviceIDs().begin() ; lIt != lPacketInfoIt->getDeviceIDs().end() ; ++lIt , ++lDeviceIndex )
					{
						tChunkList* lReplyMapPtr = &mReplyMap[ *lIt ];

						for ( std::deque< IPbusPacketInfo::tChunks >::iterator lChunksIt = lPacketInfoIt->getChunks().begin() ; lChunksIt != lPacketInfoIt->getChunks().end() ; ++lChunksIt )
						{
							lReplyMapPtr->push_back ( std::make_pair ( lDeviceIndex , & ( *lChunksIt ) ) );
						}
					}
				}

				//convert the send size into bytes instead of words and make it Big-Endian, since we are sending this value to the CHH
				lAccumulatedPacket.mCumulativeSendSize = htonl ( lAccumulatedPacket.mCumulativeSendSize );
				//simply put the registers for the 1st preamble into the default reply buffer
				lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mReplyByteCount , 4 ) ) ;
				
				if( ControlHubHostPackingProtocolVersion == CHH_1 ){
					lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mDeviceIDheader , mDeviceIDsize ) ) ;
				}else{
					lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mDeviceIDheader , mDeviceIDsize ) ) ;
					lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mErrorCode , 2 ) ) ;						
				}
				
			}

			
			mExpectedReplyType = ExpectPreamble;
			// std::cout << std::dec << "lAccumulatedPacket.mCumulativeSendSize (words) = " << (ntohl(lAccumulatedPacket.mCumulativeSendSize)>>2) << std::endl;
			// std::cout << std::dec << "lAccumulatedPacket.mCumulativeReturnSize (words) = " << lAccumulatedPacket.mCumulativeReturnSize << std::endl;
				
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion ,  IPbusProtocolVersion >::PostDispatch()
	{
		try
		{
			for ( tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() ; lPacketInfoIt != mPacketInfo.end() ; ++lPacketInfoIt )
			{
				lPacketInfoIt->setAllValMemsValid();
			}

			//clear everything ahead of the next fill cycle...
			mPacketInfo.clear();
			mAccumulatedPackets.clear();
			mReplyMap.clear();
			mLastInstruction.clear();
			mCounters.clear();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion ,  IPbusProtocolVersion >::ReceiveHandler ( const boost::system::error_code& aErrorCode , std::size_t aReplyLength , std::size_t& aReplyLengthRef , bool& aAwaitingCallBackRef , bool& aErrorRef )
	{
		try
		{
		
			tAccumulatedPacket& lAccumulatedPacket = mAccumulatedPackets.front();

			//For direct hardware access the callback is trivial
			if ( aErrorCode )
			{
				pantheios::log_ERROR ( "Asynchronous call-back found BOOST ASIO error code with message \"" , aErrorCode.message() , "\"" );
				aErrorRef = true;
				return;
			}

			if( mErrorCode ){
				pantheios::log_ERROR ( "Control Hub reported error code " , pantheios::integer( mErrorCode&0xF ) , " in transaction ID " , pantheios::integer( (mErrorCode>>4)&0xFFF , pantheios::fmt::fullHex | 6 ) );
				aErrorRef = true;
				return;
			}
			
			// #ifdef DEBUGGING
			for ( std::deque< boost::asio::mutable_buffer >::iterator lBufIt = lAccumulatedPacket.mReplyBuffers.begin() ; lBufIt != lAccumulatedPacket.mReplyBuffers.end() ; ++lBufIt )
			{
				pantheios::log_DEBUG ( ">>> ----------------" );
				std::size_t s1 = boost::asio::buffer_size ( *lBufIt );
				const uint32_t* p1 = boost::asio::buffer_cast<const uint32_t*> ( *lBufIt );

				std::string lExpected;
				switch ( mExpectedReplyType )
				{
					case ExpectPreamble :
						lExpected = " (Expecting Preamble)";
						break;
					case ExpectHeader :
						lExpected = " (Expecting Header)";
						break;
					case ExpectPayload :
						lExpected = " (Expecting Payload)";
						break;
				}				
				
				for ( unsigned int y=0; y!=s1>>2; ++y )
				{
					pantheios::log_DEBUG ( "RECEIVED " , pantheios::integer ( * ( p1++ ) , pantheios::fmt::fullHex | 10 ) , lExpected );
				}
				
				if( s1%4 ){
					pantheios::log_DEBUG ( "RECEIVED " , pantheios::integer ( *( p1 ) , pantheios::fmt::fullHex | 6 ) , lExpected );
				}
			}

			pantheios::log_DEBUG ( ">>> ----------------" );
			// #endif // DEBUGGING
			lAccumulatedPacket.mReplyBuffers.clear();

			switch ( mExpectedReplyType )
			{
				case ExpectPreamble :
					GotPreamble ( aReplyLength , aErrorRef );
					break;
				case ExpectHeader :
					aReplyLengthRef += aReplyLength;
					GotHeader ( aReplyLength , aErrorRef );
					break;
				case ExpectPayload :
					aReplyLengthRef += aReplyLength;
					GotPayload ( aReplyLength , aErrorRef );
					break;
			}

			aAwaitingCallBackRef = false;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			pantheios::log_ERROR ( "Cannot propagate an exception out of the thread, so setting the error flag instead." );
			//throw uhal::exception( aExc ); //CANNOT PROPAGATE EXCEPTION ACROSS THREADS, SET THE ERROR FLAG INSTEAD
			aErrorRef = true;
		}
	}



	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion ,  IPbusProtocolVersion >::GotPreamble ( std::size_t aReplyLength , bool& aError )
	{
		try
		{
			// pantheios::log_LOCATION();
			tAccumulatedPacket& lAccumulatedPacket = mAccumulatedPackets.front();
			if( ControlHubHostPackingProtocolVersion == CHH_1 ){
				mReplyByteCount = ntohl ( mReplyByteCount ) - 4 ;
			}else{
				mReplyByteCount = ntohl ( mReplyByteCount ) - 8 ;
			}
			mReplyMapIt = mReplyMap.find ( mDeviceIDheader );

			if ( mReplyMapIt == mReplyMap.end() )
			{
				std::stringstream lStr;
				lStr << std::hex << std::setfill('0');
				for( mReplyMapIt = mReplyMap.begin() ; mReplyMapIt !=mReplyMap.end() ; ++mReplyMapIt ){
					lStr << "\n0x" << std::setw(16) << mReplyMapIt->first;
				}
			
				pantheios::log_ERROR ( "DeviceID " ,  pantheios::integer ( mDeviceIDheader , pantheios::fmt::fullHex | 18 ) , " was returned but was not found in reply map. Known IDs are:" , lStr.str() );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				aError = true;
				return;
			}

			tChunkList* lChunkListPtr = & ( mReplyMapIt->second );
			uint32_t lDeviceIndex = lChunkListPtr->front().first;
			IPbusPacketInfo::tChunks* lChunkPtr = lChunkListPtr->front().second;
			mTransactionHeader = & ( lChunkPtr->mReplyHeaders.at ( lDeviceIndex ) );
			lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( mTransactionHeader , 4 ) ) ;
			// std::cout << "Next is Header" << std::endl;
			mExpectedReplyType = ExpectHeader;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}



	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion ,  IPbusProtocolVersion >::GotHeader ( std::size_t aReplyLength , bool& aError )
	{
		try
		{
			// pantheios::log_LOCATION();
			// pantheios::log_DEBUG ( "Header : " , pantheios::integer ( *mTransactionHeader , pantheios::fmt::fullHex | 10 ) , " = " , DebugIPbusHeader ( *mTransactionHeader ) );
			tAccumulatedPacket& lAccumulatedPacket = mAccumulatedPackets.front();
			mReplyByteCount -= aReplyLength;

			if ( ( *mTransactionHeader&0xF00000FF ) != 0x100000FC ) //BOT check
			{
				if ( *mTransactionHeader==0x00000000 )
				{
					pantheios::log_ERROR ( "Control-Hub Host reported timeout" );
					pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
					aError = true;
					return;
				}

				if ( *mTransactionHeader&0x00000003 )
				{
					pantheios::log_ERROR ( "Received result = bad" );
					pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
					aError = true;
					return;
				}

				tChunkList* lChunkListPtr = & ( mReplyMapIt->second );
				uint32_t lDeviceIndex = lChunkListPtr->front().first;
				IPbusPacketInfo::tChunks* lChunkPtr = lChunkListPtr->front().second;

				if ( *mTransactionHeader!=lChunkPtr->mExpectedReplyHeader )
				{
					pantheios::log_ERROR ( "Reply header ",
										   pantheios::integer ( *mTransactionHeader , pantheios::fmt::fullHex | 10 ),
										   " does not match expected " ,
										   pantheios::integer ( lChunkPtr->mExpectedReplyHeader , pantheios::fmt::fullHex | 10 ) ,
										   "!" );
					pantheios::log_ERROR ( "Received : " , DebugIPbusHeader< IPbusProtocolVersion > ( *mTransactionHeader ) );
					pantheios::log_ERROR ( "Expected : " , DebugIPbusHeader< IPbusProtocolVersion > ( lChunkPtr->mExpectedReplyHeader ) );
					pantheios::log_ERROR ( "Transaction history :" );

					for ( tAccumulatedPackets::const_iterator lAccumulatedPacketIt = mAccumulatedPackets.begin() ; lAccumulatedPacketIt != mAccumulatedPackets.end() ; ++lAccumulatedPacketIt )
					{
						for ( std::deque< boost::asio::const_buffer >::const_iterator lBufIt = lAccumulatedPacketIt->mSendBuffers.begin() ; lBufIt != lAccumulatedPacketIt->mSendBuffers.end() ; ++lBufIt )
						{
							pantheios::log_ERROR ( "-------------------" );
							std::size_t s1 = boost::asio::buffer_size ( *lBufIt );
							const boost::uint32_t* p1 = boost::asio::buffer_cast<const boost::uint32_t*> ( *lBufIt );

							for ( unsigned int y=0; y!=s1>>2; ++y )
							{
								pantheios::log_ERROR ( "SENT " , pantheios::integer ( * ( p1+y ) , pantheios::fmt::fullHex | 10 ) );
							}
						}

						for ( std::deque< boost::asio::mutable_buffer >::const_iterator lBufIt = lAccumulatedPacketIt->mReplyBuffers.begin() ; lBufIt != lAccumulatedPacketIt->mReplyBuffers.end() ; ++lBufIt )
						{
							pantheios::log_ERROR ( "-------------------" );
							std::size_t s1 = boost::asio::buffer_size ( *lBufIt );
							const boost::uint32_t* p1 = boost::asio::buffer_cast<const boost::uint32_t*> ( *lBufIt );

							for ( unsigned int y=0; y!=s1>>2; ++y )
							{
								pantheios::log_ERROR ( "RECEIVED " , pantheios::integer ( * ( p1+y ) , pantheios::fmt::fullHex | 10 ) );
							}
						}

						pantheios::log_ERROR ( "-------------------" );
					}

					pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
					aError = true;
					return;
				}

				if ( lChunkPtr->mReturnSize > 1 )
				{
					//there is a payload expected
					lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( lChunkPtr->mValMemPtr.at ( lDeviceIndex ) , ( lChunkPtr->mReturnSize-1 ) <<2 ) ) ;
					// std::cout << "Next is Payload" << std::endl;
					mExpectedReplyType = ExpectPayload;
				}
				else
				{
					//there is no payload expected
					lChunkListPtr->pop_front();

					if ( lChunkListPtr->size() == 0 )
					{
						mReplyMap.erase ( mReplyMapIt );
					}

					//are we done with this set of chained instructions?
					pantheios::log_DEBUG ( "mReplyByteCount = " , pantheios::integer ( mReplyByteCount ) );			
					if ( mReplyByteCount == 0 )
					{
						//new preamble follows
						lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mReplyByteCount , 4 ) ) ;
						if( ControlHubHostPackingProtocolVersion == CHH_1 ){
							lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mDeviceIDheader , mDeviceIDsize ) ) ;
						}else{
							lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mDeviceIDheader , mDeviceIDsize ) ) ;
							lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mErrorCode , 2 ) ) ;						
						}
						// std::cout << "Next is Preamble" << std::endl;
						mExpectedReplyType = ExpectPreamble;
					}
					else
					{
						//new header follows and this must be padding or from the next IPbusPacketInfo since we have already established that there is no payload
						if ( lChunkListPtr->size() == 0 )
						{
							mReplyMemory = &mTemp1;
							mTransactionHeader = &mTemp2;
						}
						else
						{
							lDeviceIndex = lChunkListPtr->front().first;
							lChunkPtr = lChunkListPtr->front().second;
							mReplyMemory = lChunkPtr->mValMemPtr.at ( lDeviceIndex );
							mTransactionHeader = & ( lChunkPtr->mReplyHeaders.at ( lDeviceIndex ) );
						}

						lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( mTransactionHeader , 4 ) ) ;
						// std::cout << "Next is Header" << std::endl;
						mExpectedReplyType = ExpectHeader; //unnecessary here, since we are already in this state, but included for clarity
					}
				}
			}
			else
			{
				if ( mReplyByteCount == 0 )
				{
					//new preamble follows
					lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mReplyByteCount , 4 ) ) ;
					if( ControlHubHostPackingProtocolVersion == CHH_1 ){
						lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mDeviceIDheader , mDeviceIDsize ) ) ;
					}else{
						lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mDeviceIDheader , mDeviceIDsize ) ) ;
						lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mErrorCode , 2 ) ) ;						
					}					// std::cout << "Next is Preamble" << std::endl;
					mExpectedReplyType = ExpectPreamble;
				}
				else
				{
					//new header follows and as this wasn't what we were expecting, we use the same Transaction header
					lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( mTransactionHeader , 4 ) ) ;
					// std::cout << "Next is Header" << std::endl;
					mExpectedReplyType = ExpectHeader; //unnecessary here, since we are already in this state, but included for clarity
				}
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion ,  IPbusProtocolVersion >::GotPayload ( std::size_t aReplyLength , bool& aError )
	{
		try
		{
			// pantheios::log_LOCATION();
			tAccumulatedPacket& lAccumulatedPacket = mAccumulatedPackets.front();
			mReplyByteCount -= aReplyLength;
			tChunkList* lChunkListPtr = & ( mReplyMapIt->second );
			uint32_t lDeviceIndex = lChunkListPtr->front().first;
			IPbusPacketInfo::tChunks* lChunkPtr = lChunkListPtr->front().second;

			if ( aReplyLength != ( lChunkPtr->mReturnSize-1 ) <<2 )
			{
				pantheios::log_ERROR ( "Wrong payload size returned" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				aError = true;
				return;
			}

			
			pantheios::log_DEBUG ( "mReplyByteCount = " , pantheios::integer ( mReplyByteCount ) );			
			if ( mReplyByteCount == 0 )
			{
				//new preamble follows
				lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mReplyByteCount , 4 ) ) ;
				if( ControlHubHostPackingProtocolVersion == CHH_1 ){
					lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mDeviceIDheader , mDeviceIDsize ) ) ;
				}else{
					lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mDeviceIDheader , mDeviceIDsize ) ) ;
					lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &mErrorCode , 2 ) ) ;						
				}
				// std::cout << "Next is Preamble" << std::endl;
				mExpectedReplyType = ExpectPreamble;
			}
			else
			{
				//new header follows
				lChunkListPtr->pop_front();

				if ( lChunkListPtr->size() == 0 )
				{
					mReplyMap.erase ( mReplyMapIt );
					//all that is left is BOTs and they will be ignored anyway...
					mReplyMemory = &mTemp1;
					mTransactionHeader = &mTemp2;
				}
				else
				{
					lDeviceIndex = lChunkListPtr->front().first;
					lChunkPtr = lChunkListPtr->front().second;
					mReplyMemory = lChunkPtr->mValMemPtr.at ( lDeviceIndex );
					mTransactionHeader = & ( lChunkPtr->mReplyHeaders.at ( lDeviceIndex ) );
				}

				lAccumulatedPacket.mReplyBuffers.push_back ( boost::asio::mutable_buffer ( mTransactionHeader , 4 ) ) ;
				// std::cout << "Next is Header" << std::endl;
				mExpectedReplyType = ExpectHeader;
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	inline const tAccumulatedPackets& ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::getAccumulatedPackets()
	{
		try
		{
			return mAccumulatedPackets;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	

	
}
