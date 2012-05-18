
namespace uhal
{

	template< eIPbusProtocolVersion IPbusProtocolVersion >

IPbusHwAccessPackingProtocol< IPbusProtocolVersion >::IPbusHwAccessPackingProtocol ( const uint32_t& aMaxPacketLength ) try :
		PackingProtocol(),
						mMaxPacketLength ( aMaxPacketLength ),
						mTransactionId ( 0 )
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
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	IPbusHwAccessPackingProtocol< IPbusProtocolVersion >::~IPbusHwAccessPackingProtocol() {}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	void IPbusHwAccessPackingProtocol< IPbusProtocolVersion >::pack ( IPbusPacketInfo& aIPbusPacketInfo , const uint64_t& aId )
	{
		try
		{
			//for local access we don't care if it already exists since we don't do any optimization, we just store it and add it to the queue
			mPacketInfo.push_back ( aIPbusPacketInfo );
			IPbusPacketInfo& lIPbusPacketInfo = mPacketInfo.back();
			lIPbusPacketInfo.setDeviceID ( aId ); //just a dummy device ID
			lIPbusPacketInfo.splitChunks< IPbusProtocolVersion > ( mMaxPacketLength , mTransactionId );

			if ( mAccumulatedPackets.size() == 0 )
			{
				mAccumulatedPackets.push_back ( tAccumulatedPacket() );
			}

			for ( std::deque< IPbusPacketInfo::tChunks >::iterator lChunksIt = lIPbusPacketInfo.getChunks().begin() ; lChunksIt != lIPbusPacketInfo.getChunks().end() ; ++lChunksIt )
			{
				//if the chunk will push the UDP packet over size, then add a new packet into the queue
				bool lSendSizeBad ( (mAccumulatedPackets.back().mCumulativeSendSize>>2) + lChunksIt->mSendSize > mMaxPacketLength );
				bool lReturnSizeBad ( (mAccumulatedPackets.back().mCumulativeReturnSize>>2) +  lChunksIt->mReturnSize > mMaxPacketLength );

				if ( lSendSizeBad || lReturnSizeBad )
				{
					mAccumulatedPackets.push_back ( tAccumulatedPacket() );
				}

				//if the packet is empty, add a leading BOT
				if ( mAccumulatedPackets.back().mCumulativeSendSize == 0 )
				{
					mAccumulatedPackets.back().mSendBuffers.push_back ( boost::asio::buffer ( mByteOrderTransaction , 4 ) ) ; //note this needs to be &mByteOrderTransaction if using a single 32bit uint, rather than an array
					mAccumulatedPackets.back().mReplyBuffers.push_back ( boost::asio::mutable_buffer ( mBOTReplyHeader , 4 ) ) ; //note this needs to be &mBOTReplyHeader if using a single 32bit uint, rather than an array
					mAccumulatedPackets.back().mCumulativeSendSize=4;
					mAccumulatedPackets.back().mCumulativeReturnSize=4;
				}

				std::size_t lSendPayloadSize = lChunksIt->mSendSize - lIPbusPacketInfo.SendHeaderSize();
				mAccumulatedPackets.back().mSendBuffers.push_back ( boost::asio::buffer ( &lChunksIt->mTransactionHeader , 4 ) ) ;

				if ( lIPbusPacketInfo.hasBaseAddress() )
				{
					mAccumulatedPackets.back().mSendBuffers.push_back ( boost::asio::buffer ( &lChunksIt->mBaseAddress , 4 ) ) ;
				}

				if ( lSendPayloadSize )
				{
					mAccumulatedPackets.back().mSendBuffers.push_back ( boost::asio::buffer ( lChunksIt->mSendPtr , lSendPayloadSize<<2 ) ) ;
				}

				mAccumulatedPackets.back().mCumulativeSendSize+=(lChunksIt->mSendSize<<2);
				std::size_t lReturnPayloadSize = lChunksIt->mReturnSize - lIPbusPacketInfo.ReturnHeaderSize();
				mAccumulatedPackets.back().mReplyBuffers.push_back ( boost::asio::mutable_buffer ( &lChunksIt->mReplyHeaders.at ( 0 ) , 4 ) ) ; //IPbus HW access clients only talk to a single board, so always at 0

				if ( lReturnPayloadSize )
				{
					mAccumulatedPackets.back().mReplyBuffers.push_back ( boost::asio::mutable_buffer ( lChunksIt->mValMemPtr.at ( 0 ) , lReturnPayloadSize<<2 ) ) ; //IPbus HW access clients only talk to a single board, so always at 0
				}

				mAccumulatedPackets.back().mCumulativeReturnSize+=(lChunksIt->mReturnSize<<2);
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	void IPbusHwAccessPackingProtocol< IPbusProtocolVersion >::PreDispatch()
	{
		try
		{
			for ( tAccumulatedPackets::iterator lAccumulatedPacketIt = mAccumulatedPackets.begin() ; lAccumulatedPacketIt != mAccumulatedPackets.end() ; ++lAccumulatedPacketIt )
			{
				if ( (lAccumulatedPacketIt->mCumulativeSendSize>>2) < 8 )
				{
					uint32_t lSize ( 8-(lAccumulatedPacketIt->mCumulativeSendSize>>2) );
					lAccumulatedPacketIt->mSendBuffers.push_back ( boost::asio::buffer ( mByteOrderTransaction , lSize<<2 ) );
					lAccumulatedPacketIt->mCumulativeSendSize+=(lSize<<2);
					lAccumulatedPacketIt->mReplyBuffers.push_back ( boost::asio::buffer ( mBOTReplyHeader , lSize<<2 ) );
					lAccumulatedPacketIt->mCumulativeReturnSize+=(lSize<<2);
				}
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	void IPbusHwAccessPackingProtocol< IPbusProtocolVersion >::PostDispatch()
	{
		try
		{
			for ( tIPbusPacketInfoStorage::iterator lPacketInfoIt = mPacketInfo.begin() ; lPacketInfoIt != mPacketInfo.end() ; ++lPacketInfoIt )
			{
				//check that, for each chunk, the reply header from each board matches the expected header
				for ( std::deque< IPbusPacketInfo::tChunks >::const_iterator lChunkIt = lPacketInfoIt->getChunks().begin() ; lChunkIt != lPacketInfoIt->getChunks().end() ; ++lChunkIt )
				{
					//for( std::vector< uint32_t >::const_iterator lReplyHeaderIt = lChunkIt->mReplyHeaders.begin() ; lReplyHeaderIt != lChunkIt->mReplyHeaders.end() ; ++lReplyHeaderIt ){
					if ( lChunkIt->mReplyHeaders[0] != lChunkIt->mExpectedReplyHeader )
					{
						pantheios::log_ERROR ( "Reply header " ,
											   pantheios::integer ( lChunkIt->mReplyHeaders[0] , pantheios::fmt::fullHex | 10 ),
											   " does not match expected ",
											   pantheios::integer ( lChunkIt->mExpectedReplyHeader , pantheios::fmt::fullHex | 10 ),
											   "!" );
						pantheios::log_ERROR ( "Received : " , DebugIPbusHeader< IPbusProtocolVersion > ( lChunkIt->mReplyHeaders[0] ) );
						pantheios::log_ERROR ( "Expected : " , DebugIPbusHeader< IPbusProtocolVersion > ( lChunkIt->mExpectedReplyHeader ) );
						pantheios::log_ERROR ( "Transaction history :" );

						for ( tAccumulatedPackets::const_iterator lAccumulatedPacketIt = mAccumulatedPackets.begin() ; lAccumulatedPacketIt != mAccumulatedPackets.end() ; ++lAccumulatedPacketIt )
						{
							for ( std::deque< boost::asio::const_buffer >::const_iterator lBufIt = lAccumulatedPacketIt->mSendBuffers.begin() ; lBufIt != lAccumulatedPacketIt->mSendBuffers.end() ; ++lBufIt )
							{
								pantheios::log_ERROR ( "-------------------" );
								std::size_t s1 = boost::asio::buffer_size ( *lBufIt );
								const uint32_t* p1 = boost::asio::buffer_cast<const uint32_t*> ( *lBufIt );

								for ( unsigned int y=0; y!=s1>>2; ++y )
								{
									pantheios::log_ERROR ( "SENT " , pantheios::integer ( * ( p1+y ) , pantheios::fmt::fullHex | 10 ) );
								}
							}

							for ( std::deque< boost::asio::mutable_buffer >::const_iterator lBufIt = lAccumulatedPacketIt->mReplyBuffers.begin() ; lBufIt != lAccumulatedPacketIt->mReplyBuffers.end() ; ++lBufIt )
							{
								pantheios::log_ERROR ( "-------------------" );
								std::size_t s1 = boost::asio::buffer_size ( *lBufIt );
								const uint32_t* p1 = boost::asio::buffer_cast<const uint32_t*> ( *lBufIt );

								for ( unsigned int y=0; y!=s1>>2; ++y )
								{
									pantheios::log_ERROR ( "RECEIVED " , pantheios::integer ( * ( p1+y ) , pantheios::fmt::fullHex | 10 ) );
								}
							}

							pantheios::log_ERROR ( "-------------------" );
						}

						pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
						throw ReplyHeaderExpectationFailure();
					}

					//}
				}

				//if the headers match, then mark the ReplyMemory as valid
				lPacketInfoIt->setAllValMemsValid();
			}

			//clear everything ahead of the next fill cycle...
			mPacketInfo.clear();
			mAccumulatedPackets.clear();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	void IPbusHwAccessPackingProtocol< IPbusProtocolVersion >::ReceiveHandler ( const boost::system::error_code& aErrorCode , std::size_t aReplyLength , std::size_t& aReplyLengthRef , bool& aAwaitingCallBackRef , bool& aErrorRef )
	{
		try
		{
			//For direct hardware access the callback is trivial
			if ( aErrorCode )
			{
				pantheios::log_ERROR ( "Asynchronous call-back found BOOST ASIO error code with message \"" , aErrorCode.message() , "\"" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				aErrorRef = true;
				return;
			}

			aReplyLengthRef = aReplyLength;
			aAwaitingCallBackRef = false;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			pantheios::log_ERROR ( "Cannot propagate an exception out of the thread, so setting the error flag instead." );
			//throw uhal::exception( aExc ); //CANNOT PROPAGATE EXCEPTION ACROSS THREADS, SET THE ERROR FLAG INSTEAD
			aErrorRef=true;
		}
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	inline const tAccumulatedPackets& IPbusHwAccessPackingProtocol< IPbusProtocolVersion >::getAccumulatedPackets()
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


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	template< int LogLevel >
	void IPbusHwAccessPackingProtocol< IPbusProtocolVersion >::debug ( const pantheios::level< LogLevel >& aLogLevel )
	{
		if ( mAccumulatedPackets.size() == 0 )
		{
			pantheios::log ( aLogLevel , "No entries in Accumulated Packet list" );
			return;
		}

		if ( mPacketInfo.size() == 0 )
		{
			pantheios::log ( aLogLevel , "No entries in Packet Info list" );
			return;
		}

		std::stringstream lStr;
		lStr << std::hex << std::setfill ( '0' );
		lStr << "+---------+------------+------------+------------+---------------+---------+\n" ;
		lStr << "|         |   Packet   | Queue that |  Raw Data  |   Packetized  |         |\n" ;
		lStr << "|         | To Be Sent |  was sent  |  Received  | data received |         |\n" ;
		bool lIPbusChunkGood ( false );
		tIPbusPacketInfoStorage::iterator lIPbusPacketIt;
		std::deque< IPbusPacketInfo::tChunks >::iterator lChunkIt;
		lIPbusPacketIt = mPacketInfo.begin();

		if ( lIPbusPacketIt != mPacketInfo.end() )
		{
			lChunkIt = lIPbusPacketIt->getChunks().begin();

			if ( lChunkIt != lIPbusPacketIt->getChunks().end() )
			{
				lIPbusChunkGood = true;
			}
		}

		bool lAccumulatedSendGood ( false );
		bool lAccumulatedReplyGood ( false );
		tAccumulatedPackets::const_iterator lAccumulatedPacketIt ;
		std::deque< boost::asio::const_buffer >::const_iterator lSendBufIt;
		std::size_t lSendBufSize ( 0 );
		const uint32_t* lSendBufStart ( NULL );
		std::deque< boost::asio::mutable_buffer >::const_iterator lReplyBufIt;
		std::size_t lReplyBufSize ( 0 );
		const uint32_t* lReplyBufStart ( NULL );
		lAccumulatedPacketIt = mAccumulatedPackets.begin();

		if ( lAccumulatedPacketIt != mAccumulatedPackets.end() )
		{
			lSendBufIt = lAccumulatedPacketIt->mSendBuffers.begin();

			if ( lSendBufIt != lAccumulatedPacketIt->mSendBuffers.end() )
			{
				lAccumulatedSendGood = true;
			}

			lReplyBufIt = lAccumulatedPacketIt->mReplyBuffers.begin();

			if ( lReplyBufIt != lAccumulatedPacketIt->mReplyBuffers.end() )
			{
				lAccumulatedReplyGood = true;
			}
		}

		enum { header , address , payload } lState ( header );
		std::size_t lCounter ( 0 );
		bool lMatch ( false );

		while ( lIPbusChunkGood || lAccumulatedSendGood || lAccumulatedReplyGood )
		{
			if ( lAccumulatedSendGood )
			{
				lSendBufSize = boost::asio::buffer_size ( *lSendBufIt ) >> 2;
				lSendBufStart = boost::asio::buffer_cast<const uint32_t*> ( *lSendBufIt );
			}

			if ( lAccumulatedReplyGood )
			{
				lReplyBufSize = boost::asio::buffer_size ( *lReplyBufIt ) >> 2;
				lReplyBufStart = boost::asio::buffer_cast<const uint32_t*> ( *lReplyBufIt );
			}

			switch ( lState )
			{
				case header:
					lStr << "+---------+------------+------------+------------+---------------+---------+\n" ;
					lStr << "| Header  | ";
					lMatch = false;

					if ( lIPbusChunkGood && lAccumulatedSendGood )
					{
						if ( *lSendBufStart == lChunkIt->mTransactionHeader )
						{
							lMatch = true;
						}
					}

					if ( lMatch )
					{
						lStr << "0x" << std::setw ( 8 ) << lChunkIt->mTransactionHeader;
					}
					else
					{
						lStr << std::string ( 10 , ' ' );
					}

					lStr << " | ";

					if ( lAccumulatedSendGood )
					{
						lStr << "0x" << std::setw ( 8 ) << *lSendBufStart;
					}
					else
					{
						lStr << std::string ( 10 , ' ' );
					}

					lStr << " | ";

					if ( lAccumulatedReplyGood )
					{
						lStr << "0x" << std::setw ( 8 ) << *lReplyBufStart;
					}
					else
					{
						lStr << std::string ( 10 , ' ' );
					}

					lStr << " |  ";

					if ( lMatch )
					{
						lStr << "0x" << std::setw ( 8 ) << lChunkIt->mReplyHeaders[0];
					}
					else
					{
						lStr << std::string ( 10 , ' ' );
					}

					lStr << "   |  Header | Sent " << DebugIPbusHeader <IPbusProtocolVersion> ( *lSendBufStart ) << "\n";

					if ( lMatch )
					{
						if ( lIPbusPacketIt->hasBaseAddress() )
						{
							lState = address;
						}
						else
						{
							lState = payload;
						}
					}

					if ( lAccumulatedPacketIt != mAccumulatedPackets.end() )
					{
						if ( ++lSendBufIt == lAccumulatedPacketIt->mSendBuffers.end() )
						{
							lAccumulatedSendGood = false;
						}

						if ( ++lReplyBufIt == lAccumulatedPacketIt->mReplyBuffers.end() )
						{
							lAccumulatedReplyGood = false;
						}

						if ( !lAccumulatedSendGood && !lAccumulatedReplyGood )
						{
							if ( ++lAccumulatedPacketIt != mAccumulatedPackets.end() )
							{
								lSendBufIt = lAccumulatedPacketIt->mSendBuffers.begin();

								if ( lSendBufIt != lAccumulatedPacketIt->mSendBuffers.end() )
								{
									lAccumulatedSendGood = true;
								}

								lReplyBufIt = lAccumulatedPacketIt->mReplyBuffers.begin();

								if ( lReplyBufIt != lAccumulatedPacketIt->mReplyBuffers.end() )
								{
									lAccumulatedReplyGood = true;
								}
							}
						}
					}

					break;
				case address:
					lStr << "| Address | ";

					if ( lIPbusChunkGood )
					{
						lStr << "0x" << std::setw ( 8 ) << lChunkIt->mBaseAddress;
					}
					else
					{
						lStr << std::string ( 10 , ' ' );
					}

					lStr << " | ";

					if ( lAccumulatedSendGood )
					{
						lStr << "0x" << std::setw ( 8 ) << *lSendBufStart;
					}
					else
					{
						lStr << std::string ( 10 , ' ' );
					}

					lStr << " | " << std::string ( 10 , ' ' )  << " |  " << std::string ( 10 , ' ' ) << "   |         |\n";

					if ( lChunkIt->mSendSize>2 || lChunkIt->mReturnSize>1 )
					{
						lState = payload;
					}
					else
					{
						lState = header;
						lIPbusChunkGood = false;

						if ( lIPbusPacketIt != mPacketInfo.end() )
						{
							lChunkIt = lIPbusPacketIt->getChunks().begin();

							if ( lChunkIt != lIPbusPacketIt->getChunks().end() )
							{
								lIPbusChunkGood = true;
							}
						}
					}

					if ( lAccumulatedPacketIt != mAccumulatedPackets.end() )
					{
						if ( ++lSendBufIt == lAccumulatedPacketIt->mSendBuffers.end() )
						{
							lAccumulatedSendGood = false;
						}

						if ( !lAccumulatedSendGood )
						{
							if ( ++lAccumulatedPacketIt != mAccumulatedPackets.end() )
							{
								lSendBufIt = lAccumulatedPacketIt->mSendBuffers.begin();

								if ( lSendBufIt != lAccumulatedPacketIt->mSendBuffers.end() )
								{
									lAccumulatedSendGood = true;
								}
							}
						}
					}

					break;
				case payload:

					if ( ( lCounter < lChunkIt->mSendSize-2 ) || ( lCounter < lChunkIt->mReturnSize-1 ) || ( lCounter < lSendBufSize ) || ( lCounter < lReplyBufSize ) )
					{
						lStr << "| Payload | ";

						if ( lIPbusChunkGood )
						{
							if ( lCounter < lChunkIt->mSendSize-2 )
							{
								lStr << "0x" << std::setw ( 8 ) << * ( lChunkIt->mSendPtr + lCounter ) ;
							}
							else
							{
								lStr << std::string ( 10 , ' ' );
							}
						}
						else
						{
							lStr << std::string ( 10 , ' ' );
						}

						lStr << " | ";

						if ( lAccumulatedSendGood )
						{
							if ( lCounter < lSendBufSize )
							{
								lStr << "0x" << std::setw ( 8 ) << * ( lSendBufStart + lCounter );
							}
							else
							{
								lStr << std::string ( 10 , ' ' );
							}
						}
						else
						{
							lStr << std::string ( 10 , ' ' );
						}

						lStr << " | ";

						if ( lAccumulatedReplyGood )
						{
							if ( lCounter < lReplyBufSize )
							{
								lStr << "0x" << std::setw ( 8 ) << * ( lReplyBufStart + lCounter );
							}
							else
							{
								lStr << std::string ( 10 , ' ' );
							}
						}
						else
						{
							lStr << std::string ( 10 , ' ' );
						}

						lStr << " |  ";

						if ( lIPbusChunkGood )
						{
							if ( lCounter < lChunkIt->mReturnSize-1 )
							{
								lStr << "0x" << std::setw ( 8 ) << * ( lChunkIt->mValMemPtr[0] + lCounter )  << "   | Payload |\n";
							}
							else
							{
								lStr << std::string ( 10 , ' ' ) << "   | " << std::string ( 7 , ' ' ) << " |\n";
							}
						}
						else
						{
							lStr << std::string ( 10 , ' ' ) << "   | " << std::string ( 7 , ' ' ) << " |\n";
						}

						lCounter++;
					}
					else
					{
						lCounter = 0;
						lState = header;

						if ( lIPbusPacketIt != mPacketInfo.end() )
						{
							if ( ++lChunkIt == lIPbusPacketIt->getChunks().end() )
							{
								lIPbusChunkGood = false;
							}

							if ( !lIPbusChunkGood )
							{
								if ( ++lIPbusPacketIt != mPacketInfo.end() )
								{
									lChunkIt = lIPbusPacketIt->getChunks().begin();

									if ( lChunkIt != lIPbusPacketIt->getChunks().end() )
									{
										lIPbusChunkGood = true;
									}
								}
							}
						}

						if ( lAccumulatedPacketIt != mAccumulatedPackets.end() )
						{
							if ( ++lSendBufIt == lAccumulatedPacketIt->mSendBuffers.end() )
							{
								lAccumulatedSendGood = false;
							}

							if ( ++lReplyBufIt == lAccumulatedPacketIt->mReplyBuffers.end() )
							{
								lAccumulatedReplyGood = false;
							}

							if ( !lAccumulatedSendGood && !lAccumulatedReplyGood )
							{
								if ( ++lAccumulatedPacketIt != mAccumulatedPackets.end() )
								{
									lSendBufIt = lAccumulatedPacketIt->mSendBuffers.begin();

									if ( lSendBufIt != lAccumulatedPacketIt->mSendBuffers.end() )
									{
										lAccumulatedSendGood = true;
									}

									lReplyBufIt = lAccumulatedPacketIt->mReplyBuffers.begin();

									if ( lReplyBufIt != lAccumulatedPacketIt->mReplyBuffers.end() )
									{
										lAccumulatedReplyGood = true;
									}
								}
							}
						}
					}

					break;
			};
		}

		lStr << "+---------+------------+------------+------------+---------------+---------+\n" ;
		pantheios::log ( aLogLevel , "\n" , lStr.str() );
	}

}



