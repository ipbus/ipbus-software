
namespace uhal
{

	template < class PACKINGPROTOCOL >

TcpTransportProtocol< PACKINGPROTOCOL >::TcpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , PACKINGPROTOCOL& aPackingProtocol , uint32_t aTimeoutPeriod ) try :
		TransportProtocol(),
						  mHostname ( aHostname ),
						  mServiceOrPort ( aServiceOrPort ),
						  mPackingProtocol ( aPackingProtocol ),
						  mIOservice(),
						  mSocket ( NULL ),
						  mResolver ( NULL ),
						  mQuery ( NULL ),
						  mTimeOut ( aTimeoutPeriod ),
						  mDeadline ( mIOservice ),
						  mTimeoutFlag ( false )
	{
		mDeadline.expires_at ( boost::posix_time::pos_infin );
		CheckDeadline(); // Start the persistent actor that checks for deadline expiry.
		//I use lazy evaluation for the TCP socket - i.e. don't try making a connection until we actually use it
		//It doesn't have to be like this, the socket could be opened here instead, it just seemed like a nice idea...
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}


	template < class PACKINGPROTOCOL >
	TcpTransportProtocol< PACKINGPROTOCOL >::~TcpTransportProtocol()
	{
		try
		{
			if ( mQuery )
			{
				delete mQuery;
				mQuery = NULL;
			}

			if ( mResolver )
			{
				delete mResolver;
				mResolver = NULL;
			}

			if ( mSocket )
			{
				mSocket->close();
				delete mSocket;
				mSocket = NULL;
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			//throw uhal::exception( aExc ); MUST NOT THROW IN A DESTRUCTOR
		}
	}



	template < class PACKINGPROTOCOL >
	void TcpTransportProtocol< PACKINGPROTOCOL >::Dispatch()
	{
		try
		{
			//I use lazy evaluation here - i.e. don't try making a connection until we actually use it
			//It doesn't have to be like this, it just seemed like a nice idea...
			if ( !mSocket )
			{
				pantheios::log_NOTICE ( "First call to dispatch for '" , mHostname , "' port " , mServiceOrPort , ". Attempting to create TCP connection now." );
				mSocket = new boost::asio::ip::tcp::socket ( mIOservice ) ;
				mResolver = new boost::asio::ip::tcp::resolver ( mIOservice );
				mQuery = new boost::asio::ip::tcp::resolver::query ( mHostname , mServiceOrPort );
				mIterator = mResolver->resolve ( *mQuery );
				mSocket -> connect ( *mIterator );
				pantheios::log_NOTICE ( "TCP connection succeeded" );
			}

			for ( tAccumulatedPackets::const_iterator lAccumulatedPacketIt = mPackingProtocol.getAccumulatedPackets().begin() ; lAccumulatedPacketIt != mPackingProtocol.getAccumulatedPackets().end() ; ++lAccumulatedPacketIt )
			{
				//#ifdef DEBUGGING
				for ( std::deque< boost::asio::const_buffer >::const_iterator lBufIt = lAccumulatedPacketIt->mSendBuffers.begin() ; lBufIt != lAccumulatedPacketIt->mSendBuffers.end() ; ++lBufIt )
				{
					pantheios::log_DEBUG ( ">>> ----------------" );
					std::size_t s1 = boost::asio::buffer_size ( *lBufIt );
					const boost::uint32_t* p1 = boost::asio::buffer_cast<const boost::uint32_t*> ( *lBufIt );

					for ( unsigned int y=0; y!=s1>>2; ++y )
					{
						pantheios::log_DEBUG ( "SENDING  " , pantheios::integer ( * ( p1+y ) , pantheios::fmt::fullHex | 10 ) );
					}
				}

				pantheios::log_DEBUG ( ">>> ----------------" );
				//#endif // DEBUGGING

				if ( lAccumulatedPacketIt->mSendBuffers.size() == 0 )
				{
					continue;    //Sending empty packet will cause trouble, so don't!
				}

				//send
				boost::asio::write ( *mSocket , lAccumulatedPacketIt->mSendBuffers );
				//set deadline for reply
				mDeadline.expires_from_now ( mTimeOut );
				//wait for reply
				// Set up the variables that receive the result of the asynchronous
				// operation. The error code is set to would_block to signal that the
				// operation is incomplete. Asio guarantees that its asynchronous
				// operations will never fail with would_block, so any other value in
				// mErrorCode indicates completion.
				std::size_t lReplyLength ( 0 );
				bool lErrorFlag ( false );

				do
				{
					bool lAwaitingCallBack ( true );
					// Start the asynchronous operation itself. The ReceiveHandler function
					// used as a callback will update the mErrorCode and length variables.
					boost::asio::async_read (
						*mSocket ,
						lAccumulatedPacketIt->mReplyBuffers ,
						boost::bind (
							&PACKINGPROTOCOL::ReceiveHandler,
							&mPackingProtocol,
							boost::asio::placeholders::error,
							boost::asio::placeholders::bytes_transferred,
							boost::ref ( lReplyLength ),
							boost::ref ( lAwaitingCallBack ),
							boost::ref ( lErrorFlag )
						)
					);

					// Block until the asynchronous operation has completed.
					do
					{
						mIOservice.run_one();

						if ( mTimeoutFlag )
						{
							pantheios::log_ERROR ( "Caught Timeout flag in asynchronous callback. Converting to an exception." );
							pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
							throw TcpTimeout();
						}

						if ( lErrorFlag )
						{
							pantheios::log_ERROR ( "Caught Error flag in asynchronous callback. Converting to an exception." );
							pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
							throw ErrorInTcpCallback();
						}
					}
					while ( lAwaitingCallBack );
				}
				while ( ( lReplyLength>>2 ) != lAccumulatedPacketIt->mCumulativeReturnSize );

				//#ifdef DEBUGGING
				for ( std::deque< boost::asio::mutable_buffer >::const_iterator lBufIt = lAccumulatedPacketIt->mReplyBuffers.begin() ; lBufIt != lAccumulatedPacketIt->mReplyBuffers.end() ; ++lBufIt )
				{
					pantheios::log_DEBUG ( ">>> ----------------" );
					std::size_t s1 = boost::asio::buffer_size ( *lBufIt );
					const boost::uint32_t* p1 = boost::asio::buffer_cast<const boost::uint32_t*> ( *lBufIt );

					for ( unsigned int y=0; y!=s1>>2; ++y )
					{
						pantheios::log_DEBUG ( "RECEIVED " , pantheios::integer ( * ( p1+y ) , pantheios::fmt::fullHex | 10 ) );
					}
				}

				pantheios::log_DEBUG ( ">>> ----------------" );
				//#endif // DEBUGGING
				// std::cout << (mThis->mReplyLength>>2) << " vs. " << lAccumulatedPacketIt->mCumulativeReturnSize << std::endl;
				//check that it is the right length...
				// if( (mThis->mReplyLength>>2) != lAccumulatedPacketIt->mCumulativeReturnSize ){
				// //Throw exception - Since the hardware does not know how to break up packets, this must be an error
				// GenericException lExc(	"Return size does not match expected..." );
				// RAISE( lExc );
				// }
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}



	template < class PACKINGPROTOCOL >
	void TcpTransportProtocol< PACKINGPROTOCOL >::CheckDeadline()
	{
		try
		{
			// Check whether the deadline has passed. We compare the deadline against the current time since a new asynchronous operation may have moved the deadline before this actor had a chance to run.
			if ( mDeadline.expires_at() <= boost::asio::deadline_timer::traits_type::now() )
			{
				// The deadline has passed
				mDeadline.expires_at ( boost::posix_time::pos_infin );
				pantheios::log_ERROR ( "TCP Timeout on connection to '" , mHostname , "' port " , mServiceOrPort );
				throw TcpTimeout();
			}

			// Put the actor back to sleep.
			mDeadline.async_wait ( boost::bind ( &TcpTransportProtocol::CheckDeadline , this ) );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			pantheios::log_ERROR ( "Cannot propagate an exception out of the thread, so setting the timeout flag instead." );
			pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
			mTimeoutFlag = true;
		}
	}




}

