// #include "uhal/performance.hpp"
#include "uhal/TransportProtocol_UDP.hpp"

#include <sys/time.h>


#include <boost/lambda/lambda.hpp>

namespace uhal
{


UdpTransportProtocol::DispatchWorker::DispatchWorker ( UdpTransportProtocol& aUdpTransportProtocol , const std::string& aHostname , const std::string& aServiceOrPort , const uint32_t& aTimeoutPeriod ) try :
		mUdpTransportProtocol ( aUdpTransportProtocol ),
							  mIOservice ( boost::shared_ptr< boost::asio::io_service > ( new boost::asio::io_service() ) ),
							  mSocket ( boost::shared_ptr< boost::asio::ip::udp::socket > ( new boost::asio::ip::udp::socket ( *mIOservice , boost::asio::ip::udp::endpoint ( boost::asio::ip::udp::v4(), 0 ) ) ) ),
							  mEndpoint ( boost::shared_ptr< boost::asio::ip::udp::endpoint > ( new boost::asio::ip::udp::endpoint (
											  * boost::asio::ip::udp::resolver::iterator (
													  boost::asio::ip::udp::resolver ( *mIOservice ).resolve (
															  boost::asio::ip::udp::resolver::query ( boost::asio::ip::udp::v4() , aHostname , aServiceOrPort )
													  )
											  )
										  )
																							  )
										),
							mDeadlineTimer( *mIOservice ),
							mTimeoutPeriod( aTimeoutPeriod )
		{
			mDeadlineTimer.async_wait(boost::bind(&UdpTransportProtocol::DispatchWorker::CheckDeadline, this));
		}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}


	UdpTransportProtocol::DispatchWorker::~DispatchWorker()
	{
		try
		{
			if ( mSocket.unique() )
			{
				mSocket->close();
			}
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
		}
	}


	void UdpTransportProtocol::DispatchWorker::operator() ()
	{
#ifdef USE_UDP_MULTITHREADED

		try
		{
			bool lBuffersPending ( false );
			Buffers* lBuffers ( NULL );

			while ( true )
			{
				boost::this_thread::interruption_point();
				{
					boost::lock_guard<boost::mutex> lLock ( mUdpTransportProtocol.mMutex );
					lBuffersPending = ( mUdpTransportProtocol.mPendingSendBuffers.size() != 0 );
				}

				if ( lBuffersPending )
				{
					{
						boost::lock_guard<boost::mutex> lLock ( mUdpTransportProtocol.mMutex );
						lBuffers = mUdpTransportProtocol.mPendingSendBuffers.front();
					}
					Dispatch ( lBuffers );
					{
						boost::lock_guard<boost::mutex> lLock ( mUdpTransportProtocol.mMutex );
						mUdpTransportProtocol.mPendingSendBuffers.pop_front();
					}
				}
			}
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
			mUdpTransportProtocol.mAsynchronousException = new uhal::exception ( aExc );
		}
		catch ( boost::thread_interrupted& )
		{
			//This thread was interrupted by main thread, which is ok - we just exit
		}

#endif
	}






	void UdpTransportProtocol::DispatchWorker::Dispatch ( Buffers* aBuffers )
	{
		try
		{
			// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
			// Send data
			// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
			std::vector< boost::asio::const_buffer > lAsioSendBuffer;
			lAsioSendBuffer.push_back ( boost::asio::const_buffer ( aBuffers->getSendBuffer() , aBuffers->sendCounter() ) );
			log ( Debug() , "Sending " , Integer ( aBuffers->sendCounter() ) , " bytes" );

			mDeadlineTimer.expires_from_now(boost::posix_time::seconds(mTimeoutPeriod));
			
			mErrorCode = boost::asio::error::would_block;
			mSocket->async_send_to ( lAsioSendBuffer , *mEndpoint , boost::lambda::var(mErrorCode) = boost::lambda::_1 );
			do mIOservice->run_one(); while ( mErrorCode == boost::asio::error::would_block );
			
			if ( mErrorCode )
			{
				log ( Error() , "ASIO reported an error: " , mErrorCode.message() );
				ErrorInUdpCallback().throwFrom ( ThisLocation() );
			}

			// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
			// Read back replies
			// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
			std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
			std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( aBuffers->getReplyBuffer() );
			lAsioReplyBuffer.reserve ( lReplyBuffers.size() );

			for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
			{
				lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( lIt->first , lIt->second ) );
			}

			log ( Debug() , "Expecting " , Integer ( aBuffers->replyCounter() ) , " bytes in reply" );
			boost::asio::ip::udp::endpoint lEndpoint;

			
			mDeadlineTimer.expires_from_now(boost::posix_time::seconds(mTimeoutPeriod));
			mErrorCode = boost::asio::error::would_block;
			mSocket->async_receive_from ( lAsioReplyBuffer , lEndpoint , 0 , boost::lambda::var(mErrorCode) = boost::lambda::_1 );
			do mIOservice->run_one(); while ( mErrorCode == boost::asio::error::would_block );			
			
			if ( mErrorCode )
			{
				log ( Error() , "ASIO reported an error: " , mErrorCode.message() );
				ErrorInUdpCallback().throwFrom ( ThisLocation() );
			}

			if ( !mUdpTransportProtocol.mPackingProtocol->Validate ( aBuffers ) )
			{
				log ( Error() , "Validation function reported an error!" );
				IPbusValidationError ().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}



	void UdpTransportProtocol::DispatchWorker::CheckDeadline()
	{

		// Check whether the deadline has passed. We compare the deadline against
		// the current time since a new asynchronous operation may have moved the
		// deadline before this actor had a chance to run.
		if (mDeadlineTimer.expires_at() <= boost::asio::deadline_timer::traits_type::now())
		{
			// The deadline has passed. The socket is closed so that any outstanding
			// asynchronous operations are cancelled.
			mSocket->close();

			// There is no longer an active deadline. The expiry is set to positive
			// infinity so that the actor takes no action until a new deadline is set.
			mDeadlineTimer.expires_at(boost::posix_time::pos_infin);
		}

		// Put the actor back to sleep.
		mDeadlineTimer.async_wait(boost::bind(&UdpTransportProtocol::DispatchWorker::CheckDeadline, this));
	}

  
	

UdpTransportProtocol::UdpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , const uint32_t& aTimeoutPeriod ) try :
		TransportProtocol ( aTimeoutPeriod ),
						  mDispatchWorker ( boost::shared_ptr< DispatchWorker > ( new DispatchWorker ( *this , aHostname , aServiceOrPort , aTimeoutPeriod ) ) )
#ifdef USE_UDP_MULTITHREADED
						  , mDispatchThread ( boost::shared_ptr< boost::thread > ( new boost::thread ( *mDispatchWorker ) ) ),
						  mAsynchronousException ( NULL )
#endif
	{}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}


	UdpTransportProtocol::~UdpTransportProtocol()
	{
		try
		{
#ifdef USE_UDP_MULTITHREADED

			if ( mDispatchThread.unique() )
			{
				mDispatchThread->interrupt();
				//For some reason a call to join blocks when placed in the destructor (see Google) so we just let the thread detach as the Transport Protocol should only be killed at the end of the program anyway...
				//mDispatchThread->join();
			}

			{
				boost::lock_guard<boost::mutex> lLock ( mMutex );

				if ( mAsynchronousException )
				{
					delete mAsynchronousException;
					mAsynchronousException = NULL;
				}
			}

#endif
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
		}
	}




	void UdpTransportProtocol::Dispatch ( Buffers* aBuffers )
	{
		try
		{
#ifdef USE_UDP_MULTITHREADED
			{
				boost::lock_guard<boost::mutex> lLock ( mMutex );

				if ( mAsynchronousException )
				{
					mAsynchronousException->throwFrom ( ThisLocation() );
				}

				mPendingSendBuffers.push_back ( aBuffers );
			}
#else
			mDispatchWorker->Dispatch ( aBuffers );
#endif
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}



	void UdpTransportProtocol::Flush( )
	{
		try
		{
#ifdef USE_UDP_MULTITHREADED
			bool lContinue ( true );

			do
			{
				boost::lock_guard<boost::mutex> lLock ( mMutex );

				// kill time while pending buffers are emptied and check for exceptions on the thread...
				if ( mAsynchronousException )
				{
					mAsynchronousException->throwFrom ( ThisLocation() );
				}

				lContinue = (bool)( mPendingSendBuffers.size() );
			}
			while ( lContinue );

#endif
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}




}

