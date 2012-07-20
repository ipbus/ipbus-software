// #include "uhal/performance.hpp"
#include "uhal/TransportProtocol_UDP.hpp"

#ifdef USE_UDP_MULTITHREADED
#include <sys/time.h>
#else
#include <signal.h>

void UDPtimeout ( int signal )
{
	log ( uhal::Error() , "UDP Timeout" );
	log ( uhal::Error() , "Throwing at " , ThisLocation() );
	uhal::UdpTimeout().throwFrom ( ThisLocation() );
}

#endif

namespace uhal
{


UdpTransportProtocol::DispatchWorker::DispatchWorker ( UdpTransportProtocol& aUdpTransportProtocol , const std::string& aHostname , const std::string& aServiceOrPort , uint32_t aTimeoutPeriod ) try :
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
										)
		{}
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
			/*
							NOTE! For TCP, using the async_methods appears to give you a 10-20% hit in performance. I don't really understand why but I will stick with the synchronous methods for now...
			*/
			mSocket->send_to ( lAsioSendBuffer , *mEndpoint ); //, mErrorCode );

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
			/*
							NOTE! For TCP, using the async_methods appears to give you a 10-20% hit in performance. I don't really understand why but I will stick with the synchronous methods for now...
			*/
			mSocket->receive_from ( lAsioReplyBuffer , lEndpoint , 0 , mErrorCode );

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




UdpTransportProtocol::UdpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , uint32_t aTimeoutPeriod ) try :
		TransportProtocol ( aTimeoutPeriod ),
						  mDispatchWorker ( boost::shared_ptr< DispatchWorker > ( new DispatchWorker ( *this , aHostname , aServiceOrPort , aTimeoutPeriod ) ) )
#ifdef USE_UDP_MULTITHREADED
						  , mDispatchThread ( boost::shared_ptr< boost::thread > ( new boost::thread ( *mDispatchWorker ) ) ),
						  mAsynchronousException ( NULL )
#endif
	{
#ifndef USE_UDP_MULTITHREADED
		struct sigaction sa;
		memset ( &sa, 0, sizeof ( sa ) );
		sa.sa_handler = UDPtimeout;

		if ( sigaction ( SIGALRM, &sa, 0 ) < 0 )
		{
			log ( Error() , "Can't establish signal handler" );
			UdpTimeout().throwFrom ( ThisLocation() );
		}

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
					uhal::exception lExc ( *mAsynchronousException );
					log ( Error() , "Exception " , Quote ( lExc.what() ) , " caught at " , ThisLocation() );
					throw lExc;
				}

				mPendingSendBuffers.push_back ( aBuffers );

			}
#else
			alarm ( mTimeoutPeriod );
			mDispatchWorker->Dispatch ( aBuffers );
			alarm ( 0 );
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
			// 				timeval lStart, lEnd;
			// 				gettimeofday ( &lStart, NULL );
			time_t lStart, lEnd; //we don't need us resolution
			time ( &lStart );
			bool lContinue ( true );

			do
			{
				{
					boost::lock_guard<boost::mutex> lLock ( mMutex );

					// kill time while pending buffers are emptied and check for exceptions on the thread...
					if ( mAsynchronousException )
					{
						uhal::exception lExc ( *mAsynchronousException );
						log ( Error() , "Exception " , Quote ( lExc.what() ) , " caught at " , ThisLocation() );
						throw lExc;
					}

					lContinue = ( mPendingSendBuffers.size() );
				}
				// 					gettimeofday ( &lEnd, NULL );
				time ( &lEnd );
				/*					double lTimeTaken ( ((double)(lEnd.tv_sec-lStart.tv_sec)*1e6) + (double)(lEnd.tv_usec-lStart.tv_usec) );*/
				double lTimeTaken ( difftime ( lEnd , lStart ) );

				if ( lTimeTaken > mTimeoutPeriod )
				{
					log ( Error() , "UDP Timeout" );
					UdpTimeout().throwFrom ( ThisLocation() );
				}
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

