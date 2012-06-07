#include "uhal/performance.hpp"
#include "uhal/TransportProtocol_TCP.hpp"

// #include <boost/lambda/bind.hpp>
// #include <boost/lambda/lambda.hpp>

#include <sys/time.h>

namespace uhal
{
	

	TcpTransportProtocol::DispatchWorker::DispatchWorker( TcpTransportProtocol& aTcpTransportProtocol , const std::string& aHostname , const std::string& aServiceOrPort , uint32_t aTimeoutPeriod ) try :
							  mTcpTransportProtocol( aTcpTransportProtocol ),
							  mIOservice( boost::shared_ptr< boost::asio::io_service >( new boost::asio::io_service() ) ),
							  mSocket ( boost::shared_ptr< boost::asio::ip::tcp::socket >( new boost::asio::ip::tcp::socket( *mIOservice ) ) ) //,
// 							  mTimeOut ( aTimeoutPeriod ) ,
// 							  mDeadline ( boost::shared_ptr< boost::asio::deadline_timer > ( new boost::asio::deadline_timer( *mIOservice ) ) )
		{
			pantheios::log_NOTICE ( "Attempting to create TCP connection to '" , aHostname , "' port " , aServiceOrPort , "." );
						
			boost::asio::connect ( *mSocket ,
							   boost::asio::ip::tcp::resolver::iterator (
								   boost::asio::ip::tcp::resolver ( *mIOservice ).resolve (
									   boost::asio::ip::tcp::resolver::query ( aHostname , aServiceOrPort )
								   )
							   )
							 );
			mSocket->set_option ( boost::asio::ip::tcp::no_delay ( true ) );

// 			mDeadline->expires_at ( boost::posix_time::pos_infin );
// 			CheckDeadline(); // Start the persistent actor that checks for deadline expiry.

			pantheios::log_NOTICE ( "TCP connection succeeded" );

			
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
		

	TcpTransportProtocol::DispatchWorker::~DispatchWorker()	{
			try
			{
				if( mSocket.unique() ){
					mSocket->close();
				}
			}
			catch ( const std::exception& aExc )
			{
				pantheios::log_EXCEPTION ( aExc );
			}
	}


	void TcpTransportProtocol::DispatchWorker::operator() (){
		#ifdef USE_TCP_MULTITHREADED
			try 
			{
				bool lBuffersPending( false );
				Buffers* lBuffers( NULL );
			
				while( true ){
					
					boost::this_thread::interruption_point();
					
					{ boost::lock_guard<boost::mutex> lock(mTcpTransportProtocol.mMutex);
					lBuffersPending = ( mTcpTransportProtocol.mPendingSendBuffers.size() != 0 );
					}
	
					if( lBuffersPending ){
						{ boost::lock_guard<boost::mutex> lock(mTcpTransportProtocol.mMutex);
						lBuffers = mTcpTransportProtocol.mPendingSendBuffers.front();
						}
					
						Dispatch( lBuffers );
	
						{ boost::lock_guard<boost::mutex> lock(mTcpTransportProtocol.mMutex);
						mTcpTransportProtocol.mPendingSendBuffers.pop_front();
						}	
					}
				}
			} 
			catch ( const std::exception& aExc )
			{
				pantheios::log_EXCEPTION ( aExc );
				mTcpTransportProtocol.mAsynchronousException = new uhal::exception( aExc );
			}
			catch (boost::thread_interrupted&) 
			{ 
				//This thread was interrupted by main thread, which is ok - we just exit
			}
		#endif
	}






	void TcpTransportProtocol::DispatchWorker::Dispatch( Buffers* aBuffers ){
		try 
		{

			std::vector< boost::asio::const_buffer > lAsioSendBuffer;
			lAsioSendBuffer.push_back ( boost::asio::const_buffer ( aBuffers->getSendBuffer() , aBuffers->sendCounter() ) );
		
// 			mDeadline->expires_from_now ( mTimeOut );
	
			// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
			// Send data
			// ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
			PERFORMANCE ( "ASIO write" ,
				mErrorCode = boost::asio::error::would_block;
				boost::asio::write ( *mSocket , lAsioSendBuffer , mErrorCode );
/*				boost::asio::async_write ( *mSocket , lAsioSendBuffer , boost::lambda::var(mErrorCode) = boost::lambda::_1 );
				do mIOservice->run_one(); while ( mErrorCode == boost::asio::error::would_block );*/
			)

			if (mErrorCode)
			{
				pantheios::log_ERROR ( "ASIO reported an error: " , mErrorCode.message() );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw ErrorInTcpCallback();
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
			
							
			PERFORMANCE ( "ASIO synchronous read" ,
				mErrorCode = boost::asio::error::would_block;
				boost::asio::read ( *mSocket , lAsioReplyBuffer ,  boost::asio::transfer_all(), mErrorCode );
/*				boost::asio::async_read ( *mSocket , lAsioReplyBuffer ,  boost::asio::transfer_all(), boost::lambda::var(mErrorCode) = boost::lambda::_1 );
				do mIOservice->run_one(); while ( mErrorCode == boost::asio::error::would_block );*/
			)						
	

			if (mErrorCode)
			{
				pantheios::log_ERROR ( "ASIO reported an error: " , mErrorCode.message() );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw ErrorInTcpCallback();
			}
			
			if( !mTcpTransportProtocol.mPackingProtocol->Validate( aBuffers ) )
			{
				pantheios::log_ERROR ( "Validation function reported an error!" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw IPbusValidationError ();
			}
			

		} 
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception( aExc );
		}
	
	}

// 	void TcpTransportProtocol::DispatchWorker::CheckDeadline()
// 	{
// 		try
// 		{
// 
// 			// Check whether the deadline has passed. We compare the deadline against the current time since a new asynchronous operation may have moved the deadline before this actor had a chance to run.
// 			if ( mDeadline->expires_at() <= boost::asio::deadline_timer::traits_type::now() )
// 			{
// 				// The deadline has passed
// 				mDeadline->expires_at ( boost::posix_time::pos_infin );
// 
// 				boost::asio::ip::tcp::endpoint lRemoteEndpoint = mSocket->remote_endpoint();
// 
// 
// 				pantheios::log_ERROR ( "TCP Timeout on connection to '" , lRemoteEndpoint.address().to_string() , "' port " , pantheios::integer( lRemoteEndpoint.port() ) );
// 				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
// 				mTcpTransportProtocol.mAsynchronousException = new TcpTimeout();
// 				return;
// 			}
// 
// 			// Put the actor back to sleep.
// 			mDeadline->async_wait ( boost::bind ( &TcpTransportProtocol::DispatchWorker::CheckDeadline , this ) );
// 
// 			
// 			
// 		}
// 		catch ( const std::exception& aExc )
// 		{
// 			pantheios::log_EXCEPTION ( aExc );
// 			mTcpTransportProtocol.mAsynchronousException = new uhal::exception( aExc );
// 		}
// 	}






TcpTransportProtocol::TcpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , uint32_t aTimeoutPeriod ) try :
		TransportProtocol(),
		mDispatchWorker( boost::shared_ptr< DispatchWorker >( new DispatchWorker( *this , aHostname , aServiceOrPort , aTimeoutPeriod ) ) )
		#ifdef USE_TCP_MULTITHREADED
			, mDispatchThread( boost::shared_ptr< boost::thread >( new boost::thread( *mDispatchWorker ) ) ),
			mAsynchronousException( NULL )
		#endif
		, mTimeOut( aTimeoutPeriod*1e6 )
	{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}


	TcpTransportProtocol::~TcpTransportProtocol()
	{
		try
		{
			#ifdef USE_TCP_MULTITHREADED

 				if( mDispatchThread.unique() ){
 					mDispatchThread->interrupt();
//For some reason a call to join blocks when placed in the destructor (see Google) so we just let the thread detach as the Transport Protocol should only be killed at the end of the program anyway...
// 					mDispatchThread->join();			
 				}	

				{ boost::lock_guard<boost::mutex> lock(mMutex);
				if( mAsynchronousException ){
					delete mAsynchronousException;
					mAsynchronousException = NULL;
				}
				}
			#endif
		
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
		}
	}

	
	

	void TcpTransportProtocol::Dispatch ( Buffers* aBuffers )
	{

		try
		{
			#ifdef USE_TCP_MULTITHREADED
				{ boost::lock_guard<boost::mutex> lock(mMutex);

				if( mAsynchronousException ){
					uhal::exception lExc( *mAsynchronousException );
					pantheios::log_EXCEPTION ( lExc );
					throw lExc;		
				}

				mPendingSendBuffers.push_back( aBuffers );

				}
			#else
				mDispatchWorker->Dispatch( aBuffers );
				if( !mPackingProtocol->Validate( lBuffers ) )
				{
					pantheios::log_ERROR ( "Validation function reported an error!" );
					pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
					throw uhal::exception ();
				}
			#endif

		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	
	void TcpTransportProtocol::Flush( )	
	{
		try
		{

			timeval lStart, lEnd;
			gettimeofday ( &lStart, NULL );

			#ifdef USE_TCP_MULTITHREADED
				bool lContinue( true );
				do{
				
					{ boost::lock_guard<boost::mutex> lock(mMutex);
					
					// kill time while pending buffers are emptied and check for exceptions on the thread...
					if( mAsynchronousException ){
						uhal::exception lExc( *mAsynchronousException );
						pantheios::log_EXCEPTION ( lExc );
						throw lExc;		
					}
		
					lContinue = ( mPendingSendBuffers.size() );
					}

					gettimeofday ( &lEnd, NULL );

					double lTimeTaken ( ((double)(lEnd.tv_sec-lStart.tv_sec)*1e6) + (double)(lEnd.tv_usec-lStart.tv_usec) );
					if( lTimeTaken > mTimeOut){
						pantheios::log_ERROR ( "TCP Timeout" );
						pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
						throw TcpTimeout();
					}

				}while ( lContinue );
			#endif
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}

	}
	



}

