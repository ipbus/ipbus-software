#include "uhal/performance.hpp"
#include "uhal/TransportProtocol_TCP.hpp"

#define USE_TCP_MULTITHREADED

#ifdef USE_TCP_MULTITHREADED
	#define MUTEX_LOCK() mutex.lock()
	#define MUTEX_UNLOCK() mutex.unlock()
#else
	#define MUTEX_LOCK()
	#define MUTEX_UNLOCK()
#endif

namespace uhal
{
	
	boost::mutex mutex; 


	TcpTransportProtocol::DispatchWorker::DispatchWorker( TcpTransportProtocol& aTcpTransportProtocol , const std::string& aHostname , const std::string& aServiceOrPort , uint32_t aTimeoutPeriod ) try :
							  mTcpTransportProtocol( aTcpTransportProtocol ),
							  mIOservice( boost::shared_ptr< boost::asio::io_service >( new boost::asio::io_service() ) ),
							  mSocket ( boost::shared_ptr< boost::asio::ip::tcp::socket >( new boost::asio::ip::tcp::socket( *mIOservice ) ) ),
							  mTimeOut ( aTimeoutPeriod )
							  // mDeadline ( mIOservice ),
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

			pantheios::log_NOTICE ( "TCP connection succeeded" );

			// mDeadline.expires_at ( boost::posix_time::pos_infin );
			// CheckDeadline(); // Start the persistent actor that checks for deadline expiry.

			
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}

		
	TcpTransportProtocol::DispatchWorker::~DispatchWorker()	{
			try
			{
				if ( mSocket.unique() ){
					mSocket->close();
				}			
			}
			catch ( const std::exception& aExc )
			{
				pantheios::log_EXCEPTION ( aExc );
				//throw uhal::exception( aExc ); MUST NOT THROW IN A DESTRUCTOR
			}
	}


	void TcpTransportProtocol::DispatchWorker::operator() (){
		#ifdef USE_TCP_MULTITHREADED
		try 
		{
			while( true ){
				
				boost::this_thread::interruption_point();
				
				MUTEX_LOCK();
				bool lBuffersPending( mTcpTransportProtocol.mPendingBuffers.size() != 0 );
				MUTEX_UNLOCK();

				if( lBuffersPending ){
					MUTEX_LOCK();
					Buffers* lBuffers = mTcpTransportProtocol.mPendingBuffers.front();
					MUTEX_UNLOCK();
				
					Dispatch( lBuffers );
					
					MUTEX_LOCK();
					mTcpTransportProtocol.mPendingBuffers.pop_front();
					MUTEX_UNLOCK();	
				}
			}
		} 
		catch (boost::thread_interrupted&) 
		{ 
		} 
		#endif
	}






	void TcpTransportProtocol::DispatchWorker::Dispatch( Buffers* aBuffers ){
	
		std::vector< boost::asio::const_buffer > lAsioSendBuffer;
		lAsioSendBuffer.push_back ( boost::asio::const_buffer ( aBuffers->getSendBuffer() , aBuffers->sendCounter() ) );

		//pantheios::log_NOTICE ( "ASIO write" );
		PERFORMANCE ( "ASIO write" ,
					  boost::asio::write ( *mSocket , lAsioSendBuffer );
					)
					
		std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
		std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( aBuffers->getReplyBuffer() );
		lAsioReplyBuffer.reserve ( lReplyBuffers.size() );

		for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
		{
			lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( lIt->first , lIt->second ) );
		}
		
		// mDeadline.expires_from_now ( mTimeOut );
	
		//pantheios::log_NOTICE ( "ASIO read" );
		boost::system::error_code ec;

		PERFORMANCE ( "ASIO synchronous read" ,
					  boost::asio::read ( *mSocket , lAsioReplyBuffer ,  boost::asio::transfer_all(), ec);
					)						

		if (ec)
		{
			pantheios::log_ERROR ( "ASIO reported an error: " , ec.message() );
		}
		else
		{
			//pantheios::log_NOTICE ( "Call back succeeded" );
		}
	
	
	}











TcpTransportProtocol::TcpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , uint32_t aTimeoutPeriod ) try :
		TransportProtocol(),
		mDispatchWorker( boost::shared_ptr< DispatchWorker >( new DispatchWorker( *this , aHostname , aServiceOrPort , aTimeoutPeriod ) ) ),
		#ifdef USE_TCP_MULTITHREADED
		mDispatchThread( boost::shared_ptr< boost::thread >( new boost::thread( *mDispatchWorker ) ) ),
		#endif
		mAsynchronousException( NULL ),
		mSent( 0 ),
		mReceived( 0 )				  
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
		MUTEX_LOCK();
			if( mAsynchronousException ){
				uhal::exception lExc( *mAsynchronousException );
				pantheios::log_EXCEPTION ( lExc );
				delete mAsynchronousException;
				mAsynchronousException = NULL;
			}
		MUTEX_UNLOCK();
		
		#ifdef USE_TCP_MULTITHREADED
			if( mDispatchThread.unique() ){
				mDispatchThread->interrupt(); 
				mDispatchThread->join();
			}	
		#endif
		
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			//throw uhal::exception( aExc ); MUST NOT THROW IN A DESTRUCTOR
		}
	}

	
	

	void TcpTransportProtocol::Dispatch ( Buffers* aBuffers )
	{

		MUTEX_LOCK();
		if( mAsynchronousException ){
			uhal::exception lExc( *mAsynchronousException );
			pantheios::log_EXCEPTION ( lExc );
			throw lExc;		
		}
		MUTEX_UNLOCK();
	
		try
		{
			MUTEX_LOCK();
			mPendingBuffers.push_back( aBuffers );
			MUTEX_UNLOCK();
			
			#ifndef USE_TCP_MULTITHREADED
			mDispatchWorker->Dispatch( aBuffers );	
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
		
		#ifdef USE_TCP_MULTITHREADED
		bool lContinue( true );
		while ( lContinue ){
		
			MUTEX_LOCK();
			lContinue = ( mPendingBuffers.size()!=0 );
			
			// kill time while pending buffers are emptied and check for exceptions on the thread...
			if( mAsynchronousException ){
				uhal::exception lExc( *mAsynchronousException );
				pantheios::log_EXCEPTION ( lExc );
				throw lExc;		
			}

			MUTEX_UNLOCK();
		}
		#endif
		
	}
	

	// template < class PACKINGPROTOCOL >
	// void TcpTransportProtocol::CheckDeadline()
	// {
		// try
		// {
// #ifndef USE_LINUX_SOCKETS			
// //--------------------------------------------------------------------------------------------------------------------------------------------------------------
// // Use Boost ASIO
// //--------------------------------------------------------------------------------------------------------------------------------------------------------------
			// // Check whether the deadline has passed. We compare the deadline against the current time since a new asynchronous operation may have moved the deadline before this actor had a chance to run.
			// if ( mDeadline.expires_at() <= boost::asio::deadline_timer::traits_type::now() )
			// {
				// // The deadline has passed
				// mDeadline.expires_at ( boost::posix_time::pos_infin );
				// pantheios::log_ERROR ( "TCP Timeout on connection to '" , mHostname , "' port " , mServiceOrPort );
				// throw TcpTimeout();
			// }

			// // Put the actor back to sleep.
			// mDeadline.async_wait ( boost::bind ( &TcpTransportProtocol::CheckDeadline , this ) );
// //--------------------------------------------------------------------------------------------------------------------------------------------------------------				
// #else
// //--------------------------------------------------------------------------------------------------------------------------------------------------------------
// // Use Linux sockets
// //--------------------------------------------------------------------------------------------------------------------------------------------------------------
			// // TIME OUT NOT YET IMPLEMENTED FOR LINUX SOCKETS			
// //--------------------------------------------------------------------------------------------------------------------------------------------------------------				
// #endif
			
		// }
		// catch ( const std::exception& aExc )
		// {
			// pantheios::log_EXCEPTION ( aExc );
			// pantheios::log_ERROR ( "Cannot propagate an exception out of the thread, so setting the timeout flag instead." );
			// pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
			// mTimeoutFlag = true;
		// }
	// }




}

