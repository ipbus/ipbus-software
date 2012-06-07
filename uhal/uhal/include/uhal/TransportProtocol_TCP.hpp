/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_TransportProtocol_TCP_hpp_
#define _uhal_TransportProtocol_TCP_hpp_

#include "uhal/exception.hpp"
#include "uhal/ProtocolInterfaces.hpp"
#include "uhal/log.hpp"

#include <iostream>
#include <iomanip>

#include <boost/bind.hpp>
#include <boost/asio.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>

#ifdef USE_TCP_MULTITHREADED
	#include <boost/thread/thread.hpp>
#endif
	
#include <string>

namespace uhal
{

	//! Exception class to handle the case where the TCP connection timed out. Uses the base uhal::exception implementation of what()
	class TcpTimeout: public uhal::exception {  };
	//! Exception class to handle the case where the error flag was raised in the asynchronous callback system. Uses the base uhal::exception implementation of what()
	class ErrorInTcpCallback: public uhal::exception {  };

	class ErrorAtTcpSocketCreation: public uhal::exception {  };
		
	class TcpTransportProtocol : public TransportProtocol
	{
	
		public:
			class DispatchWorker{
				public:
					DispatchWorker ( TcpTransportProtocol& aTcpTransportProtocol , const std::string& aHostname , const std::string& aServiceOrPort , uint32_t aTimeoutPeriod );

					virtual ~DispatchWorker();
			
					void operator() ();
					
					void Dispatch( Buffers* aBuffers );
	
				
				private:
// 					void CheckDeadline();
	
					TcpTransportProtocol& mTcpTransportProtocol;
					
					//! The boost::asio::io_service used to create the connections
					boost::shared_ptr< boost::asio::io_service > mIOservice;		
	
					boost::shared_ptr< boost::asio::ip::tcp::socket > mSocket;
	
					//! Timeout period for TCP transactions;
// 					boost::posix_time::seconds mTimeOut;
	
					//! Timer for the timeout conditions
// 					boost::shared_ptr< boost::asio::deadline_timer > mDeadline;

					//! Error code for the async callbacks to fill
					boost::system::error_code mErrorCode;
						
			};
			
			friend class DispatchWorker;
	

			/**
				Constructor
			*/
			TcpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , uint32_t aTimeoutPeriod = 10 );
			/**
				Destructor
			*/
			virtual ~TcpTransportProtocol();

			/**
			Add buffers to the dispatch queue
			*/
			virtual void Dispatch ( Buffers* aBuffers );

			virtual void Flush( );

		private:
					
			boost::shared_ptr< DispatchWorker > mDispatchWorker;

#ifdef USE_TCP_MULTITHREADED
			boost::shared_ptr< boost::thread > mDispatchThread;
			std::deque< Buffers* > mPendingSendBuffers;
			uhal::exception* mAsynchronousException;
			boost::mutex mMutex; 
#endif

			//! Timeout period for TCP transactions;
			double mTimeOut;
	};


}

// #include "uhal/TemplateDefinitions/TransportProtocol_TCP.hxx"

#endif
