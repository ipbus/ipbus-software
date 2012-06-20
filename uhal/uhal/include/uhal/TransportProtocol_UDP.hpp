/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_TransportProtocol_UDP_hpp_
#define _uhal_TransportProtocol_UDP_hpp_

#include "uhal/exception.hpp"
#include "uhal/ProtocolInterfaces.hpp"
#include "uhal/log/log.hpp"

#include <iostream>
#include <iomanip>

#include <boost/bind.hpp>
#include <boost/asio.hpp>

#ifdef USE_UDP_MULTITHREADED
#include <boost/thread/thread.hpp>
#endif

#include <string>

namespace uhal
{

	//! Exception class to handle the case where the UDP connection timed out. Uses the base uhal::exception implementation of what()
	class UdpTimeout: public uhal::exception {  };
	//! Exception class to handle the case where the error flag was raised in the asynchronous callback system. Uses the base uhal::exception implementation of what()
	class ErrorInUdpCallback: public uhal::exception {  };

	class ErrorAtUdpSocketCreation: public uhal::exception {  };

	class UdpTransportProtocol : public TransportProtocol
	{

		public:
			class DispatchWorker
			{
				public:
					DispatchWorker ( UdpTransportProtocol& aUdpTransportProtocol , const std::string& aHostname , const std::string& aServiceOrPort , uint32_t aTimeoutPeriod );

					virtual ~DispatchWorker();

					void operator() ();

					void Dispatch ( Buffers* aBuffers );


				private:

					UdpTransportProtocol& mUdpTransportProtocol;

					//! The boost::asio::io_service used to create the connections
					boost::shared_ptr< boost::asio::io_service > mIOservice;

					boost::shared_ptr< boost::asio::ip::udp::socket > mSocket;

					boost::shared_ptr< boost::asio::ip::udp::endpoint > mEndpoint;

					//! Error code for the async callbacks to fill
					boost::system::error_code mErrorCode;

			};

			friend class DispatchWorker;


			/**
				Constructor
			*/
			UdpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , uint32_t aTimeoutPeriod = 10 );
			/**
				Destructor
			*/
			virtual ~UdpTransportProtocol();

			/**
			Add buffers to the dispatch queue
			*/
			virtual void Dispatch ( Buffers* aBuffers );

			virtual void Flush( );

		private:

			boost::shared_ptr< DispatchWorker > mDispatchWorker;

#ifdef USE_UDP_MULTITHREADED
			boost::shared_ptr< boost::thread > mDispatchThread;
			std::deque< Buffers* > mPendingSendBuffers;
			uhal::exception* mAsynchronousException;
			boost::mutex mMutex;
#endif

	};


}

#endif
