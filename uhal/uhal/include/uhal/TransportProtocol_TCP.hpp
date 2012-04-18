#ifndef _uhal_TransportProtocol_TCP_hpp_
#define _uhal_TransportProtocol_TCP_hpp_

#include "uhal/exception.hpp"
#include "uhal/ProtocolInterfaces.hpp"
#include "uhal/AsioAccumulatedPacket.hpp"
#include "uhal/log.hpp"

#include <iostream>
#include <iomanip>

#include <boost/bind.hpp>
#include <boost/asio.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>

#include <string>

namespace uhal
{

	class TcpTimeout: public uhal::exception {  };
	class ErrorInTcpCallback: public uhal::exception {  };

	template < class PACKINGPROTOCOL >
	class TcpTransportProtocol : public TransportProtocol
	{
		public:

			TcpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , PACKINGPROTOCOL& aPackingProtocol , uint32_t aTimeoutPeriod = 10 );
			virtual ~TcpTransportProtocol();
			void Dispatch();

		private:

			void CheckDeadline();


			std::string mHostname;
			std::string mServiceOrPort;

			PACKINGPROTOCOL& mPackingProtocol;

			//! The boost::asio::io_service used to create the connections
			boost::asio::io_service mIOservice;

			boost::asio::ip::tcp::socket* mSocket;
			boost::asio::ip::tcp::resolver* mResolver;
			boost::asio::ip::tcp::resolver::query* mQuery;
			boost::asio::ip::tcp::resolver::iterator mIterator;

			//! Timeout period for TCP transactions;
			boost::posix_time::seconds mTimeOut;

			/// timer for the timeout conditions
			boost::asio::deadline_timer mDeadline;

			bool mTimeoutFlag;

	};


}

#include "TemplateDefinitions/TransportProtocol_TCP.hxx"

#endif
