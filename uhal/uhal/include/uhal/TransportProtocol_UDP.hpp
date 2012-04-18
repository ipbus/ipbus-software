#ifndef _uhal_TransportProtocol_UDP_hpp_
#define _uhal_TransportProtocol_UDP_hpp_

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

	class UdpTimeout: public uhal::exception {  };
	class ErrorInUdpCallback: public uhal::exception {  };
	class ReturnSizeMismatch: public uhal::exception {  };


	template < class PACKINGPROTOCOL >
	class UdpTransportProtocol : public TransportProtocol
	{
		public:

			UdpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , PACKINGPROTOCOL& aPackingProtocol , uint32_t aTimeoutPeriod = 10 );
			virtual ~UdpTransportProtocol();
			void Dispatch();

		private:

			void CheckDeadline();


			std::string mHostname;
			std::string mServiceOrPort;

			PACKINGPROTOCOL& mPackingProtocol;

			//! The boost::asio::io_service used to create the connections
			boost::asio::io_service mIOservice;

			boost::asio::ip::udp::socket* mSocket;
			boost::asio::ip::udp::resolver* mResolver;
			boost::asio::ip::udp::resolver::query* mQuery;
			boost::asio::ip::udp::resolver::iterator mIterator;

			//! Timeout period for UDP transactions;
			boost::posix_time::seconds mTimeOut;

			/// timer for the timeout conditions
			boost::asio::deadline_timer mDeadline;

			bool mTimeoutFlag;

	};


}

#include "TemplateDefinitions/TransportProtocol_UDP.hxx"

#endif
