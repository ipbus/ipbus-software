// /**
// @file
// @author Andrew W. Rose
// @date 2012
// */

// #ifndef _uhal_TransportProtocol_UDP_hpp_
// #define _uhal_TransportProtocol_UDP_hpp_

// #include "uhal/exception.hpp"
// #include "uhal/ProtocolInterfaces.hpp"
// 
// #include "uhal/log.hpp"

// #include <iostream>
// #include <iomanip>

// #include <boost/bind.hpp>
// #include <boost/asio.hpp>
// #include <boost/date_time/posix_time/posix_time_types.hpp>

// #include <string>

// namespace uhal
// {

// //! Exception class to handle the case where the UDP connection timed out. Uses the base uhal::exception implementation of what()
// class UdpTimeout: public uhal::exception {  };
// //! Exception class to handle the case where the error flag was raised in the asynchronous callback system. Uses the base uhal::exception implementation of what()
// class ErrorInUdpCallback: public uhal::exception {  };
// //! Exception class to handle the case where the returned packet size does not match that expected. Uses the base uhal::exception implementation of what()
// class ReturnSizeMismatch: public uhal::exception {  };


// template < class PACKINGPROTOCOL >
// class UdpTransportProtocol : public TransportProtocol
// {
// public:

// /**
// Constructor
// */
// UdpTransportProtocol ( const std::string& aHostname , const std::string& aServiceOrPort , PACKINGPROTOCOL& aPackingProtocol , uint32_t aTimeoutPeriod = 10 );
// /**
// Destructor
// */
// virtual ~UdpTransportProtocol();

// /**
// Flush the queue of pending IPbus transactions
// */
// void Dispatch();

// private:

// void CheckDeadline();


// std::string mHostname;
// std::string mServiceOrPort;

// PACKINGPROTOCOL& mPackingProtocol;

// //! The boost::asio::io_service used to create the connections
// boost::asio::io_service mIOservice;

// boost::asio::ip::udp::socket* mSocket;
// boost::asio::ip::udp::resolver* mResolver;
// boost::asio::ip::udp::resolver::query* mQuery;
// boost::asio::ip::udp::resolver::iterator mIterator;

// //! Timeout period for UDP transactions;
// boost::posix_time::seconds mTimeOut;

// //! timer for the timeout conditions
// boost::asio::deadline_timer mDeadline;

// bool mTimeoutFlag;

// };


// }

// #include "uhal/TemplateDefinitions/TransportProtocol_UDP.hxx"

// #endif
