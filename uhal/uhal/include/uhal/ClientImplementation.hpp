/**
	@file
	@author Andrew W. Rose
	@author Marc Magrans De Abril
	@date 2012
*/

#ifndef _uhal_ClientImplementation_hpp_
#define _uhal_ClientImplementation_hpp_

#include "uhal/Utilities.hpp"

#include "uhal/ClientInterface.hpp"
#include "uhal/AsioAccumulatedPacket.hpp"

#include "uhal/TransportProtocol_UDP.hpp"
#include "uhal/TransportProtocol_TCP.hpp"

#include "uhal/PackingProtocol_IPbusHwAccess.hpp"
#include "uhal/PackingProtocol_ControlHubHost.hpp"

#include "boost/lexical_cast.hpp"

namespace uhal
{


	// // ----------------------------------------------------------------------------------------------------------------

	// //! A class to directly access locally-connected devices via IPbus over UDP
	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// class IPBusUDPClient : public ClientInterface
	// {
	// //! The timeout period for UDP transactions in seconds
	// static const int mTimeoutPeriod = 10;
	// /**
	// The maximum allowed IPbus packet length.
	// @todo Now that the IPbusPacketInfo is templated can this be moved into the IPbusPacketInfo class itself?
	// */
	// static const int mMaxPacketLength = 85; //for some reason BOOST ASIO only sends a maximum of 340 bytes ( at least it does on my CENTOS VM )

	// //! Typedef the packing protocol which will be used by this IPbus Client
	// typedef IPbusHwAccessPackingProtocol< IPbusProtocolVersion > tPackingProtocol;
	// //! Typedef the transport protocol which will be used by this IPbus Client
	// typedef UdpTransportProtocol tTransportProtocol;

	// public:

	// /**
	// Constructor
	// @param aId the uinique identifier that the client will be given.
	// @param aUri a struct containing the full URI of the target.
	// */
	// IPBusUDPClient ( const std::string& aId , const URI& aUri );

	// /**
	// Return a description of the behaviour this client
	// @return a description of the behaviour this client
	// */
	// static std::string description();

	// private:
	// /**
	// Method to return the packing protocol. The base ClientInterface requires that this exists
	// @return a reference to an instance of the packing protocol
	// */
	// PackingProtocol& getPackingProtocol();

	// /**
	// Method to return the transport protocol. The base ClientInterface requires that this exists
	// @return a reference to an instance of the transport protocol
	// */
	// TransportProtocol& getTransportProtocol();

	// //! An instance of the packing protocol for this IPbus client
	// tPackingProtocol mPackingProtocol;
	// //! An instance of the transport protocol for this IPbus client
	// tTransportProtocol mTransportProtocol;

	// };

	// // ----------------------------------------------------------------------------------------------------------------


	// //! A class to directly access locally-connected devices via IPbus over TCP
	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// class IPBusTCPClient : public ClientInterface
	// {
	// //! The timeout period for TCP transactions in seconds
	// static const int mTimeoutPeriod = 10;
	// /**
	// The maximum allowed IPbus packet length.
	// @todo Now that the IPbusPacketInfo is templated can this be moved into the IPbusPacketInfo class itself?
	// */
	// static const int mMaxPacketLength = 100;

	// //! Typedef the packing protocol which will be used by this IPbus Client
	// typedef IPbusHwAccessPackingProtocol< IPbusProtocolVersion > tPackingProtocol;
	// //! Typedef the transport protocol which will be used by this IPbus Client
	// typedef TcpTransportProtocol tTransportProtocol;

	// public:

	// /**
	// Constructor
	// @param aId the uinique identifier that the client will be given.
	// @param aUri a struct containing the full URI of the target.
	// */
	// IPBusTCPClient ( const std::string& aId , const URI& aUri );

	// /**
	// Return a description of the behaviour this client
	// @return a description of the behaviour this client
	// */
	// static std::string description();

	// private:
	// /**
	// Method to return the packing protocol. The base ClientInterface requires that this exists
	// @return a reference to an instance of the packing protocol
	// */
	// PackingProtocol& getPackingProtocol();

	// /**
	// Method to return the transport protocol. The base ClientInterface requires that this exists
	// @return a reference to an instance of the transport protocol
	// */
	// TransportProtocol& getTransportProtocol();

	// //! An instance of the packing protocol for this IPbus client
	// tPackingProtocol mPackingProtocol;
	// //! An instance of the transport protocol for this IPbus client
	// tTransportProtocol mTransportProtocol;
	// };


	// ----------------------------------------------------------------------------------------------------------------

	//! Exception class to handle the case where the received header does not match the expected header. Uses the base uhal::exception implementation of what()
	class XMLfileMissingRequiredParameters: public uhal::exception {};

	std::pair< uint32_t , uint16_t > ExtractTargetID ( const URI& aUri );

	//! A class to indirectly access (via a Control Hub Host) devices via IPbus over UDP
	template< eIPbusProtocolVersion IPbusProtocolVersion >
	class ControlHubClient : public ClientInterface
	{

			//! The timeout period for TCP transactions in seconds
			static const int mTimeoutPeriod = 10;
			/**
				The maximum allowed IPbus packet length.
				@todo Now that the IPbusPacketInfo is templated can this be moved into the IPbusPacketInfo class itself?
			*/
			static const int mMaxPacketLength = 350;

			//! Typedef the packing protocol which will be used by this IPbus Client
			typedef ControlHubHostPackingProtocol< IPbusProtocolVersion > tPackingProtocol;
			//! Typedef the transport protocol which will be used by this IPbus Client
			typedef TcpTransportProtocol tTransportProtocol;

		public:
			/**
				Constructor
				@param aId the uinique identifier that the client will be given.
				@param aUri a struct containing the full URI of the target.
			*/
			ControlHubClient ( const std::string& aId , const URI& aUri );

			/**
				Return a description of the behaviour this client
				@return a description of the behaviour this client
			*/
			static std::string description();

		private:

			/**
				Method to return the packing protocol. The base ClientInterface requires that this exists
				@return a reference to an instance of the packing protocol
			*/
			PackingProtocol& getPackingProtocol();

			/**
				Method to return the transport protocol. The base ClientInterface requires that this exists
				@return a reference to an instance of the transport protocol
			*/
			TransportProtocol& getTransportProtocol();

			std::pair< uint32_t , uint16_t > mTargetId;

			//! A pointer to an instance of the packing protocol used by this IPbus client
			tPackingProtocol mPackingProtocol;
			//! A pointer to an instance of the transport protocol used by this IPbus client
			tTransportProtocol mTransportProtocol;

	};








}

#include "uhal/TemplateDefinitions/ClientImplementation.hxx"



#endif
