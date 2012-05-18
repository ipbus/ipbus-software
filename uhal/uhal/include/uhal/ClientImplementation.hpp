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


	// ----------------------------------------------------------------------------------------------------------------

	//! A class to directly access locally-connected devices via IPbus over UDP
	template< eIPbusProtocolVersion IPbusProtocolVersion >
	class IPBusUDPClient : public ClientInterface
	{
			//! The timeout period for UDP transactions in seconds
			static const int mTimeoutPeriod = 10;
			/**
				The maximum allowed IPbus packet length.
				@todo Now that the IPbusPacketInfo is templated can this be moved into the IPbusPacketInfo class itself?
			*/
			static const int mMaxPacketLength = 85; //for some reason BOOST ASIO only sends a maximum of 340 bytes ( at least it does on my CENTOS VM )

			//! Typedef the packing protocol which will be used by this IPbus Client
			typedef IPbusHwAccessPackingProtocol< IPbusProtocolVersion > tPackingProtocol;
			//! Typedef the transport protocol which will be used by this IPbus Client
			typedef UdpTransportProtocol< tPackingProtocol > tTransportProtocol;

		public:

			/**
				Constructor
				@param aId the uinique identifier that the client will be given.
				@param aUri a struct containing the full URI of the target.
			*/
			IPBusUDPClient ( const std::string& aId , const URI& aUri );

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

			//! An instance of the packing protocol for this IPbus client
			tPackingProtocol mPackingProtocol;
			//! An instance of the transport protocol for this IPbus client
			tTransportProtocol mTransportProtocol;

	};

	// ----------------------------------------------------------------------------------------------------------------


	//! A class to directly access locally-connected devices via IPbus over TCP
	template< eIPbusProtocolVersion IPbusProtocolVersion >
	class IPBusTCPClient : public ClientInterface
	{
			//! The timeout period for TCP transactions in seconds
			static const int mTimeoutPeriod = 10;
			/**
				The maximum allowed IPbus packet length.
				@todo Now that the IPbusPacketInfo is templated can this be moved into the IPbusPacketInfo class itself?
			*/
			static const int mMaxPacketLength = 100;

			//! Typedef the packing protocol which will be used by this IPbus Client
			typedef IPbusHwAccessPackingProtocol< IPbusProtocolVersion > tPackingProtocol;
			//! Typedef the transport protocol which will be used by this IPbus Client
			typedef TcpTransportProtocol< tPackingProtocol > tTransportProtocol;

		public:

			/**
				Constructor
				@param aId the uinique identifier that the client will be given.
				@param aUri a struct containing the full URI of the target.
			*/
			IPBusTCPClient ( const std::string& aId , const URI& aUri );

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

			//! An instance of the packing protocol for this IPbus client
			tPackingProtocol mPackingProtocol;
			//! An instance of the transport protocol for this IPbus client
			tTransportProtocol mTransportProtocol;

	};


	// ----------------------------------------------------------------------------------------------------------------

	//! Exception class to handle the case where the received header does not match the expected header. Uses the base uhal::exception implementation of what()
	class XMLfileMissingRequiredParameters: public uhal::exception {};

	//Forware declare so that we can declare the friends
	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	class ControlHubClient;
	
	//Declare two different functions rather than messing around with partial template specialization...
	template < eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ExtractTargetID1( ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >& aCHC );
	template < eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ExtractTargetID2( ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >& aCHC );
		
	
	//! A class to indirectly access (via a Control Hub Host) devices via IPbus over UDP
	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	class ControlHubClient : public ClientInterface
	{

			friend void ExtractTargetID1<>( ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >& aCHC );
			friend void ExtractTargetID2<>( ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >& aCHC );
	
			//! The timeout period for TCP transactions in seconds
			static const int mTimeoutPeriod = 10;
			/**
				The maximum allowed IPbus packet length.
				@todo Now that the IPbusPacketInfo is templated can this be moved into the IPbusPacketInfo class itself?
			*/
			static const int mMaxPacketLength = 100;

			//! Typedef the packing protocol which will be used by this IPbus Client
			typedef ControlHubHostPackingProtocol< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion > tPackingProtocol;
			//! Typedef the transport protocol which will be used by this IPbus Client
			typedef TcpTransportProtocol< tPackingProtocol > tTransportProtocol;
			//! Typedef a map of string identifiers to pairs a PackingProtocol/TransportProtocol pair
			typedef std::hash_map< std::string , std::pair< tPackingProtocol* , tTransportProtocol* > > tMap;

		public:
			/**
				Constructor
				@param aId the uinique identifier that the client will be given.
				@param aUri a struct containing the full URI of the target.
			*/
			ControlHubClient ( const std::string& aId , const URI& aUri );

			/**
				Overloaded version of the pack function which adds the target ID, as reguired by this packing protocol
				@param aIPbusPacketInfo an IPbusPacketInfo to be packed into the stream
			*/
			void pack ( IPbusPacketInfo& aIPbusPacketInfo );

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

			//! The target ID with which this instance of the client is associated
			uint64_t mTargetId;

			//! A pointer to an instance of the packing protocol used by this IPbus client
			tPackingProtocol* mPackingProtocolPtr;
			//! A pointer to an instance of the transport protocol used by this IPbus client
			tTransportProtocol* mTransportProtocolPtr;

			//! Map association the address of the Control Hub Host with an instance of a PackingProtocol/TransportProtocol pair
			static tMap mMapNameAndPortToCHH;
	};

	

	

	
	/*
	// ----------------------------------------------------------------------------------------------------------------

		class DummyClient : public ClientInterface
		{
			public:
				DummyClient ( const std::string& aId , const URI& aUri )
					: ClientInterface ( aId , aUri ),
					mTransportProtocol( NULL ),
					mPackingProtocol()
				{};

			private:
				PackingProtocol& getPackingProtocol(){ pantheios::log_ERROR("USING IPbusHwAccess FOR TESTING, SHOULD BE DUMMY"); return mPackingProtocol; }
				TransportProtocol& getTransportProtocol(){ return *mTransportProtocol; }

				TcpTransportProtocol* mTransportProtocol; // needs changing to correct protocol
				IPbusHwAccessPackingProtocol mPackingProtocol; // needs changing to correct protocol

		};

	// ----------------------------------------------------------------------------------------------------------------
	*/

}

#include "uhal/TemplateDefinitions/ClientImplementation.hxx"



#endif
