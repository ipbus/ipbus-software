#ifndef _uhal_ClientImplementation_hpp_
#define _uhal_ClientImplementation_hpp_

#include "uhal/Utilities.hpp"

#include "uhal/ClientInterface.hpp"
#include "uhal/AsioAccumulatedPacket.hpp"

#include "uhal/TransportProtocol_UDP.hpp"
#include "uhal/TransportProtocol_TCP.hpp"

#include "uhal/PackingProtocol_IPbusHwAccess.hpp"
#include "uhal/PackingProtocol_ControlHubHost.hpp"

namespace uhal
{


// ----------------------------------------------------------------------------------------------------------------

	class IPBusUDPClient : public ClientInterface
	{
		static const int mTimeoutPeriod = 10;
		static const int mMaxPacketLength = 128;

		public:
			IPBusUDPClient ( const std::string& aId , const URI& aUri )
				: ClientInterface ( aId , aUri ),
				mPackingProtocol( mMaxPacketLength ),
				mTransportProtocol( mUri.mHostname , //hostname
									mUri.mPort , //port number
									mPackingProtocol, //reference to a PackingProtocol object which implements a function to handle the BOOST::ASIO callback
									mTimeoutPeriod //the timeout period for the TCP transactions in seconds
									)
			{}
					
		private:
			PackingProtocol& getPackingProtocol(){ return mPackingProtocol; }
			TransportProtocol& getTransportProtocol(){ return mTransportProtocol; }
		
			IPbusHwAccessPackingProtocol mPackingProtocol;	
			UdpTransportProtocol< IPbusHwAccessPackingProtocol > mTransportProtocol;

	};
	
// ----------------------------------------------------------------------------------------------------------------

	
	class IPBusTCPClient : public ClientInterface
	{
		static const int mTimeoutPeriod = 10;
		static const int mMaxPacketLength = 128;
	
		public:
			IPBusTCPClient ( const std::string& aId , const URI& aUri )
				: ClientInterface ( aId , aUri ),
				mPackingProtocol( mMaxPacketLength ),
				mTransportProtocol( mUri.mHostname , //hostname
									mUri.mPort , //port number
									mPackingProtocol, //reference to a PackingProtocol object which implements a function to handle the BOOST::ASIO callback
									mTimeoutPeriod //the timeout period for the TCP transactions in seconds
									)
			{}
			
		private:
			PackingProtocol& getPackingProtocol(){ return mPackingProtocol; }
			TransportProtocol& getTransportProtocol(){ return mTransportProtocol; }
		
			IPbusHwAccessPackingProtocol mPackingProtocol;	
			TcpTransportProtocol< IPbusHwAccessPackingProtocol > mTransportProtocol;

	};


// ----------------------------------------------------------------------------------------------------------------
	
	class ControlHubClient : public ClientInterface
	{
		static const int mTimeoutPeriod = 10;
		static const int mMaxPacketLength = 128;
		
		typedef ControlHubHostPackingProtocol tPackingProtocol;
		typedef TcpTransportProtocol< tPackingProtocol > tTransportProtocol;
		typedef std::hash_map< std::string , std::pair< tPackingProtocol* , tTransportProtocol* > > tMap;
		
		public:
			ControlHubClient ( const std::string& aId , const URI& aUri );

			virtual ~ControlHubClient ();
			
			void pack( IPbusPacketInfo& aIPbusPacketInfo )
			{
				mPackingProtocolPtr->pack( aIPbusPacketInfo , mTargetId );
			}
	
		private:
			PackingProtocol& getPackingProtocol(){ return *mPackingProtocolPtr; }	
			TransportProtocol& getTransportProtocol(){ return *mTransportProtocolPtr; } 
			
			uint32_t mTargetId;
			tPackingProtocol *mPackingProtocolPtr;				
			tTransportProtocol *mTransportProtocolPtr;
			
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
			PackingProtocol& getPackingProtocol(){ pantheios::log_ALERT("USING IPbusHwAccess FOR TESTING, SHOULD BE DUMMY"); return mPackingProtocol; }
			TransportProtocol& getTransportProtocol(){ return *mTransportProtocol; } 
		
			TcpTransportProtocol* mTransportProtocol; // needs changing to correct protocol
			IPbusHwAccessPackingProtocol mPackingProtocol; // needs changing to correct protocol				
		
	};

// ----------------------------------------------------------------------------------------------------------------
*/

}

#endif
