#ifndef _uhal_ClientImplementation_hpp_
#define _uhal_ClientImplementation_hpp_

#include "uhal/ClientInterface.hpp"

#include "uhal/AsioAccumulatedPacket.hpp"

#include "uhal/TransportProtocol_UDP.hpp"
#include "uhal/TransportProtocol_TCP.hpp"
#include "uhal/PackingProtocol_IPbusHwAccess.hpp"

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
									boost::lexical_cast<std::string>(mUri.mPort) , //port number
									mPackingProtocol, //reference to a PackingProtocol object which implements a function to handle the BOOST::ASIO callback
									mTimeoutPeriod //the timeout period for the TCP transactions in seconds
									)
			{
			}
					
		private:
			PackingProtocol& getPackingProtocol(){
				return mPackingProtocol;
			}
			
			TransportProtocol& getTransportProtocol(){
				return mTransportProtocol;
			}
		
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
									boost::lexical_cast<std::string>(mUri.mPort) , //port number
									mPackingProtocol, //reference to a PackingProtocol object which implements a function to handle the BOOST::ASIO callback
									mTimeoutPeriod //the timeout period for the TCP transactions in seconds
									)
			{
			}
			
		private:
			PackingProtocol& getPackingProtocol(){
				return mPackingProtocol;
			}
			
			TransportProtocol& getTransportProtocol(){
				return mTransportProtocol;
			}
		
			IPbusHwAccessPackingProtocol mPackingProtocol;	
			TcpTransportProtocol< IPbusHwAccessPackingProtocol > mTransportProtocol;

	};

/*
// ----------------------------------------------------------------------------------------------------------------
	
	class ControlHubClient : public ClientInterface
	{
		public:
			ControlHubClient ( const std::string& aId , const URI& aUri )
				: ClientInterface ( aId , aUri ),
				mTransportProtocol( NULL ),
				mPackingProtocol()
			{};

		private:
			PackingProtocol& getPackingProtocol(){ pantheios::log_ALERT("USING IPbusHwAccess FOR TESTING, SHOULD BE ControlHub"); return mPackingProtocol; }	
			TransportProtocol& getTransportProtocol(){ return *mTransportProtocol; } 
		
			TcpTransportProtocol* mTransportProtocol;
			IPbusHwAccessPackingProtocol mPackingProtocol; // needs changing to correct protocol					
	};

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
