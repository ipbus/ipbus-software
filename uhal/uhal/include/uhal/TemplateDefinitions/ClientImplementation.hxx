
#define ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary 8

namespace uhal
{
	// ----------------------------------------------------------------------------------------------------------------

	//! A class to directly access locally-connected devices via IPbus over UDP
	template< eIPbusProtocolVersion IPbusProtocolVersion >

IPBusUDPClient< IPbusProtocolVersion >::IPBusUDPClient ( const std::string& aId , const URI& aUri ) try :
		ClientInterface ( aId , aUri ),
						mPackingProtocol ( mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
						mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
	{
		Link ( mTransportProtocol , mPackingProtocol );
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw uhal::exception ( aExc );
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	std::string IPBusUDPClient< IPbusProtocolVersion >::description()
	{
		return "Direct access to hardware via UDP, using " + toString ( IPbusProtocolVersion );
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	PackingProtocol& IPBusUDPClient< IPbusProtocolVersion >::getPackingProtocol()
	{
		try
		{
			return mPackingProtocol;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	TransportProtocol& IPBusUDPClient< IPbusProtocolVersion >::getTransportProtocol()
	{
		try
		{
			return mTransportProtocol;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}







	//! A class to directly access locally-connected devices via IPbus over TCP
	template< eIPbusProtocolVersion IPbusProtocolVersion >

IPBusTCPClient< IPbusProtocolVersion >::IPBusTCPClient ( const std::string& aId , const URI& aUri ) try :
		ClientInterface ( aId , aUri ),
						mPackingProtocol ( mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
						mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
	{
		Link ( mTransportProtocol , mPackingProtocol );
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw uhal::exception ( aExc );
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	std::string IPBusTCPClient< IPbusProtocolVersion >::description()
	{
		return "Direct access to hardware via TCP, using " + toString ( IPbusProtocolVersion );
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	PackingProtocol& IPBusTCPClient< IPbusProtocolVersion >::getPackingProtocol()
	{
		try
		{
			return mPackingProtocol;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	TransportProtocol& IPBusTCPClient< IPbusProtocolVersion >::getTransportProtocol()
	{
		try
		{
			return mTransportProtocol;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}



















	template< eIPbusProtocolVersion IPbusProtocolVersion >

ControlHubClient< IPbusProtocolVersion >::ControlHubClient ( const std::string& aId , const URI& aUri ) try :
		ClientInterface ( aId , aUri ),
						mTargetId ( ExtractTargetID ( aUri ) ),
						mPackingProtocol ( mTargetId.first , mTargetId.second , mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
						mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
	{
		Link ( mTransportProtocol , mPackingProtocol );
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw uhal::exception ( aExc );
	}



	template< eIPbusProtocolVersion IPbusProtocolVersion >
	std::string ControlHubClient< IPbusProtocolVersion >::description()
	{
		return "Hardware access via the Control Hub, using " + toString ( IPbusProtocolVersion );
	}



	template< eIPbusProtocolVersion IPbusProtocolVersion >
	PackingProtocol& ControlHubClient< IPbusProtocolVersion >::getPackingProtocol()
	{
		try
		{
			return mPackingProtocol;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	TransportProtocol& ControlHubClient< IPbusProtocolVersion >::getTransportProtocol()
	{
		try
		{
			return mTransportProtocol;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}















}

