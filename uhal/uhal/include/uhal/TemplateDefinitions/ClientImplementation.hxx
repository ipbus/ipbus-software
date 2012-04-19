

namespace uhal
{
	// ----------------------------------------------------------------------------------------------------------------

	//! A class to directly access locally-connected devices via IPbus over UDP
	template< eIPbusProtocolVersion IPbusProtocolVersion >

IPBusUDPClient< IPbusProtocolVersion >::IPBusUDPClient ( const std::string& aId , const URI& aUri ) try :
		ClientInterface ( aId , aUri ),
						mPackingProtocol ( mMaxPacketLength ),
						mTransportProtocol ( mUri.mHostname , //hostname
											 mUri.mPort , //port number
											 mPackingProtocol, //reference to a PackingProtocol object which implements a function to handle the BOOST::ASIO callback
											 mTimeoutPeriod //the timeout period for the TCP transactions in seconds
										   )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
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
			pantheios::log_EXCEPTION ( aExc );
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
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}




	//! A class to directly access locally-connected devices via IPbus over TCP
	template< eIPbusProtocolVersion IPbusProtocolVersion >

IPBusTCPClient< IPbusProtocolVersion >::IPBusTCPClient ( const std::string& aId , const URI& aUri ) try :
		ClientInterface ( aId , aUri ),
						mPackingProtocol ( mMaxPacketLength ),
						mTransportProtocol ( mUri.mHostname , //hostname
											 mUri.mPort , //port number
											 mPackingProtocol, //reference to a PackingProtocol object which implements a function to handle the BOOST::ASIO callback
											 mTimeoutPeriod //the timeout period for the TCP transactions in seconds
										   )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
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
			pantheios::log_EXCEPTION ( aExc );
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
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}





	template< eIPbusProtocolVersion IPbusProtocolVersion >

	void ControlHubClient< IPbusProtocolVersion >::pack ( IPbusPacketInfo& aIPbusPacketInfo )
	{
		try
		{
			mPackingProtocolPtr->pack ( aIPbusPacketInfo , mTargetId );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >

	PackingProtocol& ControlHubClient< IPbusProtocolVersion >::getPackingProtocol()
	{
		try
		{
			return *mPackingProtocolPtr;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >

	TransportProtocol& ControlHubClient< IPbusProtocolVersion >::getTransportProtocol()
	{
		try
		{
			return *mTransportProtocolPtr;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}











	template< eIPbusProtocolVersion IPbusProtocolVersion >

ControlHubClient< IPbusProtocolVersion >::ControlHubClient ( const std::string& aId , const URI& aUri ) try :
		ClientInterface ( aId , aUri ),
						mPackingProtocolPtr ( NULL ),
						mTransportProtocolPtr ( NULL )
	{
		std::string lHostId ( mUri.mHostname + ":" + mUri.mPort );
		typename ControlHubClient< IPbusProtocolVersion >::tMap::iterator lIt = ControlHubClient< IPbusProtocolVersion >::mMapNameAndPortToCHH.find ( lHostId );

		if ( lIt == ControlHubClient::mMapNameAndPortToCHH.end() )
		{
			pantheios::log_NOTICE ( "Creating ControlHubClient Map entry for hostname " , mUri.mHostname , ", port " , mUri.mPort );
			mPackingProtocolPtr = new tPackingProtocol ( mMaxPacketLength );
			mTransportProtocolPtr = new tTransportProtocol ( mUri.mHostname , //hostname
					mUri.mPort , //port number
					*mPackingProtocolPtr, //reference to a PackingProtocol object which implements a function to handle the BOOST::ASIO callback
					mTimeoutPeriod //the timeout period for the TCP transactions in seconds
														   );
			ControlHubClient::mMapNameAndPortToCHH.insert (
				std::make_pair (
					lHostId,
					std::make_pair (
						mPackingProtocolPtr,
						mTransportProtocolPtr
					)
				)
			);
		}
		else
		{
			pantheios::log_NOTICE ( "Found existing entry in ControlHubClient Map entry for hostname " , mUri.mHostname , ", port " , mUri.mPort );
			mPackingProtocolPtr = lIt->second.first;
			mTransportProtocolPtr = lIt->second.second;
		}

		mTargetId = boost::lexical_cast< uint32_t > ( mUri.mPath );
		pantheios::log_NOTICE ( "Converted uri path \"" , mUri.mPath , "\" to device identifier " , pantheios::integer ( mTargetId  , pantheios::fmt::fullHex | 10 ) );
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	typename ControlHubClient< IPbusProtocolVersion >::tMap ControlHubClient< IPbusProtocolVersion >::mMapNameAndPortToCHH = typename ControlHubClient< IPbusProtocolVersion >::tMap();

}


