
namespace uhal
{
	// // ----------------------------------------------------------------------------------------------------------------

	// //! A class to directly access locally-connected devices via IPbus over UDP
	// template< eIPbusProtocolVersion IPbusProtocolVersion >

	// IPBusUDPClient< IPbusProtocolVersion >::IPBusUDPClient ( const std::string& aId , const URI& aUri ) try :
	// ClientInterface ( aId , aUri ),
	// mPackingProtocol ( mMaxPacketLength ),
	// mTransportProtocol ( mUri.mHostname , //hostname
	// mUri.mPort , //port number
	// mPackingProtocol, //reference to a PackingProtocol object which implements a function to handle the BOOST::ASIO callback
	// mTimeoutPeriod //the timeout period for the TCP transactions in seconds
	// )
	// {}
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }


	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// std::string IPBusUDPClient< IPbusProtocolVersion >::description(){
	// std::stringstream lStr;
	// lStr << "Direct access to hardware via UDP, using IPbus version ";
	// switch ( IPbusProtocolVersion )
	// {
	// case IPbus_1_2:
	// lStr << "1.2";
	// break;
	// case IPbus_1_3:
	// lStr << "1.3";
	// break;
	// case IPbus_1_4:
	// lStr << "1.4";
	// break;
	// case IPbus_2_0:
	// lStr << "2.0";
	// break;
	// }
	// return lStr.str();
	// }


	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// PackingProtocol& IPBusUDPClient< IPbusProtocolVersion >::getPackingProtocol()
	// {
	// try
	// {
	// return mPackingProtocol;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }


	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// TransportProtocol& IPBusUDPClient< IPbusProtocolVersion >::getTransportProtocol()
	// {
	// try
	// {
	// return mTransportProtocol;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }







	// //! A class to directly access locally-connected devices via IPbus over TCP
	// template< eIPbusProtocolVersion IPbusProtocolVersion >

	// IPBusTCPClient< IPbusProtocolVersion >::IPBusTCPClient ( const std::string& aId , const URI& aUri ) try :
	// ClientInterface ( aId , aUri ),
	// mPackingProtocol ( mMaxPacketLength ),
	// mTransportProtocol ( mUri.mHostname , //hostname
	// mUri.mPort , //port number
	// mPackingProtocol, //reference to a PackingProtocol object which implements a function to handle the BOOST::ASIO callback
	// mTimeoutPeriod //the timeout period for the TCP transactions in seconds
	// )
	// {}
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }

	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// std::string IPBusTCPClient< IPbusProtocolVersion >::description(){
	// std::stringstream lStr;
	// lStr << "Direct access to hardware via TCP, using IPbus version ";
	// switch ( IPbusProtocolVersion )
	// {
	// case IPbus_1_2:
	// lStr << "1.2";
	// break;
	// case IPbus_1_3:
	// lStr << "1.3";
	// break;
	// case IPbus_1_4:
	// lStr << "1.4";
	// break;
	// case IPbus_2_0:
	// lStr << "2.0";
	// break;
	// }
	// return lStr.str();
	// }


	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// PackingProtocol& IPBusTCPClient< IPbusProtocolVersion >::getPackingProtocol()
	// {
	// try
	// {
	// return mPackingProtocol;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }

	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// TransportProtocol& IPBusTCPClient< IPbusProtocolVersion >::getTransportProtocol()
	// {
	// try
	// {
	// return mTransportProtocol;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }




















	template< eIPbusProtocolVersion IPbusProtocolVersion >

ControlHubClient< IPbusProtocolVersion >::ControlHubClient ( const std::string& aId , const URI& aUri ) try :
		ClientInterface ( aId , aUri ),
						mTargetId ( ExtractTargetID ( aUri ) ),
						mPackingProtocol ( mTargetId.first , mTargetId.second , mMaxPacketLength , mMaxPacketLength ),
						mTransportProtocol ( mUri.mHostname , mUri.mPort )
	{
		Link ( mTransportProtocol , mPackingProtocol );
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}



	template< eIPbusProtocolVersion IPbusProtocolVersion >
	std::string ControlHubClient< IPbusProtocolVersion >::description()
	{
		std::stringstream lStr;
		lStr << "Hardware access via the Control Hub, using IPbus version ";

		switch ( IPbusProtocolVersion )
		{
			case IPbus_1_2:
				lStr << "1.2";
				break;
			case IPbus_1_3:
				lStr << "1.3";
				break;
			case IPbus_1_4:
				lStr << "1.4";
				break;
			case IPbus_2_0:
				lStr << "2.0";
				break;
		}

		return lStr.str();
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
			pantheios::log_EXCEPTION ( aExc );
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
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}















}

