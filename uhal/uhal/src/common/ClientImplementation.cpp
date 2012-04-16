#include "uhal/ClientImplementation.hpp"

#include "boost/lexical_cast.hpp"

namespace uhal
{

ControlHubClient::ControlHubClient ( const std::string& aId , const URI& aUri ) try :
		ClientInterface ( aId , aUri ),
						mPackingProtocolPtr ( NULL ),
						mTransportProtocolPtr ( NULL )
	{
		std::string lHostId ( mUri.mHostname + ":" + mUri.mPort );
		tMap::iterator lIt = ControlHubClient::mMapNameAndPortToCHH.find ( lHostId );

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

	ControlHubClient::~ControlHubClient ()
	{}


	ControlHubClient::tMap ControlHubClient::mMapNameAndPortToCHH = ControlHubClient::tMap();
}



