
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
	std::string IPBusUDPClient< IPbusProtocolVersion >::description(){
		std::stringstream lStr;
		lStr << "Direct access to hardware via UDP, using IPbus version ";
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
	std::string IPBusTCPClient< IPbusProtocolVersion >::description(){
		std::stringstream lStr;
		lStr << "Direct access to hardware via TCP, using IPbus version ";
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




	
	
	
	
	

	
	
	
	
	
	

	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >

	void ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::pack ( IPbusPacketInfo& aIPbusPacketInfo )
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


	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >

	PackingProtocol& ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::getPackingProtocol()
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


	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >

	TransportProtocol& ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::getTransportProtocol()
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

	
	
	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >

ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::ControlHubClient ( const std::string& aId , const URI& aUri ) try :
		ClientInterface ( aId , aUri ),
						mPackingProtocolPtr ( NULL ),
						mTransportProtocolPtr ( NULL )
	{
		std::string lHostId ( mUri.mHostname + ":" + mUri.mPort );
			typename ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::tMap::iterator lIt = ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::mMapNameAndPortToCHH.find ( lHostId );

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
		
		//Partial template specialization is a PAIN IN THE ARSE and GCC should optimize this switch away anyway...
		switch ( ControlHubHostPackingProtocolVersion )
		{
			case CHH_1:
				ExtractTargetID1( *this );
				break;
			case CHH_2:
			case CHH_3:
				ExtractTargetID2( *this );
				break;		
		}
			
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	
	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	std::string ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::description(){
		std::stringstream lStr;
		lStr << "Hardware access via the Control Hub, using Control Hub Protocol version ";

		switch ( ControlHubHostPackingProtocolVersion )
		{
			case CHH_1:
				lStr << "1 (Multi target packets with 32-bit identifier)";
				break;
			case CHH_2:
				lStr << "2 (Multi target packets with 48-bit IP+port as identifier)";
				break;		
			case CHH_3:
				lStr << "3 (Single target packets with 48-bit IP+port as identifier)";
				break;		
		}
		
		lStr << " and IPbus version ";
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

	

	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	typename ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::tMap
		ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::mMapNameAndPortToCHH = typename ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >::tMap();


	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ExtractTargetID1( ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >& aCHC )
	{
		try
		{
			aCHC.mTargetId = uint64_t( htonl( boost::lexical_cast< uint32_t > ( aCHC.mUri.mPath ) ) );
			pantheios::log_NOTICE ( "Converted uri path \"" , aCHC.mUri.mPath , 
									"\" to device identifier " , pantheios::integer ( aCHC.mTargetId  , pantheios::fmt::fullHex | 18 ) , 
									" (" , pantheios::integer ( uint32_t(aCHC.mTargetId)  , pantheios::fmt::fullHex | 10 ) , ")"
								);	
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	
	
	template< eControlHubHostPackingProtocolVersion ControlHubHostPackingProtocolVersion , eIPbusProtocolVersion IPbusProtocolVersion >
	void ExtractTargetID2( ControlHubClient< ControlHubHostPackingProtocolVersion , IPbusProtocolVersion >& aCHC )
	{
		try{
			pantheios::log_LOCATION();

			NameValuePairVectorType::iterator lIPstr = aCHC.mUri.mArguments.end();
			NameValuePairVectorType::iterator lPortStr = aCHC.mUri.mArguments.end();
			
			for( NameValuePairVectorType::iterator lIt = aCHC.mUri.mArguments.begin() ; lIt != aCHC.mUri.mArguments.end() ; ++lIt ){
				if( lIPstr == aCHC.mUri.mArguments.end() )
				{
					if( lIt->first == "IP" ) lIPstr=lIt;
				}
				else if( lPortStr == aCHC.mUri.mArguments.end() )
				{
					if( lIt->first == "port" ) lPortStr=lIt;
				}
				else
				{
					break;
				}
			}
			
			if( (lIPstr == aCHC.mUri.mArguments.end()) || (lPortStr == aCHC.mUri.mArguments.end() ) ){
				pantheios::log_ERROR ( "This function expects arguments of the form IP=192.168.200.200&port=50001. One or more of these was missing" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw XMLfileMissingRequiredParameters();
			}
			
			pantheios::log_LOCATION();
			
			std::vector<uint32_t> lIP;
			boost::spirit::qi::phrase_parse (	lIPstr->second.begin() , 
												lIPstr->second.end() , 
												+( boost::spirit::qi::uint_ > -boost::spirit::qi::lit ( "." ) ) , 
												boost::spirit::ascii::space , 
												lIP 
											);

			pantheios::log_LOCATION();
											
			uint16_t lPort = boost::lexical_cast< uint16_t > ( lPortStr->second );
			
			uint32_t lIPaddress = (lIP.at(0)<<24) | (lIP.at(1)<<16) | (lIP.at(2)<<8) | (lIP.at(3));
			aCHC.mTargetId = (uint64_t( htons(lPort) )<<32) | (uint64_t( htonl(lIPaddress) ) ) ;
			pantheios::log_NOTICE ( "Converted IP address string \"" , lIPstr->second , 
									"\" to " , pantheios::integer ( lIP.at(0) ) , "." , pantheios::integer ( lIP.at(1) ) , "." , pantheios::integer ( lIP.at(2) ) , "." , pantheios::integer ( lIP.at(3) ) ,
									" and converted this to " , pantheios::integer ( lIPaddress  , pantheios::fmt::fullHex | 10 ) , 
									". Converted port string \"" , lPortStr->second ,  	
									"\" to " , pantheios::integer ( lPort  , pantheios::fmt::fullHex | 6 ) , 				
									". Full device identifier " , pantheios::integer ( aCHC.mTargetId  , pantheios::fmt::fullHex | 18 ) 
								);
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}	
		
	
}

