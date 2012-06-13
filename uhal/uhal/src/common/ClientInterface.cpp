#include "uhal/ClientInterface.hpp"

#include "uhal/Utilities.hpp"
#include <sstream>

namespace uhal
{

ClientInterface::ClientInterface ( const std::string& aId, const URI& aUri ) try :
		mId ( aId ),
			mUri ( aUri )
	{
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw uhal::exception ( aExc );
	}

	ClientInterface::~ClientInterface() {}

	const std::string& ClientInterface::id()
	{
		try
		{
			return mId;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	void ClientInterface::ping()
	{
		try
		{
			std::string lInstruction ( "ping -q -c 1 " + mUri.mHostname + " &> /dev/null" );
			log ( Notice() , "Pinging \"" , mId , "\" with instruction : " , lInstruction );
			//Cant use ICMP here because it requires raw socket (and hence superuser) access, so use system PING instead
			int lPingStatus = system ( lInstruction.c_str() );

			if ( WEXITSTATUS ( lPingStatus ) )
			{
				log ( Error() , "Ping returned exit status ", Integer ( WEXITSTATUS ( lPingStatus ) ) );
				log ( Error() , "Throwing at " , ThisLocation() );
				throw PingFailed();
			}
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	std::string ClientInterface::url()
	{
		try
		{
			std::stringstream lReturn;
			// url is always of the form "protocol://hostname:port"
			lReturn << mUri.mProtocol << "://" << mUri.mHostname << ":" << mUri.mPort;

			// there is sometimes a path
			if ( mUri.mPath != __PRETTY_FUNCTION__ )
			{
				lReturn << "/" << mUri.mPath;
			}

			// there is sometimes a filename extension
			if ( mUri.mExtension != __PRETTY_FUNCTION__ )
			{
				lReturn << "." << mUri.mExtension;
			}

			// there are sometimes arguments
			if ( mUri.mArguments.size() )
			{
				lReturn << "?";
				uhal::NameValuePairVectorType::const_iterator lIt = mUri.mArguments.begin();

				while ( true )
				{
					lReturn << lIt->first << "=" << lIt->second;

					if ( ++lIt == mUri.mArguments.end() )
					{
						break;
					}

					lReturn << "&";
				}
			}

			return lReturn.str();
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}


	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	void ClientInterface::write ( const uint32_t& aAddr, const uint32_t& aSource )
	{
		try
		{
			getPackingProtocol().write ( aAddr , aSource );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	void ClientInterface::write ( const uint32_t& aAddr, const uint32_t& aSource, const uint32_t& aMask )
	{
		try
		{
			getPackingProtocol().rmw_bits ( aAddr , ~aMask , ( aSource << utilities::TrailingRightBits ( aMask ) ) & aMask );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	void ClientInterface::writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			getPackingProtocol().writeBlock ( aAddr, aSource, aMode );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr )
	{
		try
		{
			return getPackingProtocol().read ( aAddr );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr, const uint32_t& aMask )
	{
		try
		{
			return getPackingProtocol().read ( aAddr, aMask );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	ValVector< uint32_t > ClientInterface::readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			return getPackingProtocol().readBlock ( aAddr, aSize, aMode );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< int32_t > ClientInterface::readSigned ( const uint32_t& aAddr )
	{
		try
		{
			return getPackingProtocol().readSigned ( aAddr );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	ValWord< int32_t > ClientInterface::readSigned ( const uint32_t& aAddr, const uint32_t& aMask )
	{
		try
		{
			return getPackingProtocol().readSigned ( aAddr , aMask );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	ValVector< int32_t > ClientInterface::readBlockSigned ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			return getPackingProtocol().readBlockSigned ( aAddr, aSize, aMode );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValVector< uint32_t > ClientInterface::readReservedAddressInfo ()
	{
		try
		{
			return getPackingProtocol().readReservedAddressInfo ();
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< uint32_t > ClientInterface::rmw_bits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm )
	{
		try
		{
			return getPackingProtocol().rmw_bits ( aAddr , aANDterm , aORterm );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< int32_t > ClientInterface::rmw_sum ( const uint32_t& aAddr , const int32_t& aAddend )
	{
		try
		{
			return getPackingProtocol().rmw_sum ( aAddr , aAddend );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	void ClientInterface::dispatch ()
	{
		try
		{
			getPackingProtocol().Dispatch();
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



	void ClientInterface::setTimeoutPeriod ( const uint32_t& aTimeoutPeriod )
	{
		getTransportProtocol().setTimeoutPeriod ( aTimeoutPeriod );
	}

	const uint32_t& ClientInterface::getTimeoutPeriod()
	{
		return getTransportProtocol().getTimeoutPeriod();
	}

}
