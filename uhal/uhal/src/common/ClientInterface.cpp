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
		pantheios::log_EXCEPTION ( aExc );
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
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void ClientInterface::ping()
	{
		try
		{
			std::string lInstruction ( "ping -q -c 1 " + mUri.mHostname + " &> /dev/null" );
			pantheios::log_NOTICE ( "Pinging \"" , mId , "\" with instruction : " , lInstruction );
			//Cant use ICMP here because it requires raw socket (and hence superuser) access, so use system PING instead
			int lPingStatus = system ( lInstruction.c_str() );

			if ( WEXITSTATUS ( lPingStatus ) )
			{
				pantheios::log_ERROR ( "Ping returned exit status ", pantheios::integer ( WEXITSTATUS ( lPingStatus ) ) );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw PingFailed();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
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
			pantheios::log_EXCEPTION ( aExc );
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
			pantheios::log_EXCEPTION ( aExc );
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
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void ClientInterface::writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			getPackingProtocol().writeBlock ( aAddr, aSource, aMode );
			// IPbusPacketInfo lIPbusPacketInfo;
			// if ( aMode == defs::INCREMENTAL )
			// {
			// lIPbusPacketInfo.setHeader ( WRITE , aSource.size() , aAddr );
			// }
			// else
			// {
			// lIPbusPacketInfo.setHeader ( NI_WRITE , aSource.size() , aAddr );
			// }
			// lIPbusPacketInfo.setPayload ( aSource );
			// pack ( lIPbusPacketInfo );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
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
			// mUnsignedReplyWords.push_back ( ValWord< uint32_t > ( 0x00000000 ) );
			// IPbusPacketInfo lIPbusPacketInfo;
			// lIPbusPacketInfo.setHeader ( READ , 1 , aAddr );
			// lIPbusPacketInfo.setValMem ( mUnsignedReplyWords.back() );
			// pack ( lIPbusPacketInfo );
			// return mUnsignedReplyWords.back();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr, const uint32_t& aMask )
	{
		try
		{
			return getPackingProtocol().read ( aAddr, aMask );
			// mUnsignedReplyWords.push_back ( ValWord< uint32_t > ( 0x00000000 , aMask ) );
			// IPbusPacketInfo lIPbusPacketInfo;
			// lIPbusPacketInfo.setHeader ( READ , 1 , aAddr );
			// lIPbusPacketInfo.setValMem ( mUnsignedReplyWords.back() );
			// pack ( lIPbusPacketInfo );
			// return mUnsignedReplyWords.back();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	ValVector< uint32_t > ClientInterface::readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			return getPackingProtocol().readBlock ( aAddr, aSize, aMode );
						  // mUnsignedReplyVectors.push_back ( ValVector< uint32_t > ( aSize ) );
						  // IPbusPacketInfo lIPbusPacketInfo;
						  // if ( aMode == defs::INCREMENTAL )
						  // {
						  // lIPbusPacketInfo.setHeader ( READ , aSize , aAddr );
						  // }
						  // else
						  // {
						  // lIPbusPacketInfo.setHeader ( NI_READ , aSize , aAddr );
						  // }
						  // lIPbusPacketInfo.setValMem ( mUnsignedReplyVectors.back() );
						  // pack ( lIPbusPacketInfo );
			// return mUnsignedReplyVectors.back();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
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
			// mSignedReplyWords.push_back ( ValWord< int32_t > ( 0x00000000 ) );
			// IPbusPacketInfo lIPbusPacketInfo;
			// lIPbusPacketInfo.setHeader ( READ , 1 , aAddr );
			// lIPbusPacketInfo.setValMem ( mSignedReplyWords.back() );
			// pack ( lIPbusPacketInfo );
			// return mSignedReplyWords.back();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	ValWord< int32_t > ClientInterface::readSigned ( const uint32_t& aAddr, const uint32_t& aMask )
	{
		try
		{
			return getPackingProtocol().readSigned ( aAddr , aMask );
			// mSignedReplyWords.push_back ( ValWord< int32_t > ( 0x00000000 , aMask ) );
			// IPbusPacketInfo lIPbusPacketInfo;
			// lIPbusPacketInfo.setHeader ( READ , 1 , aAddr );
			// lIPbusPacketInfo.setValMem ( mSignedReplyWords.back() );
			// pack ( lIPbusPacketInfo );
			// return mSignedReplyWords.back();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	ValVector< int32_t > ClientInterface::readBlockSigned ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			return getPackingProtocol().readBlockSigned ( aAddr, aSize, aMode );
			// mSignedReplyVectors.push_back ( ValVector< int32_t > ( aSize ) );
			// IPbusPacketInfo lIPbusPacketInfo;
			// if ( aMode == defs::INCREMENTAL )
			// {
			// lIPbusPacketInfo.setHeader ( READ , aSize , aAddr );
			// }
			// else
			// {
			// lIPbusPacketInfo.setHeader ( NI_READ , aSize , aAddr );
			// }
			// lIPbusPacketInfo.setValMem ( mSignedReplyVectors.back() );
			// pack ( lIPbusPacketInfo );
			// return mSignedReplyVectors.back();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
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
			// mUnsignedReplyVectors.push_back ( ValVector< uint32_t > ( 2 ) );
			// IPbusPacketInfo lIPbusPacketInfo;
			// lIPbusPacketInfo.setHeader ( R_A_I , 0 );
			// lIPbusPacketInfo.setValMem ( mUnsignedReplyVectors.back() );
			// pack ( lIPbusPacketInfo );
			// return mUnsignedReplyVectors.back();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
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
			// mUnsignedReplyWords.push_back ( ValWord< uint32_t > ( 0x00000000 ) );
			// IPbusPacketInfo lIPbusPacketInfo;
			// lIPbusPacketInfo.setHeader ( RMW_BITS , 2 , aAddr );
			// lIPbusPacketInfo.setPayload ( aANDterm );
			// lIPbusPacketInfo.setPayload ( aORterm );
			// lIPbusPacketInfo.setValMem ( mUnsignedReplyWords.back() );
			// pack ( lIPbusPacketInfo );
			// return mUnsignedReplyWords.back();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
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
			// mSignedReplyWords.push_back ( ValWord< int32_t > ( 0x00000000 ) );
			// IPbusPacketInfo lIPbusPacketInfo;
			// lIPbusPacketInfo.setHeader ( RMW_SUM , 1 , aAddr );
			// lIPbusPacketInfo.setPayload ( aAddend );
			// lIPbusPacketInfo.setValMem ( mSignedReplyWords.back() );
			// pack ( lIPbusPacketInfo );
			// return mSignedReplyWords.back();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
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
			// PERFORMANCE( "Call the packing protocols pre-dispatch function" ,
			// getPackingProtocol().PreDispatch();
			// )
			// PERFORMANCE( "Call the transport protocols dispatch function" ,
			// getTransportProtocol().Dispatch();
			// )
			// PERFORMANCE( "Call the packing protocols post-dispatch function" ,
			// getPackingProtocol().PostDispatch();
			// )
			// mUnsignedReplyWords.clear();
			// mSignedReplyWords.clear();
			// mUnsignedReplyVectors.clear();
			// mSignedReplyVectors.clear();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



	void ClientInterface::setTimeoutPeriod( const uint32_t& aTimeoutPeriod )
	{
		getTransportProtocol().setTimeoutPeriod( aTimeoutPeriod );
	}

	const uint32_t& ClientInterface::getTimeoutPeriod()
	{
		return getTransportProtocol().getTimeoutPeriod();
	}

}
