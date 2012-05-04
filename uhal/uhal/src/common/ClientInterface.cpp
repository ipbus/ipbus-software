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
			if ( mUri.mPath != "" )
			{
				lReturn << "/" << mUri.mPath;
			}

			// there is sometimes a filename extension
			if ( mUri.mExtension != "" )
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
			IPbusPacketInfo lIPbusPacketInfo;
			lIPbusPacketInfo.setHeader ( WRITE , 1 , aAddr );
			lIPbusPacketInfo.setPayload ( aSource );
			pack ( lIPbusPacketInfo );
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
			// If we are writing to a masked sub-field then we expect to preserve all the other sub-fields.
			// We must, therefore, use Read-Modify-Write, rather than a plain old write
			IPbusPacketInfo lIPbusPacketInfo;
			// lIPbusPacketInfo.setHeader ( WRITE , 1 , aAddr );
			// lIPbusPacketInfo.setPayload ( ( aSource << utilities::TrailingRightBits ( aMask ) ) & aMask );
			mUnsignedReplyWords.push_back ( ValWord< uint32_t > ( 0x00000000 ) );
			lIPbusPacketInfo.setHeader ( RMW_BITS , 2 , aAddr );
			lIPbusPacketInfo.setPayload ( ~aMask );
			lIPbusPacketInfo.setPayload ( ( aSource << utilities::TrailingRightBits ( aMask ) ) & aMask );
			lIPbusPacketInfo.setValMem ( mUnsignedReplyWords.back() );
			pack ( lIPbusPacketInfo );
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
			IPbusPacketInfo lIPbusPacketInfo;

			if ( aMode == defs::INCREMENTAL )
			{
				lIPbusPacketInfo.setHeader ( WRITE , aSource.size() , aAddr );
			}
			else
			{
				lIPbusPacketInfo.setHeader ( NI_WRITE , aSource.size() , aAddr );
			}

			lIPbusPacketInfo.setPayload ( aSource );
			pack ( lIPbusPacketInfo );
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
			mUnsignedReplyWords.push_back ( ValWord< uint32_t > ( 0x00000000 ) );
			IPbusPacketInfo lIPbusPacketInfo;
			lIPbusPacketInfo.setHeader ( READ , 1 , aAddr );
			lIPbusPacketInfo.setValMem ( mUnsignedReplyWords.back() );
			pack ( lIPbusPacketInfo );
			return mUnsignedReplyWords.back();
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
			mUnsignedReplyWords.push_back ( ValWord< uint32_t > ( 0x00000000 , aMask ) );
			IPbusPacketInfo lIPbusPacketInfo;
			lIPbusPacketInfo.setHeader ( READ , 1 , aAddr );
			lIPbusPacketInfo.setValMem ( mUnsignedReplyWords.back() );
			pack ( lIPbusPacketInfo );
			return mUnsignedReplyWords.back();
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
			mUnsignedReplyVectors.push_back ( ValVector< uint32_t > ( aSize ) );
			IPbusPacketInfo lIPbusPacketInfo;

			if ( aMode == defs::INCREMENTAL )
			{
				lIPbusPacketInfo.setHeader ( READ , aSize , aAddr );
			}
			else
			{
				lIPbusPacketInfo.setHeader ( NI_READ , aSize , aAddr );
			}

			lIPbusPacketInfo.setValMem ( mUnsignedReplyVectors.back() );
			pack ( lIPbusPacketInfo );
			return mUnsignedReplyVectors.back();
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
			mSignedReplyWords.push_back ( ValWord< int32_t > ( 0x00000000 ) );
			IPbusPacketInfo lIPbusPacketInfo;
			lIPbusPacketInfo.setHeader ( READ , 1 , aAddr );
			lIPbusPacketInfo.setValMem ( mSignedReplyWords.back() );
			pack ( lIPbusPacketInfo );
			return mSignedReplyWords.back();
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
			mSignedReplyWords.push_back ( ValWord< int32_t > ( 0x00000000 , aMask ) );
			IPbusPacketInfo lIPbusPacketInfo;
			lIPbusPacketInfo.setHeader ( READ , 1 , aAddr );
			lIPbusPacketInfo.setValMem ( mSignedReplyWords.back() );
			pack ( lIPbusPacketInfo );
			return mSignedReplyWords.back();
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
			mSignedReplyVectors.push_back ( ValVector< int32_t > ( aSize ) );
			IPbusPacketInfo lIPbusPacketInfo;

			if ( aMode == defs::INCREMENTAL )
			{
				lIPbusPacketInfo.setHeader ( READ , aSize , aAddr );
			}
			else
			{
				lIPbusPacketInfo.setHeader ( NI_READ , aSize , aAddr );
			}

			lIPbusPacketInfo.setValMem ( mSignedReplyVectors.back() );
			pack ( lIPbusPacketInfo );
			return mSignedReplyVectors.back();
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
			mUnsignedReplyVectors.push_back ( ValVector< uint32_t > ( 2 ) );
			IPbusPacketInfo lIPbusPacketInfo;
			lIPbusPacketInfo.setHeader ( R_A_I , 0 );
			lIPbusPacketInfo.setValMem ( mUnsignedReplyVectors.back() );
			pack ( lIPbusPacketInfo );
			return mUnsignedReplyVectors.back();
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
			mUnsignedReplyWords.push_back ( ValWord< uint32_t > ( 0x00000000 ) );
			IPbusPacketInfo lIPbusPacketInfo;
			lIPbusPacketInfo.setHeader ( RMW_BITS , 2 , aAddr );
			lIPbusPacketInfo.setPayload ( aANDterm );
			lIPbusPacketInfo.setPayload ( aORterm );
			lIPbusPacketInfo.setValMem ( mUnsignedReplyWords.back() );
			pack ( lIPbusPacketInfo );
			return mUnsignedReplyWords.back();
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
			mSignedReplyWords.push_back ( ValWord< int32_t > ( 0x00000000 ) );
			IPbusPacketInfo lIPbusPacketInfo;
			lIPbusPacketInfo.setHeader ( RMW_SUM , 1 , aAddr );
			lIPbusPacketInfo.setPayload ( aAddend );
			lIPbusPacketInfo.setValMem ( mSignedReplyWords.back() );
			pack ( lIPbusPacketInfo );
			return mSignedReplyWords.back();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	void ClientInterface::pack ( IPbusPacketInfo& aIPbusPacketInfo )
	{
		try
		{
			getPackingProtocol().pack ( aIPbusPacketInfo );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	void ClientInterface::dispatch ()
	{
		try
		{
			getPackingProtocol().PreDispatch();
			getTransportProtocol().Dispatch();
			getPackingProtocol().PostDispatch();
			mUnsignedReplyWords.clear();
			mSignedReplyWords.clear();
			mUnsignedReplyVectors.clear();
			mSignedReplyVectors.clear();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}



}
