#include "uhal/ClientInterface.hpp"

#include "uhal/Utilities.hpp"
#include <sstream>

namespace uhal
{
	ClientInterface::ClientInterface ( const std::string& aId, const URI& aUri ):
		mId ( aId ),
		mUri ( aUri )
	{
	}
	
	ClientInterface::~ClientInterface() {}

	const std::string& ClientInterface::id()
	{
		return mId;
	}

	bool ClientInterface::ping()
	{
		//Cant use ICMP here because it requires raw socket (and hence superuser) access, so use system PING instead 
		int lPingStatus = system( ( "ping -q -c 1 " + mUri.mHostname + " &> /dev/null" ).c_str() );
		if( WEXITSTATUS( lPingStatus ) ){
			return false;
		}
		return true;
	}
	
	std::string ClientInterface::url()
	{
		std::stringstream lReturn;
		// url is always of the form "protocol://hostname:port"
		lReturn << mUri.mProtocol << "://" << mUri.mHostname << ":" << mUri.mPort;
		
		// there is sometimes a path
		if( mUri.mPath != "" ) lReturn << "/" << mUri.mPath;
		
		// there is sometimes a filename extension
		if( mUri.mExtension != "" ) lReturn << "." << mUri.mExtension;
		
		// there are sometimes arguments
		if( mUri.mArguments.size() ){
			lReturn << "?";			
			uhal::NameValuePairVectorType::const_iterator lIt = mUri.mArguments.begin();
			while( true ){
				lReturn << lIt->first << "=" << lIt->second;
				if( ++lIt == mUri.mArguments.end() ) break;
				lReturn << "&";
			}
		}
		
		return lReturn.str();
	}

	
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	void ClientInterface::write ( const uint32_t aAddr, const uint32_t& aSource )
	{
		IPbusPacketInfo lPacketInfo; 
		lPacketInfo.setHeader( WRITE , 1 , aAddr );					
		lPacketInfo.setPayload( aSource );			
		getPackingProtocol().pack( lPacketInfo );
	}

	void ClientInterface::write ( const uint32_t& aAddr, const uint32_t& aSource, const uint32_t& aMask )
	{
		IPbusPacketInfo lPacketInfo; 
		lPacketInfo.setHeader( WRITE , 1 , aAddr );					
		lPacketInfo.setPayload( ( aSource << utilities::TrailingRightBits ( aMask ) ) && aMask );			
		getPackingProtocol().pack( lPacketInfo );		
	}

	void ClientInterface::writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode aMode )
	{
		IPbusPacketInfo lPacketInfo; 
		
		if (  aMode == defs::INCREMENTAL ){
			lPacketInfo.setHeader( WRITE , aSource.size() , aAddr );					
		}else{
			lPacketInfo.setHeader( NI_WRITE , aSource.size() , aAddr );							
		}

		lPacketInfo.setPayload( aSource );			
		getPackingProtocol().pack( lPacketInfo );
	}
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	
	
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr )
	{
		ValWord< uint32_t > lValWord( 0x00000000 );

		IPbusPacketInfo lPacketInfo; 
		lPacketInfo.setHeader( READ , 1 , aAddr );
		lPacketInfo.setValMem( lValWord );
		getPackingProtocol().pack( lPacketInfo );
		
		return lValWord;
	}

	ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr, const uint32_t& aMask )
	{
		ValWord< uint32_t > lValWord( 0x00000000 , aMask );

		IPbusPacketInfo lPacketInfo; 
		lPacketInfo.setHeader( READ , 1 , aAddr );
		lPacketInfo.setValMem( lValWord );
		getPackingProtocol().pack( lPacketInfo );
		
		return lValWord;
	}

	ValVector< uint32_t > ClientInterface::readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode aMode )
	{
		ValVector< uint32_t > lValVector( aSize );
		
		IPbusPacketInfo lPacketInfo; 
		if ( aMode == defs::INCREMENTAL ){
			lPacketInfo.setHeader( READ , aSize , aAddr );
		}else{
			lPacketInfo.setHeader( NI_READ , aSize , aAddr );
		}
		
		lPacketInfo.setValMem( lValVector );
		getPackingProtocol().pack( lPacketInfo );

		return lValVector;
	}
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	
	
	
	
	
	void ClientInterface::dispatch ( defs::DispatchMode aMode )
	{
		getPackingProtocol().PreDispatch();
		getTransportProtocol().Dispatch();
		getPackingProtocol().PostDispatch();
	}
			
			

}
