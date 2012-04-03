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
		IPbusPacketInfo lIPbusPacketInfo; 
		lIPbusPacketInfo.setHeader( WRITE , 1 , aAddr );					
		lIPbusPacketInfo.setPayload( aSource );			
		pack( lIPbusPacketInfo );
	}

	void ClientInterface::write ( const uint32_t& aAddr, const uint32_t& aSource, const uint32_t& aMask )
	{
		IPbusPacketInfo lIPbusPacketInfo; 
		lIPbusPacketInfo.setHeader( WRITE , 1 , aAddr );					
		lIPbusPacketInfo.setPayload( ( aSource << utilities::TrailingRightBits ( aMask ) ) && aMask );			
		pack( lIPbusPacketInfo );		
	}

	void ClientInterface::writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode aMode )
	{
		IPbusPacketInfo lIPbusPacketInfo; 
		
		if (  aMode == defs::INCREMENTAL ){
			lIPbusPacketInfo.setHeader( WRITE , aSource.size() , aAddr );					
		}else{
			lIPbusPacketInfo.setHeader( NI_WRITE , aSource.size() , aAddr );							
		}

		lIPbusPacketInfo.setPayload( aSource );			
		pack( lIPbusPacketInfo );
	}
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	
	
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr )
	{
		ValWord< uint32_t > lValWord( 0x00000000 );

		IPbusPacketInfo lIPbusPacketInfo; 
		lIPbusPacketInfo.setHeader( READ , 1 , aAddr );
		lIPbusPacketInfo.setValMem( lValWord );
		pack( lIPbusPacketInfo );
		
		return lValWord;
	}

	ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr, const uint32_t& aMask )
	{
		ValWord< uint32_t > lValWord( 0x00000000 , aMask );

		IPbusPacketInfo lIPbusPacketInfo; 
		lIPbusPacketInfo.setHeader( READ , 1 , aAddr );
		lIPbusPacketInfo.setValMem( lValWord );
		pack( lIPbusPacketInfo );
		
		return lValWord;
	}

	ValVector< uint32_t > ClientInterface::readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode aMode )
	{
		ValVector< uint32_t > lValVector( aSize );
		
		IPbusPacketInfo lIPbusPacketInfo; 
		if ( aMode == defs::INCREMENTAL ){
			lIPbusPacketInfo.setHeader( READ , aSize , aAddr );
		}else{
			lIPbusPacketInfo.setHeader( NI_READ , aSize , aAddr );
		}
		
		lIPbusPacketInfo.setValMem( lValVector );
		pack( lIPbusPacketInfo );

		return lValVector;
	}
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValVector< uint32_t > ClientInterface::readReservedAddressInfo (){
		ValVector< uint32_t > lValVector( 2 );
	
		IPbusPacketInfo lIPbusPacketInfo; 
		lIPbusPacketInfo.setHeader( R_A_I , 0 );
		lIPbusPacketInfo.setValMem( lValVector );
		pack( lIPbusPacketInfo );
		
		return lValVector;
	}	
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< uint32_t > ClientInterface::rmw_bits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm )
	{
		ValWord< uint32_t > lValWord;

		IPbusPacketInfo lIPbusPacketInfo; 
		lIPbusPacketInfo.setHeader( RMW_BITS , 2 , aAddr );
		lIPbusPacketInfo.setPayload( aANDterm );
		lIPbusPacketInfo.setPayload( aORterm );
		lIPbusPacketInfo.setValMem( lValWord );
		pack( lIPbusPacketInfo );
		
		return lValWord;
	}
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< int32_t > ClientInterface::rmw_sum ( const uint32_t& aAddr , const int32_t& aAddend )
	{
		ValWord< int32_t > lValWord;

		IPbusPacketInfo lIPbusPacketInfo; 
		lIPbusPacketInfo.setHeader( RMW_SUM , 1 , aAddr );
		lIPbusPacketInfo.setPayload( aAddend );
		lIPbusPacketInfo.setValMem( lValWord );
		pack( lIPbusPacketInfo );
		
		return lValWord;
	}	
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	void ClientInterface::pack( IPbusPacketInfo& aIPbusPacketInfo )
	{
		getPackingProtocol().pack( aIPbusPacketInfo );
	}
		
	
	bool ClientInterface::dispatch ( defs::DispatchMode aMode )
	{
		getPackingProtocol().PreDispatch();
		if ( !getTransportProtocol().Dispatch() ) return false;
		if ( !getPackingProtocol().PostDispatch() ) return false;
		return true;
	}
			
			

}
