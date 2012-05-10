/*
	@file
	@author Andrew W. Rose
	@date 2012
*/

#include "uhal/IPbusPacketInfo.hpp"
#include "uhal/log.hpp"
#include <arpa/inet.h>


// std::ostream& operator<< ( std::ostream& aStream, const uhal::IPbusPacketInfo& aIPbusPacketInfo )
// {
// try
// {
// /*	if( aIPbusPacketInfo.mTransactionHeader.size()==0 ){
// aStream << "'IPbusPacketInfo with no type'" << std::endl;
// return aStream;
// }

// aStream << std::hex << std::uppercase << std::setfill('0') << "Header : 0x" << std::setw(8) << aIPbusPacketInfo.mTransactionHeader.at(0) << std::endl;
// if( aIPbusPacketInfo.mTransactionHeader.size()==2 )
// aStream << std::hex << std::uppercase << std::setfill('0') << "Base Address : 0x" << std::setw(8) << aIPbusPacketInfo.mTransactionHeader.at(1) << std::endl;

// if( aIPbusPacketInfo.mPayload.size() ){
// aStream << "Payload :\n" ;

// std::vector<uint32_t>::const_iterator lIt;
// int i;

// for( lIt = aIPbusPacketInfo.mPayload.begin() , i = 0 ; lIt != aIPbusPacketInfo.mPayload.end(); ++lIt , ++i )
// aStream << "\t" << std::dec << i << "\t" << std::hex << std::setw(8) << *lIt << "\n";
// }

// aStream << std::hex << std::uppercase << std::setfill('0')
// << "aTransactionId = 0x" << std::setw(8) << aIPbusPacketInfo.transactionId()
// << ", mType = 0x" << std::setw(8) << (aIPbusPacketInfo.type()>>3)
// << std::dec
// << ", mWordCount = " << aIPbusPacketInfo.wordCount()
// << ", #DeviceIDs = " << aIPbusPacketInfo.mDeviceIDs.size()
// << ", Payload size = " << aIPbusPacketInfo.mPayload.size()
// << ".";
// */
// aStream << aIPbusPacketInfo.mType << " " <<  aIPbusPacketInfo.mWordCount << " " <<  aIPbusPacketInfo.mBaseAddress << " " <<  aIPbusPacketInfo.mHasBaseAddress
// << " Returning : " <<  aIPbusPacketInfo.ReturnSize() << " (" << aIPbusPacketInfo.ReturnHeaderSize() << " , " <<  aIPbusPacketInfo.ReturnPayloadSize() << ")";
// return aStream;
// }
// catch ( const std::exception& aExc )
// {
// pantheios::log_EXCEPTION ( aExc );
// throw uhal::exception ( aExc );
// }
// }

// Using the uhal namespace
namespace uhal
{

	bool IPbusPacketInfo::operator== ( const uhal::IPbusPacketInfo& aIPbusPacketInfo )
	{
		try
		{
			if ( aIPbusPacketInfo.mType != mType )
			{
				return false;
			}

			if ( aIPbusPacketInfo.mWordCount != mWordCount )
			{
				return false;
			}

			if ( aIPbusPacketInfo.mBaseAddress != mBaseAddress )
			{
				return false;
			}

			if ( aIPbusPacketInfo.mPayload != mPayload )
			{
				return false;
			}

			/*
					//we cannot have two identical device IDs in the same IPbus Packet as this causes merry hell.
					for ( std::vector<uint32_t>::const_iterator lIt1 = a1.mDeviceIDs.begin() ; lIt1 != a1.mDeviceIDs.end() ; ++lIt1 )
					{
						for ( std::vector<uint32_t>::const_iterator lIt2 = a2.mDeviceIDs.begin() ; lIt2 != a2.mDeviceIDs.end() ; ++lIt2 )
						{
							if ( *lIt1 == *lIt2 )
							{
								return false;
							}
						}
					}
			*/
			//so must be identical...
			return true;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


IPbusPacketInfo::IPbusPacketInfo() try :
		mType ( B_O_T ),
			  mWordCount ( 0 ),
			  mBaseAddress ( 0 ),
			  mHasBaseAddress ( 0 )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	IPbusPacketInfo::~IPbusPacketInfo() {}

	void IPbusPacketInfo::setHeader ( const eIPbusTransactionType& aType,
									  const uint32_t& aWordCount
									)
	{
		try
		{
			mType = aType;
			mWordCount = aWordCount;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void IPbusPacketInfo::setHeader ( const eIPbusTransactionType& aType ,
									  const uint32_t& aWordCount ,
									  const uint32_t& aBaseAddress
									)
	{
		try
		{
			mType = aType;
			mWordCount = aWordCount;
			mBaseAddress = aBaseAddress ;
			mHasBaseAddress = true;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	void IPbusPacketInfo::setPayload ( const uint32_t& aPayload )
	{
		try
		{
			mPayload.push_back ( aPayload );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void IPbusPacketInfo::setPayload ( const std::vector<uint32_t>& aPayload )
	{
		try
		{
			mPayload.insert ( mPayload.end() , aPayload.begin() , aPayload.end() );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	// void IPbusPacketInfo::setPayload( const std::vector<uint32_t>::const_iterator& aBegin , const std::vector<uint32_t>::const_iterator& aEnd )
	// {
	// mPayload.insert( mPayload.end() , aBegin , aEnd );
	// }


	void IPbusPacketInfo::setDeviceID ( const uint32_t& aDeviceID )
	{
		try
		{
			mDeviceIDs.push_back ( htonl ( aDeviceID ) );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void IPbusPacketInfo::setAllValMemsValid()
	{
		try
		{
			for ( std::vector< std::pair< uint32_t* , bool* > >::iterator lIt = mValMemPtr.begin(); lIt != mValMemPtr.end(); ++lIt )
			{
				* ( lIt->second ) = true;
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void IPbusPacketInfo::merge ( const IPbusPacketInfo& aIPbusPacketInfo )
	{
		try
		{
			mDeviceIDs.insert ( mDeviceIDs.end() , aIPbusPacketInfo.mDeviceIDs.begin() , aIPbusPacketInfo.mDeviceIDs.end() );
			mValMemPtr.insert ( mValMemPtr.end() , aIPbusPacketInfo.mValMemPtr.begin() , aIPbusPacketInfo.mValMemPtr.end() );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


}




