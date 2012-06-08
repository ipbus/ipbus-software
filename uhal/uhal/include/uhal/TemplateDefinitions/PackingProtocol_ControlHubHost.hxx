#include <uhal/performance.hpp>

namespace uhal
{

	template< eIPbusProtocolVersion IPbusProtocolVersion >

ControlHubHostPackingProtocol<  IPbusProtocolVersion >::ControlHubHostPackingProtocol ( const uint32_t& aDeviceIPaddr , const uint16_t& aDevicePort , const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize ) try :
		PackingProtocol ( aMaxSendSize<<2 , aMaxReplySize<<2 ),
						mDeviceIPaddress ( htonl ( aDeviceIPaddr ) ),
						mDevicePort ( htons ( aDevicePort ) ),
						mTransactionCounter ( 0 )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	ControlHubHostPackingProtocol<  IPbusProtocolVersion >::~ControlHubHostPackingProtocol() {}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	uint32_t ControlHubHostPackingProtocol<  IPbusProtocolVersion >::calculateIPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount )
	{
		return IPbusHeaderHelper<IPbusProtocolVersion>::calculate ( aType , aWordCount , mTransactionCounter++ );
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	bool ControlHubHostPackingProtocol<  IPbusProtocolVersion >::extractIPbusHeader (
		const uint32_t& aHeader ,
		eIPbusTransactionType& aType ,
		uint32_t& aWordCount ,
		uint32_t& aTransactionId ,
		uint8_t& aResponseGood )
	{
		return IPbusHeaderHelper<IPbusProtocolVersion>::extract ( aHeader , aType , aWordCount , aTransactionId , aResponseGood );
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol<  IPbusProtocolVersion >::Preamble( )
	{
		PERFORMANCE ( "Add preamble" ,
					  // 12 bytes form the preamble:
					  // Byte-count (4 bytes) will be updated before transmission in Predispatch
					  // Device IP address (4 bytes)
					  // Device Port number (2 bytes)
					  // Word-count (2 bytes) will be updated before transmission in Predispatch
					  // 16 bytes form the preamble reply:
					  // Total Byte-count (4 bytes)
					  // Chunk Byte-count (4 bytes)
					  // Device IP address (4 bytes)
					  // Device Port number (2 bytes)
					  // Error code (2 bytes)
					  mMutex.lock();
					  mPreambles.push_back ( tPreamble() );
					  tPreamble& lPreamble ( mPreambles.back() );
					  mMutex.unlock();
					  lPreamble.mSendByteCountPtr = ( uint32_t* ) ( mCurrentBuffers->send ( ( uint32_t ) ( 0 ) ) );
					  mCurrentBuffers->send ( mDeviceIPaddress );
					  mCurrentBuffers->send ( mDevicePort );
					  lPreamble.mSendWordCountPtr = ( uint16_t* ) ( mCurrentBuffers->send ( ( uint16_t ) ( 0 ) ) );
					  mCurrentBuffers->receive ( lPreamble.mReplyTotalByteCounter );
					  mCurrentBuffers->receive ( lPreamble.mReplyChunkByteCounter );
					  mCurrentBuffers->receive ( lPreamble.mReplyDeviceIPaddress );
					  mCurrentBuffers->receive ( lPreamble.mReplyDevicePort );
					  mCurrentBuffers->receive ( lPreamble.mReplyErrorCode );
					  PackingProtocol::Preamble();
					)
	}

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	void ControlHubHostPackingProtocol<  IPbusProtocolVersion >::Predispatch( )
	{
		PERFORMANCE ( "Complete Preamble" ,
					  mMutex.lock();
					  tPreamble& lPreamble ( mPreambles.back() );
					  mMutex.unlock();
					  uint32_t lByteCount ( mCurrentBuffers->sendCounter() );
					  *lPreamble.mSendByteCountPtr = htonl ( lByteCount-4 );
					  *lPreamble.mSendWordCountPtr = htons ( ( lByteCount-12 ) >>2 );
					)
	}


	// ----------------------------------------------------------------------------------------------------------------------------------------------------------------
	// NOTE! THIS FUNCTION MUST BE THREAD SAFE: THAT IS:
	// IT MUST ONLY USE LOCAL VARIABLES
	//            --- OR ---
	// IT MUST MUTEX PROTECT ACCESS TO MEMBER VARIABLES!
	// ----------------------------------------------------------------------------------------------------------------------------------------------------------------
	template< eIPbusProtocolVersion IPbusProtocolVersion >
	bool ControlHubHostPackingProtocol< IPbusProtocolVersion >::Validate ( Buffers* aBuffers )
	{
		uint8_t* lSendBuffer ( aBuffers->getSendBuffer() );
		uint8_t* lSendBufferEnd ( lSendBuffer+aBuffers->sendCounter() );
		lSendBuffer += 12;
		std::deque< std::pair< uint8_t* , uint32_t > >::iterator lReplyIt ( aBuffers->getReplyBuffer().begin() );
		std::deque< std::pair< uint8_t* , uint32_t > >::iterator lReplyEnd ( aBuffers->getReplyBuffer().end() );
		mMutex.lock();
		tPreamble& lPreamble ( mPreambles.front() );
		mMutex.unlock();
		/*		pantheios::log_NOTICE( "Byte Count 1 : " , pantheios::integer ( *(( uint32_t* )( lReplyIt->first )) , pantheios::fmt::fullHex | 10 ) ,
										" : Memory : " , pantheios::integer ( lPreamble.mReplyTotalByteCounter , pantheios::fmt::fullHex | 10 ) ,
										" : Reply counter : " , pantheios::integer ( aBuffers->replyCounter(), pantheios::fmt::fullHex | 10 )
				);*/
		lReplyIt++;
		/*		pantheios::log_NOTICE( "Byte Count 2 : " , pantheios::integer ( *(( uint32_t* )( lReplyIt->first )) , pantheios::fmt::fullHex | 10 )  ,
										" : Memory : " , pantheios::integer ( lPreamble.mReplyChunkByteCounter , pantheios::fmt::fullHex | 10 ) );*/
		lReplyIt++;

		/*		pantheios::log_NOTICE( "IP : " , pantheios::integer ( *(( uint32_t* )( lReplyIt->first )) , pantheios::fmt::fullHex | 10 )  ,
										" : Memory : " , pantheios::integer ( lPreamble.mReplyDeviceIPaddress , pantheios::fmt::fullHex | 10 ) );*/
		if ( lPreamble.mReplyDeviceIPaddress != mDeviceIPaddress )
		{
			pantheios::log_ERROR ( "Returned IP address " , pantheios::integer ( lPreamble.mReplyDeviceIPaddress , pantheios::fmt::fullHex | 10 ) ,
								   " does not match that sent " , pantheios::integer ( mDeviceIPaddress , pantheios::fmt::fullHex | 10 ) );
			return false;
		}

		lReplyIt++;

		/*		pantheios::log_NOTICE( "PORT : " , pantheios::integer ( *(( uint16_t* )( lReplyIt->first )) , pantheios::fmt::fullHex | 6 )  ,
										" : Memory : " , pantheios::integer ( lPreamble.mReplyDevicePort , pantheios::fmt::fullHex | 6 ) );*/
		if ( lPreamble.mReplyDevicePort != mDevicePort )
		{
			pantheios::log_ERROR ( "Returned Port number " , pantheios::integer ( lPreamble.mReplyDevicePort , pantheios::fmt::fullHex | 10 ) ,
								   " does not match that sent " , pantheios::integer ( mDevicePort , pantheios::fmt::fullHex | 10 ) );
			return false;
		}

		lReplyIt++;

		/*		pantheios::log_NOTICE( "Error code : " , pantheios::integer ( *(( uint16_t* )( lReplyIt->first )) , pantheios::fmt::fullHex | 6 )  ,
										" : Memory : " , pantheios::integer ( lPreamble.mReplyErrorCode , pantheios::fmt::fullHex | 6 ) );*/
		if ( lPreamble.mReplyErrorCode != 0 )
		{
			pantheios::log_ERROR ( "Control Hub reported error code " , pantheios::integer ( lPreamble.mReplyErrorCode , pantheios::fmt::fullHex | 10 ) );
			return false;
		}

		lReplyIt++;
		mMutex.lock();
		mPreambles.pop_front();
		mMutex.unlock();
		bool lRet =  PackingProtocol::Validate ( lSendBuffer ,
					 lSendBufferEnd ,
					 lReplyIt ,
					 lReplyEnd );

		if ( lRet )
		{
			aBuffers->validate();
		}

		return lRet;
	}

}
