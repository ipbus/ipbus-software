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
	uint32_t ControlHubHostPackingProtocol<  IPbusProtocolVersion >::IPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount )
	{
		return IPbusHeaderHelper<IPbusProtocolVersion>::calculate ( aType , aWordCount , mTransactionCounter++ );
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

					  mPreambles.push_back ( tPreamble() );
					  tPreamble& lPreamble ( mPreambles.back() );
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
					  tPreamble& lPreamble ( mPreambles.back() );
					  uint32_t lByteCount ( mCurrentBuffers->sendCounter() );
					  *lPreamble.mSendByteCountPtr = htonl ( lByteCount-4 );
					  *lPreamble.mSendWordCountPtr = htons ( ( lByteCount-12 ) >>2 );
					)
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	bool ControlHubHostPackingProtocol< IPbusProtocolVersion >::Validate( Buffers* aBuffers )
	{

		uint8_t* lSendBuffer( aBuffers->getSendBuffer() );
		uint8_t* lSendBufferEnd( lSendBuffer+aBuffers->sendCounter() );

		lSendBuffer += 12;

		std::deque< std::pair< uint8_t* , uint32_t > >::iterator lReplyIt( aBuffers->getReplyBuffer().begin() );
 		std::deque< std::pair< uint8_t* , uint32_t > >::iterator lReplyEnd( aBuffers->getReplyBuffer().end() );

		tPreamble& lPreamble ( mPreambles.front() );

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
		if( lPreamble.mReplyDeviceIPaddress != mDeviceIPaddress ){
			pantheios::log_ERROR( "Returned IP address " , pantheios::integer ( lPreamble.mReplyDeviceIPaddress , pantheios::fmt::fullHex | 10 ) , 
									" does not match that sent " , pantheios::integer ( mDeviceIPaddress , pantheios::fmt::fullHex | 10 ) );
			return false;
		}
		lReplyIt++;

/*		pantheios::log_NOTICE( "PORT : " , pantheios::integer ( *(( uint16_t* )( lReplyIt->first )) , pantheios::fmt::fullHex | 6 )  , 
								" : Memory : " , pantheios::integer ( lPreamble.mReplyDevicePort , pantheios::fmt::fullHex | 6 ) );*/
		if( lPreamble.mReplyDevicePort != mDevicePort ){
			pantheios::log_ERROR( "Returned Port number " , pantheios::integer ( lPreamble.mReplyDevicePort , pantheios::fmt::fullHex | 10 ) , 
									" does not match that sent " , pantheios::integer ( mDevicePort , pantheios::fmt::fullHex | 10 ) );
			return false;
		}
		lReplyIt++;

/*		pantheios::log_NOTICE( "Error code : " , pantheios::integer ( *(( uint16_t* )( lReplyIt->first )) , pantheios::fmt::fullHex | 6 )  , 
								" : Memory : " , pantheios::integer ( lPreamble.mReplyErrorCode , pantheios::fmt::fullHex | 6 ) );*/
		if( lPreamble.mReplyErrorCode != 0 ){
			pantheios::log_ERROR( "Control Hub reported error code " , pantheios::integer ( lPreamble.mReplyErrorCode , pantheios::fmt::fullHex | 10 ) );
			return false;
		}
		lReplyIt++;


		mPreambles.pop_front();

		eIPbusTransactionType lSendIPbusTransactionType , lReplyIPbusTransactionType;
		uint32_t lSendWordCount , lReplyWordCount;
		uint32_t lSendTransactionId , lReplyTransactionId; 
		uint8_t lSendResponseGood , lReplyResponseGood;

 		do{	
	
//  			pantheios::log_NOTICE( "Send Header  : " , pantheios::integer (  *(( uint32_t* )( lSendBuffer )) , pantheios::fmt::fullHex | 10 ) );
//  			pantheios::log_NOTICE( "Reply Header : " , pantheios::integer (  *(( uint32_t* )( lReplyIt->first )) , pantheios::fmt::fullHex | 10 ) );

			if( ! IPbusHeaderHelper< IPbusProtocolVersion >::extract( *(( uint32_t* )( lSendBuffer )) , lSendIPbusTransactionType , lSendWordCount , lSendTransactionId , lSendResponseGood ) ){
				pantheios::log_ERROR( "Unable to parse send header " , pantheios::integer ( *(( uint32_t* )( lSendBuffer )) , pantheios::fmt::fullHex | 10 ) );
				return false;
			} 
	
			if( ! IPbusHeaderHelper< IPbusProtocolVersion >::extract( *(( uint32_t* )( lReplyIt->first )) , lReplyIPbusTransactionType , lReplyWordCount , lReplyTransactionId , lReplyResponseGood ) ){
				pantheios::log_ERROR( "Unable to parse reply header " , pantheios::integer ( *(( uint32_t* )( lReplyIt->first )) , pantheios::fmt::fullHex | 10 ) );
				return false;
			} 

			if( lReplyResponseGood ){
				pantheios::log_ERROR( "Returned Response " , pantheios::integer ( lReplyResponseGood , pantheios::fmt::fullHex | 4 ) , " indicated error" );
				return false;
			}
	
			if( lSendIPbusTransactionType != lReplyIPbusTransactionType ){
				pantheios::log_ERROR( "Returned Transaction Type " , pantheios::integer ( (uint8_t)(lReplyIPbusTransactionType) , pantheios::fmt::fullHex | 4 ) , 
										" does not match that sent " , pantheios::integer ( (uint8_t)(lSendIPbusTransactionType) , pantheios::fmt::fullHex | 4 ) );
				return false;
			}
			
			if( lSendTransactionId != lReplyTransactionId ){
				pantheios::log_ERROR( "Returned Transaction Id " , pantheios::integer ( lReplyTransactionId , pantheios::fmt::fullHex | 10 ) , 
										" does not match that sent " , pantheios::integer ( lSendTransactionId , pantheios::fmt::fullHex | 10 ) );
				return false;
			}

			switch ( lSendIPbusTransactionType )
			{
				case B_O_T:
				case R_A_I:
					lSendBuffer += (1<<2);
					break;
				case NI_READ:
				case READ:
					lSendBuffer += (2<<2);
					break;
				case NI_WRITE:
				case WRITE:
					lSendBuffer += ((2+lSendWordCount)<<2);
					break;
				case RMW_SUM:
					lSendBuffer += (3<<2);
					break;
				case RMW_BITS:
					lSendBuffer += (4<<2);
					break;
			}

			switch ( lReplyIPbusTransactionType )
			{
				case B_O_T:
				case NI_WRITE:
				case WRITE:
					lReplyIt++;
					break;
				case R_A_I:
				case NI_READ:
				case READ:
				case RMW_SUM:
				case RMW_BITS:
					lReplyIt+=2;
					break;
			}

 		}while( (lSendBufferEnd - lSendBuffer != 0) && (lReplyEnd - lReplyIt != 0) );


		aBuffers->validate(); 

// 		uint32_t lPayloadCount( 1 );
// 		uint32_t lNextReplyCount( 1 );


// 		for ( uint32_t i=0 ; i != aBuffers->sendCounter()>>2 ; ++i ){
// 			pantheios::log_NOTICE( pantheios::integer ( *lSendBuffer , pantheios::fmt::fullHex | 10 ) );
// 			lSendBuffer++;
// 		}
	// 		for( ; lReplyIt != lReplyEnd ; ++lReplyIt ){
	// 			uint32_t* lReplyBuffer( ( uint32_t* )( lReplyIt->first ) );
	// 			for( uint32_t i = 0 ; i!=lReplyIt->second>>2 ; ++i ){
	// 				pantheios::log_NOTICE( pantheios::integer ( *lReplyBuffer , pantheios::fmt::fullHex | 10 ) );
	// 				lReplyBuffer++;
	// 			}
	// 		}

		return true;
	}

}
