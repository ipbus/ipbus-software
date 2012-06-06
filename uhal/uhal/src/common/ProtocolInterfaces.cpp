#include "uhal/ProtocolInterfaces.hpp"
#include "uhal/performance.hpp"

namespace uhal
{

Buffers::Buffers ( const uint32_t& aMaxSendSize ) try :
		mSendCounter ( 0 ),
					 mReplyCounter ( 0 ),
					 mSendBuffer ( new uint8_t[ aMaxSendSize ] )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}


	Buffers::~Buffers()
	{
		PERFORMANCE ( "" ,

					  if ( mSendBuffer )
	{
		delete mSendBuffer;
		mSendBuffer = NULL;
	}
				)
	}


	const uint32_t& Buffers::sendCounter()
	{
		return mSendCounter;
	}
	const uint32_t& Buffers::replyCounter()
	{
		return mReplyCounter;
	}



	uint8_t* Buffers::send ( const uint8_t* aPtr , const uint32_t& aSize )
	{
		PERFORMANCE ( "" ,
					  uint8_t* lStartPtr ( mSendBuffer+mSendCounter );
					  memcpy ( lStartPtr , aPtr , aSize );
					  mSendCounter += aSize;
				)
		return lStartPtr;
	}


	void Buffers::receive ( uint8_t* aPtr , const uint32_t& aSize )
	{
		PERFORMANCE ( "" ,
					  mReplyBuffer.push_back ( std::make_pair ( aPtr , aSize ) );
					  mReplyCounter += aSize;
					)
	}

	void Buffers::add ( const ValHeader& aValMem )
	{
		PERFORMANCE ( "" ,
// 					  mValMems.push_back ( aValMem );
					  mValHeaders.push_back ( aValMem );
					)
	}

	void Buffers::add ( const ValWord< uint32_t >& aValMem )
	{
		PERFORMANCE ( "" ,
// 					  mValMems.push_back ( aValMem );
					  mUnsignedValWords.push_back ( aValMem );
				)
	}

	void Buffers::add ( const ValWord< int32_t >& aValMem )
	{
		PERFORMANCE ( "" ,
// 					  mValMems.push_back ( aValMem );
					  mSignedValWords.push_back ( aValMem );
					)
	}

	void Buffers::add ( const ValVector< uint32_t >& aValMem )
	{
		PERFORMANCE ( "" ,
// 					  mValMems.push_back ( aValMem );
					  mUnsignedValVectors.push_back ( aValMem );
					)
	}

	void Buffers::add ( const ValVector< int32_t >& aValMem )
	{
		PERFORMANCE ( "" ,
// 					  mValMems.push_back ( aValMem );
					  mSignedValVectors.push_back ( aValMem );
					)
	}

	uint8_t* Buffers::getSendBuffer()
	{
		return mSendBuffer;
	}

	std::deque< std::pair< uint8_t* , uint32_t > >& Buffers::getReplyBuffer()
	{
		return mReplyBuffer;
	}


	void Buffers::validate()
	{
		for( std::deque< ValHeader >::iterator lIt = mValHeaders.begin() ; lIt != mValHeaders.end() ; ++lIt ){
			lIt->valid( true );
		} 

		for( std::deque< ValWord< uint32_t > >::iterator lIt = mUnsignedValWords.begin() ; lIt != mUnsignedValWords.end() ; ++lIt ){
			lIt->valid( true );
		} 

		for( std::deque< ValWord< int32_t > >::iterator lIt = mSignedValWords.begin() ; lIt != mSignedValWords.end() ; ++lIt ){
			lIt->valid( true );
		} 

		for( std::deque< ValVector< uint32_t > >::iterator lIt = mUnsignedValVectors.begin() ; lIt != mUnsignedValVectors.end() ; ++lIt ){
			lIt->valid( true );
		} 

		for( std::deque< ValVector< int32_t > >::iterator lIt = mSignedValVectors.begin() ; lIt != mSignedValVectors.end() ; ++lIt ){
			lIt->valid( true );
		}
	}




	void Link ( TransportProtocol& aTransportProtocol , PackingProtocol& aPackingProtocol )
	{
		PERFORMANCE ( "" ,
					  aTransportProtocol.mPackingProtocol = &aPackingProtocol;
					  aPackingProtocol.mTransportProtocol = &aTransportProtocol;
					)
	}


PackingProtocol::PackingProtocol ( const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize ) try :
		mTransportProtocol ( NULL ),
						   mCurrentBuffers ( NULL ),
						   mMaxSendSize ( aMaxSendSize ),
						   mMaxReplySize ( aMaxReplySize )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}

	PackingProtocol::~PackingProtocol()
	{
		PERFORMANCE ( "" ,
	if ( mCurrentBuffers )
	{
		delete mCurrentBuffers;
		mCurrentBuffers = NULL;
	}
				)
	}


	void PackingProtocol::Preamble( )
	{
		PERFORMANCE ( "" ,

					  try
		{
			this->ByteOrderTransaction();
		}
		catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
					)
	}

	void PackingProtocol::Predispatch( )
	{
		pantheios::log_LOCATION();	
	}


	void PackingProtocol::Dispatch( )
	{
		PERFORMANCE ( "" ,
						if( mCurrentBuffers ){
							this->Predispatch();
							mTransportProtocol->Dispatch ( mCurrentBuffers );
							mCurrentBuffers = NULL;
							mTransportProtocol->Flush();
						}
					)
	}




	bool PackingProtocol::Validate( Buffers* aBuffers )
	{
		pantheios::log_NOTICE( "CALL TO BASE" );
// 		uint32_t* lSendBuffer( ( uint32_t* )( aBuffers->getSendBuffer() ) );
// 		std::deque< std::pair< uint8_t* , uint32_t > >::iterator lReplyIt( aBuffers->getReplyBuffer().begin() );
// 		std::deque< std::pair< uint8_t* , uint32_t > >::iterator lReplyEnd( aBuffers->getReplyBuffer().end() );
// 
// 		uint32_t lNextSendCount( 1 );
// 		uint32_t lNextReplyCount( 1 );
// 
// // 		for ( uint32_t i=0 ; i != aBuffers->sendCounter()>>2 ; ++i ){
// // 			pantheios::log_NOTICE( pantheios::integer ( *lSendBuffer , pantheios::fmt::fullHex | 10 ) );
// // 			lSendBuffer++;
// // 		}
// 
// 		for( ; lReplyIt != lReplyEnd ; ++lReplyIt ){
// 			uint32_t* lReplyBuffer( ( uint32_t* )( lReplyIt->first ) );
// 			for( uint32_t i = 0 ; i!=lReplyIt->second>>2 ; ++i ){
// 				pantheios::log_NOTICE( pantheios::integer ( *lReplyBuffer , pantheios::fmt::fullHex | 10 ) );
// 				lReplyBuffer++;
// 			}
// 		}

		return true;
	}




	void PackingProtocol::ByteOrderTransaction()
	{
		try
		{
			PERFORMANCE ( "" ,
						  // IPbus send packet format is:
						  // HEADER
						  uint32_t lSendByteCount ( 1 << 2 );
						  // IPbus reply packet format is:
						  // HEADER
						  uint32_t lReplyByteCount ( 1 << 2 );
						  uint32_t lSendBytesAvailable;
						  uint32_t  lReplyBytesAvailable;
						  this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
						  mCurrentBuffers->send ( this->IPbusHeader ( B_O_T , 0 ) );
						  ValHeader lReply;
						  mCurrentBuffers->add ( lReply );
						  mCurrentBuffers->receive ( lReply.mMembers->IPbusHeader );
					)
			//return lReply;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void PackingProtocol::write ( const uint32_t& aAddr, const uint32_t& aSource )
	{
		try
		{
			PERFORMANCE ( "" ,
						  // IPbus packet format is:
						  // HEADER
						  // BASE ADDRESS
						  // WORD
						  uint32_t lSendByteCount ( 3 << 2 );
						  // IPbus reply packet format is:
						  // HEADER
						  uint32_t lReplyByteCount ( 1 << 2 );
						  uint32_t lSendBytesAvailable;
						  uint32_t  lReplyBytesAvailable;
						  this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
						  mCurrentBuffers->send ( this->IPbusHeader ( WRITE , 1 ) );
						  mCurrentBuffers->send ( aAddr );
						  mCurrentBuffers->send ( aSource );
						  ValHeader lReply;
						  mCurrentBuffers->add ( lReply );
						  mCurrentBuffers->receive ( lReply.mMembers->IPbusHeader );
						)
			//return lReply;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	void PackingProtocol::writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			PERFORMANCE ( "" ,
						  // IPbus packet format is:
						  // HEADER
						  // BASE ADDRESS
						  // WORD
						  // WORD
						  // ....
						  uint32_t lSendHeaderByteCount ( 2 << 2 );
						  // IPbus reply packet format is:
						  // HEADER
						  uint32_t lReplyByteCount ( 1 << 2 );
						  uint32_t lSendBytesAvailable;
						  uint32_t  lReplyBytesAvailable;
						  eIPbusTransactionType lType ( ( aMode == defs::INCREMENTAL ) ? WRITE : NI_WRITE );
						  int32_t lPayloadByteCount ( aSource.size() << 2 );
						  uint8_t* lSourcePtr ( ( uint8_t* ) ( & ( aSource.at ( 0 ) ) ) );
						  uint32_t lAddr ( aAddr );

						  while ( lPayloadByteCount > 0 )
		{
			this->checkBufferSpace ( lSendHeaderByteCount+lPayloadByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
				uint32_t lSendBytesAvailableForPayload ( ( lSendBytesAvailable - lSendHeaderByteCount ) & 0xFFFFFFFC );
				//pantheios::log_NOTICE( "lSendBytesAvailable = " , pantheios::integer(lSendBytesAvailable) );
				//pantheios::log_NOTICE( "lSendBytesAvailableForPayload (bytes) = " , pantheios::integer(lSendBytesAvailableForPayload) );
				//pantheios::log_NOTICE( "lSendBytesAvailableForPayload (words) = " , pantheios::integer(lSendBytesAvailableForPayload>>2) );
				//pantheios::log_NOTICE( "lPayloadByteCount = " , pantheios::integer(lPayloadByteCount) );
				mCurrentBuffers->send ( this->IPbusHeader ( lType , lSendBytesAvailableForPayload>>2 ) );
				mCurrentBuffers->send ( lAddr );
				mCurrentBuffers->send ( lSourcePtr , lSendBytesAvailableForPayload );
				lSourcePtr += lSendBytesAvailableForPayload;
				lPayloadByteCount -= lSendBytesAvailableForPayload;

				if ( aMode == defs::INCREMENTAL )
				{
					lAddr += ( lSendBytesAvailableForPayload>>2 );
				}

				ValHeader lReply;
				mCurrentBuffers->add ( lReply );
				mCurrentBuffers->receive ( lReply.mMembers->IPbusHeader );
			}
						)
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

	ValWord< uint32_t > PackingProtocol::read ( const uint32_t& aAddr, const uint32_t& aMask )
	{
		try
		{
			PERFORMANCE ( "" ,
						  // IPbus packet format is:
						  // HEADER
						  // BASE ADDRESS
						  uint32_t lSendByteCount ( 2 << 2 );
						  // IPbus reply packet format is:
						  // HEADER
						  // WORD
						  uint32_t lReplyByteCount ( 2 << 2 );
						  uint32_t lSendBytesAvailable;
						  uint32_t  lReplyBytesAvailable;
						  this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
						  mCurrentBuffers->send ( this->IPbusHeader ( READ , 1 ) );
						  mCurrentBuffers->send ( aAddr );
						  ValWord< uint32_t > lReply ( 0 , aMask );
						  mCurrentBuffers->add ( lReply );
						  _ValWord_< uint32_t >& lReplyMem = * ( lReply.mMembers );
						  mCurrentBuffers->receive ( lReplyMem.IPbusHeader );
						  mCurrentBuffers->receive ( lReplyMem.value );
						)
			return lReply;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	ValVector< uint32_t > PackingProtocol::readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			PERFORMANCE ( "" ,
						  // IPbus packet format is:
						  // HEADER
						  // BASE ADDRESS
						  uint32_t lSendByteCount ( 2 << 2 );
						  // IPbus reply packet format is:
						  // HEADER
						  // WORD
						  // WORD
						  // ....
						  uint32_t lReplyHeaderByteCount ( 1 << 2 );
						  uint32_t lSendBytesAvailable;
						  uint32_t  lReplyBytesAvailable;
						  ValVector< uint32_t > lReply ( aSize );
						  _ValVector_< uint32_t >& lReplyMem = * ( lReply.mMembers );
						  uint8_t* lReplyPtr = ( uint8_t* ) ( & ( lReplyMem.value[0] ) );
						  eIPbusTransactionType lType ( ( aMode == defs::INCREMENTAL ) ? READ : NI_READ );
						  int32_t lPayloadByteCount ( aSize << 2 );
						  uint32_t lAddr ( aAddr );

						  while ( lPayloadByteCount > 0 )
		{
			this->checkBufferSpace ( lSendByteCount , lReplyHeaderByteCount+lPayloadByteCount , lSendBytesAvailable , lReplyBytesAvailable );
				uint32_t lReplyBytesAvailableForPayload ( ( lReplyBytesAvailable - lReplyHeaderByteCount ) & 0xFFFFFFFC );
				mCurrentBuffers->send ( this->IPbusHeader ( lType , lReplyBytesAvailableForPayload>>2 ) );
				mCurrentBuffers->send ( lAddr );
				lReplyMem.IPbusHeaders.push_back ( 0 );
				mCurrentBuffers->receive ( lReplyMem.IPbusHeaders.back() );
				mCurrentBuffers->receive ( lReplyPtr , lReplyBytesAvailableForPayload );
				lReplyPtr += lReplyBytesAvailableForPayload;
				lPayloadByteCount -= lReplyBytesAvailableForPayload;

				if ( aMode == defs::INCREMENTAL )
				{
					lAddr += ( lReplyBytesAvailableForPayload>>2 );
				}
			}
			mCurrentBuffers->add ( lReply ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
						)
			return lReply;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< int32_t > PackingProtocol::readSigned ( const uint32_t& aAddr, const uint32_t& aMask )
	{
		try
		{
			PERFORMANCE ( "" ,
						  // IPbus packet format is:
						  // HEADER
						  // BASE ADDRESS
						  uint32_t lSendByteCount ( 2 << 2 );
						  // IPbus reply packet format is:
						  // HEADER
						  // WORD
						  uint32_t lReplyByteCount ( 2 << 2 );
						  uint32_t lSendBytesAvailable;
						  uint32_t  lReplyBytesAvailable;
						  this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
						  mCurrentBuffers->send ( this->IPbusHeader ( READ , 1 ) );
						  mCurrentBuffers->send ( aAddr );
						  ValWord< int32_t > lReply ( 0 , aMask );
						  mCurrentBuffers->add ( lReply );
						  _ValWord_< int32_t >& lReplyMem = * ( lReply.mMembers );
						  mCurrentBuffers->receive ( lReplyMem.IPbusHeader );
						  mCurrentBuffers->receive ( lReplyMem.value );
						)
			return lReply;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	ValVector< int32_t > PackingProtocol::readBlockSigned ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			PERFORMANCE ( "" ,
						  // IPbus packet format is:
						  // HEADER
						  // BASE ADDRESS
						  uint32_t lSendByteCount ( 2 << 2 );
						  // IPbus reply packet format is:
						  // HEADER
						  // WORD
						  // WORD
						  // ....
						  uint32_t lReplyHeaderByteCount ( 1 << 2 );
						  uint32_t lSendBytesAvailable;
						  uint32_t  lReplyBytesAvailable;
						  ValVector< int32_t > lReply ( aSize );
						  _ValVector_< int32_t >& lReplyMem = * ( lReply.mMembers );
						  uint8_t* lReplyPtr = ( uint8_t* ) ( & ( lReplyMem.value[0] ) );
						  eIPbusTransactionType lType ( ( aMode == defs::INCREMENTAL ) ? READ : NI_READ );
						  int32_t lPayloadByteCount ( aSize << 2 );
						  uint32_t lAddr ( aAddr );

						  while ( lPayloadByteCount > 0 )
		{
			this->checkBufferSpace ( lSendByteCount , lReplyHeaderByteCount+lPayloadByteCount , lSendBytesAvailable , lReplyBytesAvailable );
				uint32_t lReplyBytesAvailableForPayload ( ( lReplyBytesAvailable - lReplyHeaderByteCount ) & 0xFFFFFFFC );
				mCurrentBuffers->send ( this->IPbusHeader ( lType , lReplyBytesAvailableForPayload>>2 ) );
				mCurrentBuffers->send ( lAddr );
				lReplyMem.IPbusHeaders.push_back ( 0 );
				mCurrentBuffers->receive ( lReplyMem.IPbusHeaders.back() );
				mCurrentBuffers->receive ( lReplyPtr , lReplyBytesAvailableForPayload );
				lReplyPtr += lReplyBytesAvailableForPayload;
				lPayloadByteCount -= lReplyBytesAvailableForPayload;

				if ( aMode == defs::INCREMENTAL )
				{
					lAddr += ( lReplyBytesAvailableForPayload>>2 );
				}
			}
			mCurrentBuffers->add ( lReply ); //we store the valmem in the last chunk so that, if the reply is split over many chunks, the valmem is guaranteed to still exist when the other chunks come back...
						)
			return lReply;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValVector< uint32_t > PackingProtocol::readReservedAddressInfo ()
	{
		try
		{
			PERFORMANCE ( "" ,
						  // IPbus packet format is:
						  // HEADER
						  uint32_t lSendByteCount ( 1 << 2 );
						  // IPbus reply packet format is:
						  // HEADER
						  // WORD
						  // WORD
						  uint32_t lReplyByteCount ( 3 << 2 );
						  uint32_t lSendBytesAvailable;
						  uint32_t  lReplyBytesAvailable;
						  this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
						  mCurrentBuffers->send ( this->IPbusHeader ( R_A_I , 0 ) );
						  ValVector< uint32_t > lReply ( 2 );
						  mCurrentBuffers->add ( lReply );
						  _ValVector_< uint32_t >& lReplyMem = * ( lReply.mMembers );
						  lReplyMem.IPbusHeaders.push_back ( 0 );
						  mCurrentBuffers->receive ( lReplyMem.IPbusHeaders[0] );
						  mCurrentBuffers->receive ( ( uint8_t* ) ( & ( lReplyMem.value[0] ) ) , 2<<2 );
						)
			return lReply;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< uint32_t > PackingProtocol::rmw_bits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm )
	{
		try
		{
			PERFORMANCE ( "" ,
						  // IPbus packet format is:
						  // HEADER
						  // BASE ADDRESS
						  // AND TERM
						  // OR TERM
						  uint32_t lSendByteCount ( 4 << 2 );
						  // IPbus reply packet format is:
						  // HEADER
						  // WORD
						  uint32_t lReplyByteCount ( 2 << 2 );
						  uint32_t lSendBytesAvailable;
						  uint32_t  lReplyBytesAvailable;
						  this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
						  mCurrentBuffers->send ( this->IPbusHeader ( RMW_BITS , 1 ) );
						  mCurrentBuffers->send ( aAddr );
						  mCurrentBuffers->send ( aANDterm );
						  mCurrentBuffers->send ( aORterm );
						  ValWord< uint32_t > lReply ( 0 );
						  mCurrentBuffers->add ( lReply );
						  _ValWord_< uint32_t >& lReplyMem = * ( lReply.mMembers );
						  mCurrentBuffers->receive ( lReplyMem.IPbusHeader );
						  mCurrentBuffers->receive ( lReplyMem.value );
						)
			return lReply;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	ValWord< int32_t > PackingProtocol::rmw_sum ( const uint32_t& aAddr , const int32_t& aAddend )
	{
		try
		{
			PERFORMANCE ( "" ,
						  // IPbus packet format is:
						  // HEADER
						  // BASE ADDRESS
						  // ADDEND
						  uint32_t lSendByteCount ( 3 << 2 );
						  // IPbus reply packet format is:
						  // HEADER
						  // WORD
						  uint32_t lReplyByteCount ( 2 << 2 );
						  uint32_t lSendBytesAvailable;
						  uint32_t  lReplyBytesAvailable;
						  this->checkBufferSpace ( lSendByteCount , lReplyByteCount , lSendBytesAvailable , lReplyBytesAvailable );
						  mCurrentBuffers->send ( this->IPbusHeader ( RMW_SUM , 1 ) );
						  mCurrentBuffers->send ( aAddr );
						  mCurrentBuffers->send ( static_cast< uint32_t > ( aAddend ) );
						  ValWord< int32_t > lReply ( 0 );
						  mCurrentBuffers->add ( lReply );
						  _ValWord_< int32_t >& lReplyMem = * ( lReply.mMembers );
						  mCurrentBuffers->receive ( lReplyMem.IPbusHeader );
						  mCurrentBuffers->receive ( lReplyMem.value );
						)
			return lReply;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	void PackingProtocol::checkBufferSpace ( const uint32_t& aRequestedSendSize , const uint32_t& aRequestedReplySize , uint32_t& aAvailableSendSize , uint32_t& aAvailableReplySize )
	{
		try
		{
			if ( !mCurrentBuffers )
			{
				mCurrentBuffers = new Buffers ( mMaxSendSize );
				this->Preamble();
			}

			uint32_t lSendBufferFreeSpace ( mMaxSendSize - mCurrentBuffers->sendCounter() );
			uint32_t lReplyBufferFreeSpace ( mMaxReplySize - mCurrentBuffers->replyCounter() );
			//pantheios::log_NOTICE( "aRequestedSendSize = " , pantheios::integer(aRequestedSendSize) );
			//pantheios::log_NOTICE( "aRequestedReplySize = " , pantheios::integer(aRequestedReplySize) );
			//pantheios::log_NOTICE( "lSendBufferFreeSpace = " , pantheios::integer(lSendBufferFreeSpace) );
			//pantheios::log_NOTICE( "lReplyBufferFreeSpace = " , pantheios::integer(lReplyBufferFreeSpace) );

			if ( ( aRequestedSendSize <= lSendBufferFreeSpace ) && ( aRequestedReplySize <= lReplyBufferFreeSpace ) )
			{
				aAvailableSendSize = aRequestedSendSize;
				aAvailableReplySize = aRequestedReplySize;
				//pantheios::log_NOTICE( "replying with aAvailableSendSize = " , pantheios::integer(aAvailableSendSize) );
				//pantheios::log_NOTICE( "replying with aAvailableReplySize = " , pantheios::integer(aAvailableReplySize) );
				return;
			}

			if ( ( lSendBufferFreeSpace > 16 ) && ( lReplyBufferFreeSpace > 16 ) )
			{
				aAvailableSendSize = lSendBufferFreeSpace;
				aAvailableReplySize = lReplyBufferFreeSpace;
				//pantheios::log_NOTICE( "replying with aAvailableSendSize = " , pantheios::integer(aAvailableSendSize) );
				//pantheios::log_NOTICE( "replying with aAvailableReplySize = " , pantheios::integer(aAvailableReplySize) );
				return;
			}

			this->Predispatch();
			mTransportProtocol->Dispatch ( mCurrentBuffers );
			//pantheios::log_NOTICE( "Creating new buffer" );
			mCurrentBuffers = new Buffers ( mMaxSendSize );
			this->Preamble();
			lSendBufferFreeSpace = mMaxSendSize - mCurrentBuffers->sendCounter();
			lReplyBufferFreeSpace = mMaxReplySize - mCurrentBuffers->replyCounter();

			if ( ( aRequestedSendSize <= lSendBufferFreeSpace ) && ( aRequestedReplySize <= lReplyBufferFreeSpace ) )
			{
				aAvailableSendSize = aRequestedSendSize;
				aAvailableReplySize = aRequestedReplySize;
				//pantheios::log_NOTICE( "replying with aAvailableSendSize = " , pantheios::integer(aAvailableSendSize) );
				//pantheios::log_NOTICE( "replying with aAvailableReplySize = " , pantheios::integer(aAvailableReplySize) );
				return;
			}

			aAvailableSendSize = lSendBufferFreeSpace;
			aAvailableReplySize = lReplyBufferFreeSpace;
			//pantheios::log_NOTICE( "replying with aAvailableSendSize = " , pantheios::integer(aAvailableSendSize) );
			//pantheios::log_NOTICE( "replying with aAvailableReplySize = " , pantheios::integer(aAvailableReplySize) );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}
	//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}


