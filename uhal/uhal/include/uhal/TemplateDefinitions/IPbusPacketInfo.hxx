

namespace uhal
{

	template< eIPbusProtocolVersion IPbusProtocolVersion >
	uint32_t IPbusHeaderHelper< IPbusProtocolVersion >::calculate ( const eIPbusTransactionType& aType , const uint32_t& aWordCount , const uint32_t& aTransactionId )
	{
		return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( aWordCount&0x1ff ) <<8 ) | aType;
	}


	template< eIPbusProtocolVersion IPbusProtocolVersion >
	bool IPbusHeaderHelper< IPbusProtocolVersion >::extract( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood )
	{
		try
		{
			aType = eIPbusTransactionType( aHeader & 0xF8 );
			aWordCount = ( aHeader >> 8 ) & 0x1ff;
			aTransactionId = ( aHeader >> 17 ) & 0x7ff;
			aResponseGood = aHeader & 0x3;
			return true;
		}
		catch ( const std::exception& aExc )
		{
			return false;
		}
	}


	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// std::string DebugIPbusHeader ( const uint32_t& aHeader )
	// {
	// std::stringstream lStr;

	// switch ( aHeader & 0x000000F8 )
	// {
	// case B_O_T:
	// lStr << "Byte Order Transaction";
	// break;
	// case R_A_I:
	// lStr << "Reserved Address Information";
	// break;
	// case NI_READ:
	// lStr << "Non-incrementing Read";
	// break;
	// case READ:
	// lStr << "Incrementing Read";
	// break;
	// case NI_WRITE:
	// lStr << "Non-incrementing Write";
	// break;
	// case WRITE:
	// lStr << "Incrementing Write";
	// break;
	// case RMW_SUM:
	// lStr << "Read-Modify-Write Sum";
	// break;
	// case RMW_BITS:
	// lStr << "Read-Modify-Write Bits";
	// break;
	// default:
	// lStr << "Unknown IPbus transaction type";
	// }

	// switch ( IPbusProtocolVersion )
	// {
	// case IPbus_1_2:
	// case IPbus_1_3:
	// lStr << " : Transaction ID = " << ( ( aHeader>>17 ) & 0x7ff );
	// lStr << ", WordCount = " << ( ( aHeader>>8 ) & 0x1ff );
	// lStr << ", IPbus Version = " << ( ( aHeader>>28 ) & 0xf );
	// break;
	// case IPbus_1_4:
	// case IPbus_2_0:
	// lStr << " : Transaction ID = " << ( ( aHeader>>8 ) & 0xff );
	// lStr << ", WordCount = " << ( ( aHeader>>16 ) & 0xfff );
	// lStr << ", IPbus Version = " << ( ( aHeader>>28 ) & 0xf );
	// break;
	// }

	// return lStr.str();
	// }



	// template< typename T >
	// void IPbusPacketInfo::setValMem ( ValWord< T >& aValWord )
	// {
	// try
	// {
	// mValMemPtr.push_back (
	// std::make_pair (
	// reinterpret_cast<uint32_t*> ( & ( aValWord.mMembers->value ) ) ,
	// & ( aValWord.mMembers->valid )
	// )
	// );
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }

	// template< typename T >
	// void IPbusPacketInfo::setValMem ( ValVector< T >& aValVector )
	// {
	// try
	// {
	// mValMemPtr.push_back (
	// std::make_pair (
	// reinterpret_cast<uint32_t*> ( & ( aValVector.mMembers->value[0] ) ) ,
	// & ( aValVector.mMembers->valid )
	// )
	// );
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }


	// std::deque< IPbusPacketInfo::tChunks >& IPbusPacketInfo::getChunks()
	// {
	// try
	// {
	// return mChunks;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }

	// const bool& IPbusPacketInfo::hasBaseAddress()
	// {
	// try
	// {
	// return mHasBaseAddress;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }

	// const std::vector<uint64_t>& IPbusPacketInfo::getDeviceIDs()
	// {
	// try
	// {
	// return mDeviceIDs;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }

	// std::size_t IPbusPacketInfo::SendSize() const
	// {
	// try
	// {
	// switch ( mType )
	// {
	// case B_O_T:
	// case R_A_I:
	// return 1;
	// case NI_READ:
	// case READ:
	// return 2;
	// case NI_WRITE:
	// case WRITE:
	// return mWordCount+2;
	// case RMW_SUM:
	// return 3;
	// case RMW_BITS:
	// return 4;
	// }

	// return 0;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }

	// std::size_t IPbusPacketInfo::SendHeaderSize() const
	// {
	// try
	// {
	// switch ( mType )
	// {
	// case B_O_T:
	// case R_A_I:
	// return 1;
	// case NI_READ:
	// case READ:
	// case NI_WRITE:
	// case WRITE:
	// case RMW_SUM:
	// case RMW_BITS:
	// return 2;
	// }

	// return 0;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }


	// std::size_t IPbusPacketInfo::SendPayloadSize() const
	// {
	// try
	// {
	// switch ( mType )
	// {
	// case B_O_T:
	// case R_A_I:
	// case NI_READ:
	// case READ:
	// return 0;
	// case NI_WRITE:
	// case WRITE:
	// return mWordCount;
	// case RMW_SUM:
	// return 1;
	// case RMW_BITS:
	// return 2;
	// }

	// return 0;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }

	// std::size_t IPbusPacketInfo::ReturnSize() const
	// {
	// try
	// {
	// switch ( mType )
	// {
	// case B_O_T:
	// case NI_WRITE:
	// case WRITE:
	// return 1;
	// case NI_READ:
	// case READ:
	// return mWordCount+1;
	// case RMW_SUM:
	// case RMW_BITS:
	// return 2;
	// case R_A_I:
	// return 3;
	// }

	// return 0;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }

	// std::size_t IPbusPacketInfo::ReturnHeaderSize() const
	// {
	// try
	// {
	// return 1;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }

	// std::size_t IPbusPacketInfo::ReturnPayloadSize() const
	// {
	// try
	// {
	// switch ( mType )
	// {
	// case B_O_T:
	// case NI_WRITE:
	// case WRITE:
	// return 0;
	// case NI_READ:
	// case READ:
	// return mWordCount;
	// case RMW_SUM:
	// case RMW_BITS:
	// return 1;
	// case R_A_I:
	// return 2;
	// }

	// return 0;
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }



	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// uint32_t IPbusPacketInfo::calculateHeader ( const uint32_t& aTransactionId , const uint32_t& aWordCount )
	// {
	// try
	// {
	// switch ( IPbusProtocolVersion )
	// {
	// case IPbus_1_2:

	// switch ( mType )
	// {
	// case RMW_BITS:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( 2&0x1ff ) <<8 ) | mType;
	// case B_O_T:
	// case NI_WRITE:
	// case WRITE:
	// case NI_READ:
	// case READ:
	// case RMW_SUM:
	// case R_A_I:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( aWordCount&0x1ff ) <<8 ) | mType;
	// }

	// case IPbus_1_3:

	// switch ( mType )
	// {
	// case RMW_BITS:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( 1&0x1ff ) <<8 ) | mType;
	// case B_O_T:
	// case NI_WRITE:
	// case WRITE:
	// case NI_READ:
	// case READ:
	// case RMW_SUM:
	// case R_A_I:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( aWordCount&0x1ff ) <<8 ) | mType;
	// }

	// case IPbus_1_4:
	// case IPbus_2_0:

	// switch ( mType )
	// {
	// case RMW_BITS:
	// return ( ( 2&0xF ) <<28 ) | ( ( 1&0xfff ) <<16 ) | ( ( aTransactionId&0xff ) <<8 ) | mType | 0xf;
	// case B_O_T:
	// case NI_WRITE:
	// case WRITE:
	// case NI_READ:
	// case READ:
	// case RMW_SUM:
	// case R_A_I:
	// return ( ( 2&0xF ) <<28 ) | ( ( aWordCount&0xfff ) <<16 ) | ( ( aTransactionId&0xff ) <<8 ) | mType | 0xf;
	// }
	// }

	// return uint32_t ( -1 );
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }


	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// uint32_t IPbusPacketInfo::calculateReplyHeader ( const uint32_t& aTransactionId , const uint32_t& aWordCount )
	// {
	// try
	// {
	// switch ( IPbusProtocolVersion )
	// {
	// case IPbus_1_2:

	// switch ( mType )
	// {
	// case RMW_BITS:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( 1&0x1ff ) <<8 ) | mType | 0x4;
	// case R_A_I:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( 2&0x1ff ) <<8 ) | mType | 0x4;
	// case NI_WRITE:
	// case WRITE:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( 0&0x1ff ) <<8 ) | mType | 0x4;
	// case B_O_T:
	// case NI_READ:
	// case READ:
	// case RMW_SUM:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( aWordCount&0x1ff ) <<8 ) | mType | 0x4;
	// }

	// case IPbus_1_3:

	// switch ( mType )
	// {
	// case RMW_BITS:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( 1&0x1ff ) <<8 ) | mType | 0x4;
	// case R_A_I:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( 2&0x1ff ) <<8 ) | mType | 0x4;
	// case NI_WRITE:
	// case WRITE:
	// case B_O_T:
	// case NI_READ:
	// case READ:
	// case RMW_SUM:
	// return ( ( 1&0xF ) <<28 ) | ( ( aTransactionId&0x7ff ) <<17 ) | ( ( aWordCount&0x1ff ) <<8 ) | mType | 0x4;
	// }

	// case IPbus_1_4:
	// case IPbus_2_0:

	// switch ( mType )
	// {
	// case RMW_BITS:
	// return ( ( 2&0xF ) <<28 ) | ( ( 1&0xfff ) <<16 ) | ( ( aTransactionId&0xff ) <<8 ) | mType;
	// case R_A_I:
	// return ( ( 2&0xF ) <<28 ) | ( ( 2&0xfff ) <<16 ) | ( ( aTransactionId&0xff ) <<8 ) | mType;
	// case NI_WRITE:
	// case WRITE:
	// case B_O_T:
	// case NI_READ:
	// case READ:
	// case RMW_SUM:
	// return ( ( 2&0xF ) <<28 ) | ( ( aWordCount&0xfff ) <<16 ) | ( ( aTransactionId&0xff ) <<8 ) | mType;
	// }
	// }

	// return uint32_t ( -1 );
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }


	// template< eIPbusProtocolVersion IPbusProtocolVersion >
	// void IPbusPacketInfo::splitChunks ( const uint32_t& aMaxChunkSize , uint32_t& aTransactionId )
	// {
	// try
	// {
	// //calculate how the packets should be split and calculate the headers + base addresses
	// bool lSendSizeOk ( SendSize() <= aMaxChunkSize );
	// bool lReturnSizeOk ( ReturnSize() <= aMaxChunkSize );

	// if ( lSendSizeOk && lReturnSizeOk )
	// {
	// mChunks.push_back ( tChunks() );
	// tChunks& lChunk = mChunks.back();
	// lChunk.mTransactionHeader = calculateHeader< IPbusProtocolVersion > ( aTransactionId , mWordCount );
	// lChunk.mExpectedReplyHeader = calculateReplyHeader< IPbusProtocolVersion > ( aTransactionId , mWordCount );
	// lChunk.mBaseAddress = mBaseAddress;
	// lChunk.mMaxSendSize = SendSize();
	// lChunk.mReturnSize = ReturnSize();
	// lChunk.mReplyHeaders.resize ( mDeviceIDs.size() );
	// lChunk.mValMemPtr.resize ( mDeviceIDs.size() );

	// if ( ReturnPayloadSize() != 0 )
	// {
	// std::vector<uint32_t*>::iterator lChunkValMemIt = lChunk.mValMemPtr.begin();
	// std::vector< std::pair< uint32_t* , bool* > >::iterator lMasterValMemIt = mValMemPtr.begin();

	// for ( ; lChunkValMemIt != lChunk.mValMemPtr.end() && lMasterValMemIt != mValMemPtr.end() ; ++lChunkValMemIt , ++lMasterValMemIt )
	// {
	// *lChunkValMemIt = lMasterValMemIt->first;
	// }
	// }

	// lChunk.mSendPtr = & ( mPayload[0] );
	// aTransactionId++;
	// }
	// else if ( lReturnSizeOk )   // so SendSize is too big (can only happen on a write/ni-write)
	// {
	// uint32_t lHeaderSize = SendHeaderSize();
	// uint32_t lSendSize = SendPayloadSize();
	// uint32_t lBaseAddress = mBaseAddress;
	// uint32_t lOffset ( 0 );
	// uint32_t* lSendPtr = & ( mPayload[0] );

	// do
	// {
	// mChunks.push_back ( tChunks() );
	// tChunks& lChunk = mChunks.back();
	// lChunk.mBaseAddress = lBaseAddress;
	// lChunk.mReturnSize = ReturnSize();	//This condition can only happen on a write/ni-write so the ReturnSize for each chunk is just the header size
	// lChunk.mReplyHeaders.resize ( mDeviceIDs.size() );
	// lChunk.mValMemPtr.resize ( mDeviceIDs.size() );
	// // can only happen on a write/ni-write which must never return a payload
	// // if( ReturnPayloadSize() != 0 ){
	// // ...
	// // }
	// lChunk.mSendPtr = lSendPtr;

	// if ( lSendSize <= aMaxChunkSize )
	// {
	// lChunk.mTransactionHeader = calculateHeader< IPbusProtocolVersion > ( aTransactionId , lSendSize );
	// lChunk.mExpectedReplyHeader = calculateReplyHeader< IPbusProtocolVersion > ( aTransactionId , lSendSize );
	// lChunk.mMaxSendSize = lSendSize + lHeaderSize;
	// aTransactionId++;
	// break;
	// }
	// else
	// {
	// lChunk.mTransactionHeader = calculateHeader< IPbusProtocolVersion > ( aTransactionId , aMaxChunkSize );
	// lChunk.mExpectedReplyHeader = calculateReplyHeader< IPbusProtocolVersion > ( aTransactionId , aMaxChunkSize );
	// lChunk.mMaxSendSize = aMaxChunkSize + lHeaderSize;
	// aTransactionId++;
	// lSendSize -= aMaxChunkSize;

	// if ( mType==WRITE )
	// {
	// lBaseAddress += aMaxChunkSize;
	// }
	// }

	// lOffset += lChunk.mReturnSize - ReturnHeaderSize();
	// lSendPtr += lChunk.mMaxSendSize - lHeaderSize;
	// }
	// while ( true );
	// }
	// else     // so ReturnSize is too big (can only happen on a read/ni-read)
	// {
	// uint32_t lReturnHeaderSize = ReturnHeaderSize();
	// uint32_t lReturnSize = ReturnPayloadSize();
	// uint32_t lBaseAddress = mBaseAddress;
	// uint32_t lOffset ( 0 );
	// uint32_t* lSendPtr = & ( mPayload[0] );

	// do
	// {
	// mChunks.push_back ( tChunks() );
	// tChunks& lChunk = mChunks.back();
	// lChunk.mBaseAddress = lBaseAddress;
	// lChunk.mMaxSendSize = SendSize();	//This condition can only happen on a read/ni-read so the ReturnSize for each chunk is just the header size
	// lChunk.mReplyHeaders.resize ( mDeviceIDs.size() );
	// lChunk.mValMemPtr.resize ( mDeviceIDs.size() );
	// // can only happen on a read/ni-read which must return a payload
	// // if( ReturnPayloadSize() != 0 ){
	// std::vector<uint32_t*>::iterator lChunkValMemIt = lChunk.mValMemPtr.begin();
	// std::vector< std::pair< uint32_t* , bool* > >::iterator lMasterValMemIt = mValMemPtr.begin();

	// for ( ; lChunkValMemIt != lChunk.mValMemPtr.end() && lMasterValMemIt != mValMemPtr.end() ; ++lChunkValMemIt , ++lMasterValMemIt )
	// {
	// *lChunkValMemIt = 	lMasterValMemIt->first + lOffset;
	// }

	// // }
	// lChunk.mSendPtr = lSendPtr;

	// if ( lReturnSize <= aMaxChunkSize )
	// {
	// lChunk.mTransactionHeader = calculateHeader< IPbusProtocolVersion > ( aTransactionId , lReturnSize );
	// lChunk.mExpectedReplyHeader = calculateReplyHeader< IPbusProtocolVersion > ( aTransactionId , lReturnSize );
	// lChunk.mReturnSize = lReturnSize + lReturnHeaderSize;
	// aTransactionId++;
	// break;
	// }
	// else
	// {
	// lChunk.mTransactionHeader = calculateHeader< IPbusProtocolVersion > ( aTransactionId , aMaxChunkSize );
	// lChunk.mExpectedReplyHeader = calculateReplyHeader< IPbusProtocolVersion > ( aTransactionId , aMaxChunkSize );
	// lChunk.mReturnSize = aMaxChunkSize + lReturnHeaderSize;
	// aTransactionId++;
	// lReturnSize -= aMaxChunkSize;

	// if ( mType==READ )
	// {
	// lBaseAddress += aMaxChunkSize;
	// }
	// }

	// lOffset += lChunk.mReturnSize - lReturnHeaderSize;
	// // lSendPtr += lChunk.mMaxSendSize - SendHeaderSize();
	// }
	// while ( true );
	// }
	// }
	// catch ( const std::exception& aExc )
	// {
	// pantheios::log_EXCEPTION ( aExc );
	// throw uhal::exception ( aExc );
	// }
	// }

}

