

namespace uhal
{
	template< typename T >
	uint8_t* Buffers::send ( const T* aPtr )
	{
		uint32_t lSize ( sizeof ( T ) );
		uint8_t* lStartPtr ( mSendBuffer+mSendCounter );
		memcpy ( lStartPtr , aPtr , lSize );
		mSendCounter += lSize;
		return lStartPtr;
	}

	template< typename T >
	uint8_t* Buffers::send ( const T& aRef )
	{
		uint32_t lSize ( sizeof ( T ) );
		uint8_t* lStartPtr ( mSendBuffer+mSendCounter );
		memcpy ( lStartPtr , &aRef , lSize );
		mSendCounter += lSize;
		return lStartPtr;
	}

	template< typename T >
	void Buffers::receive ( T* aPtr )
	{
		uint32_t lSize ( sizeof ( T ) );
		mReplyBuffer.push_back ( std::make_pair ( ( uint8_t* ) ( aPtr ) , lSize ) );
		mReplyCounter += lSize;
	}

	template< typename T >
	void Buffers::receive ( T& aRef )
	{
		uint32_t lSize ( sizeof ( T ) );
		mReplyBuffer.push_back ( std::make_pair ( ( uint8_t* ) ( &aRef ) , lSize ) );
		mReplyCounter += lSize;
	}

}


