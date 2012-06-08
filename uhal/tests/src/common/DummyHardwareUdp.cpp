#include "uhal/IPbusPacketInfo.hpp"
#include <boost/asio.hpp>
#include <boost/lexical_cast.hpp>

using boost::asio::ip::udp;
using namespace uhal;


class UDPdummyHardware
{
	public:
		UDPdummyHardware ( const uint16_t& aPort ) :
			mIOservice(),
			mSocket ( mIOservice , udp::endpoint ( udp::v4(), aPort ) )
		{}

		~UDPdummyHardware() {}

		void run()
		{
			for ( ;; )
			{
				uint32_t lUDPreceiveCounter = mSocket.receive_from ( boost::asio::buffer ( mUDPreceiveBuffer, 500<<2 ) , mSenderEndpoint );
				uint32_t* lReceivePtr ( mUDPreceiveBuffer );
				uint32_t* lReceiveEnd ( lReceivePtr+ ( lUDPreceiveCounter>>2 ) );
				uint32_t* lReplyPtr ( mUDPreplyBuffer );

				do
				{
					if ( ! IPbusHeaderHelper< IPbus_1_3 >::extract (
								*lReceivePtr ,
								mType ,
								mWordCounter ,
								mTransactionId ,
								mResponseGood )
					   )
					{
						pantheios::log_ERROR ( "Unable to parse send header " , pantheios::integer ( *lReceivePtr , pantheios::fmt::fullHex | 10 ) );
						return;
					}

					lReceivePtr++;

					switch ( mType )
					{
						case B_O_T:
							*lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
							lReplyPtr++;
							break;
						case R_A_I:
							*lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 2 , mTransactionId ) | 0x4;
							lReplyPtr+=3;
							break;
						case NI_READ:
							mAddress = *lReceivePtr;
							lReceivePtr++;
							*lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , mWordCounter , mTransactionId ) | 0x4;
							lReplyPtr++;

							for ( ; mWordCounter!=0 ; --mWordCounter )
							{
								*lReplyPtr = mMemory[ mAddress & 0xFFFF ];
								lReplyPtr++;
							}

							break;
						case READ:
							mAddress = *lReceivePtr;
							lReceivePtr++;
							*lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , mWordCounter , mTransactionId ) | 0x4;
							lReplyPtr++;

							for ( ; mWordCounter!=0 ; --mWordCounter )
							{
								*lReplyPtr = mMemory[ mAddress++ & 0xFFFF ];
								lReplyPtr++;
							}

							break;
						case NI_WRITE:
							mAddress = *lReceivePtr;
							lReceivePtr++;

							for ( ; mWordCounter!=0 ; --mWordCounter )
							{
								mMemory[ mAddress & 0xFFFF ] = *lReceivePtr;
								lReceivePtr++;
							}

							*lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
							lReplyPtr++;
							break;
						case WRITE:
							mAddress = *lReceivePtr;
							lReceivePtr++;

							for ( ; mWordCounter!=0 ; --mWordCounter )
							{
								mMemory[ mAddress++ & 0xFFFF ] = *lReceivePtr;
								lReceivePtr++;
							}

							*lReplyPtr = IPbusHeaderHelper< IPbus_1_3 >::calculate ( mType , 0 , mTransactionId ) | 0x4;
							lReplyPtr++;
							break;
						case RMW_SUM:
							mAddress = *lReceivePtr;
							lReceivePtr++;
							mMemory[ mAddress & 0xFFFF ] += ( int32_t ) ( *lReceivePtr );
							lReceivePtr++;
							*lReplyPtr = mMemory[ mAddress & 0xFFFF ];
							lReplyPtr++;
							break;
						case RMW_BITS:
							mAddress = *lReceivePtr;
							lReceivePtr++;
							mMemory[ mAddress & 0xFFFF ] &= ( int32_t ) ( *lReceivePtr );
							lReceivePtr++;
							mMemory[ mAddress & 0xFFFF ] |= ( int32_t ) ( *lReceivePtr );
							lReceivePtr++;
							*lReplyPtr = mMemory[ mAddress & 0xFFFF ];
							lReplyPtr++;
							break;
					}
				}
				while ( lReceivePtr!=lReceiveEnd );

				mSocket.send_to ( boost::asio::buffer ( mUDPreplyBuffer , ( lReplyPtr-mUDPreplyBuffer ) <<2 ) , mSenderEndpoint );
			}
		}

	private:
		boost::asio::io_service mIOservice;
		udp::socket mSocket;
		udp::endpoint mSenderEndpoint;

		uint32_t mMemory[65536];

		uint32_t mUDPreceiveBuffer[500];
		uint32_t mUDPreplyBuffer[500];

		eIPbusTransactionType mType;
		uint32_t mWordCounter;
		uint32_t mTransactionId;
		uint8_t mResponseGood;

		uint32_t mAddress;

};



int main ( int argc, char* argv[] )
{
	try
	{
		if ( argc != 2 )
		{
			std::cerr << "Usage: " << argv[0] << " <port>\n";
			return 1;
		}

		UDPdummyHardware lDummyHardware ( boost::lexical_cast<uint16_t> ( argv[1] ) );
		lDummyHardware.run();
	}
	catch ( std::exception& e )
	{
		std::cerr << "Exception: " << e.what() << "\n";
	}

	return 0;
}
