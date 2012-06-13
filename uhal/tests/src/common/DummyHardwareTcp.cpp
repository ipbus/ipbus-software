#include "uhal/IPbusPacketInfo.hpp"
#include <boost/asio.hpp>
#include <boost/lexical_cast.hpp>

using boost::asio::ip::tcp;
using namespace uhal;


class TCPdummyHardware
{
	public:

	TCPdummyHardware ( const uint16_t& aPort ) try :
			mIOservice(),
					   mAcceptor ( mIOservice , tcp::endpoint ( tcp::v4() , aPort ) ),
					   mSocket ( mIOservice )
		{
			mAcceptor.accept ( mSocket );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}

		~TCPdummyHardware() {}

		void run()
		{
			try
			{
				for ( ;; )
				{
					boost::system::error_code lError;
					uint32_t lTCPreceiveCounter = mSocket.read_some ( boost::asio::buffer ( mTCPreceiveBuffer, 500<<2 ) , lError );

					if ( lError == boost::asio::error::eof )
					{
						break; // Connection closed cleanly by peer.
					}

					uint32_t* lReceivePtr ( mTCPreceiveBuffer );
					uint32_t* lReceiveEnd ( lReceivePtr+ ( lTCPreceiveCounter>>2 ) );
					uint32_t* lReplyPtr ( mTCPreplyBuffer );

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
							log ( Error() , "Unable to parse send header " ,  Integer<hex,fixed> ( *lReceivePtr ) );
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

					boost::asio::write ( mSocket , boost::asio::buffer ( mTCPreplyBuffer , ( lReplyPtr-mTCPreplyBuffer ) <<2 ) );
				}
			}
			catch ( const std::exception& aExc )
			{
				log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
				throw uhal::exception ( aExc );
			}
		}

	private:
		boost::asio::io_service mIOservice;
		tcp::acceptor mAcceptor;
		tcp::socket mSocket;
		tcp::endpoint mSenderEndpoint;

		uint32_t mMemory[65536];

		uint32_t mTCPreceiveBuffer[500];
		uint32_t mTCPreplyBuffer[500];

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

		for ( ;; )
		{
			TCPdummyHardware lDummyHardware ( boost::lexical_cast<uint16_t> ( argv[1] ) );
			lDummyHardware.run();
			//if the connection is closed, open a new one 
		}
		
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw uhal::exception ( aExc );
	}

	return 0;
}
