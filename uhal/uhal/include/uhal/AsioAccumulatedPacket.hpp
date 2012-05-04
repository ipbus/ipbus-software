/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_AsioAccumulatedPacket_hpp_
#define _uhal_AsioAccumulatedPacket_hpp_

#include <deque>
#include <vector>
#include <boost/asio.hpp>

//! A struct for creating, filling and holding a buffer to be sent
struct tAccumulatedPacket
{
	//! A buffer sequence to be sent
	std::deque< boost::asio::const_buffer > mSendBuffers;
	//! A buffer sequence for receiving the replies
	std::deque< boost::asio::mutable_buffer > mReplyBuffers;
	//! The number of words to be sent in the packet
	std::size_t mCumulativeSendSize;
	//! The expected number of words to be returned by the packet
	std::size_t mCumulativeReturnSize;
	//! default constructor
	tAccumulatedPacket() :
		mCumulativeSendSize ( 0 ),
		mCumulativeReturnSize ( 0 )
	{}
};

//! Typedef a container for AccumulatedPackets
typedef std::deque< tAccumulatedPacket > tAccumulatedPackets;


#endif
