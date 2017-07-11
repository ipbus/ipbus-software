/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.


      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

---------------------------------------------------------------------------
*/

#include "uhal/Buffers.hpp"


namespace uhal
{

  Buffers::Buffers ( const uint32_t& aMaxSendSize ) :
    mSendCounter ( 0 ),
    mReplyCounter ( 0 ),
    mSendBuffer ( aMaxSendSize , 0x00 )
  {
  }



  Buffers::~Buffers()
  {
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
    uint8_t* lStartPtr ( &mSendBuffer[0]+mSendCounter );
    memcpy ( lStartPtr , aPtr , aSize );
    mSendCounter += aSize;
    return lStartPtr;
  }


  void Buffers::receive ( uint8_t* aPtr , const uint32_t& aSize )
  {
    mReplyBuffer.push_back ( std::make_pair ( aPtr , aSize ) );
    mReplyCounter += aSize;
  }

  void Buffers::add ( const ValHeader& aValMem )
  {
    mValHeaders.push_back ( aValMem );
  }

  void Buffers::add ( const ValWord< uint32_t >& aValMem )
  {
    mUnsignedValWords.push_back ( aValMem );
  }

  // void Buffers::add ( const ValWord< int32_t >& aValMem )
  // {
  //
  // mSignedValWords.push_back ( aValMem );
  // }

  void Buffers::add ( const ValVector< uint32_t >& aValMem )
  {
    mUnsignedValVectors.push_back ( aValMem );
  }

  // void Buffers::add ( const ValVector< int32_t >& aValMem )
  // {
  //
  // mSignedValVectors.push_back ( aValMem );
  // }

  uint8_t* Buffers::getSendBuffer()
  {
    return &mSendBuffer[0];
  }

  std::deque< std::pair< uint8_t* , uint32_t > >& Buffers::getReplyBuffer()
  {
    return mReplyBuffer;
  }


  void Buffers::validate ( )
  {
    for ( std::deque< ValHeader >::iterator lIt = mValHeaders.begin() ; lIt != mValHeaders.end() ; ++lIt )
    {
      lIt->valid ( true );
    }

    for ( std::deque< ValWord< uint32_t > >::iterator lIt = mUnsignedValWords.begin() ; lIt != mUnsignedValWords.end() ; ++lIt )
    {
      lIt->valid ( true );
    }

    // for ( std::deque< ValWord< int32_t > >::iterator lIt = mSignedValWords.begin() ; lIt != mSignedValWords.end() ; ++lIt )
    // {
    // lIt->valid ( true );
    // }

    for ( std::deque< ValVector< uint32_t > >::iterator lIt = mUnsignedValVectors.begin() ; lIt != mUnsignedValVectors.end() ; ++lIt )
    {
      lIt->valid ( true );
    }

    // for ( std::deque< ValVector< int32_t > >::iterator lIt = mSignedValVectors.begin() ; lIt != mSignedValVectors.end() ; ++lIt )
    // {
    // lIt->valid ( true );
    // }
  }

  void Buffers::clear()
  {
    mSendCounter = 0 ;
    mReplyCounter = 0 ;
    mReplyBuffer.clear();
    mValHeaders.clear();
    mUnsignedValWords.clear();
    // mSignedValWords.clear();
    mUnsignedValVectors.clear();
    // mSignedValVectors.clear();
  }

}


