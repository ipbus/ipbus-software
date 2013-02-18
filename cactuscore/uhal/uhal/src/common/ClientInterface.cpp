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

#include "uhal/ClientInterface.hpp"

#include "uhal/Utilities.hpp"
#include <sstream>

namespace uhal
{

  ClientInterface::ClientInterface ( const std::string& aId, const URI& aUri ) :
    mId ( aId ),
    mUri ( aUri )
  {
    logging();
  }



  ClientInterface::ClientInterface ( )
  {
    logging();
  }



  ClientInterface::ClientInterface ( const ClientInterface& aClientInterface ) :
    mId ( aClientInterface.mId ),
    mUri ( aClientInterface.mUri )
  {
    logging();
  }


  ClientInterface& ClientInterface::operator= ( const ClientInterface& aClientInterface )
  {
    logging();
    mId  = aClientInterface.mId;
    mUri = aClientInterface.mUri;
    return *this;
  }


  ClientInterface::~ClientInterface()
  {
    logging();
  }

  const std::string& ClientInterface::id() const
  {
    logging();
    return mId;
  }


  // void ClientInterface::ping()
  // {
  // try
  // {
  // std::string lInstruction ( "ping -q -c 1 " + mUri.mHostname + " &> /dev/null" );
  // log ( Info() , "Pinging " ,  Quote ( mId ) , " with instruction : " , lInstruction );
  // //Cant use ICMP here because it requires raw socket (and hence superuser) access, so use system PING instead
  // int lPingStatus = system ( lInstruction.c_str() );

  // if ( WEXITSTATUS ( lPingStatus ) )
  // {
  // log ( Error() , "Pinging " , Quote ( mId ) , " at address " , Quote( mUri.mHostname ) , " returned exit status ", Integer ( WEXITSTATUS ( lPingStatus ) ) );
  // throw exception::// PingFailed();
  // }
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }


  std::string ClientInterface::uri() const
  {
    logging();
    std::stringstream lReturn;
    // url is always of the form "protocol://hostname:port"
    lReturn << mUri.mProtocol << "://" << mUri.mHostname << ":" << mUri.mPort;

    // there is sometimes a path
    if ( mUri.mPath != "" )
    {
      lReturn << "/" << mUri.mPath;
    }

    // there is sometimes a filename extension
    if ( mUri.mExtension != "" )
    {
      lReturn << "." << mUri.mExtension;
    }

    // there are sometimes arguments
    if ( mUri.mArguments.size() )
    {
      lReturn << "?";
      uhal::NameValuePairVectorType::const_iterator lIt = mUri.mArguments.begin();

      while ( true )
      {
        lReturn << lIt->first << "=" << lIt->second;

        if ( ++lIt == mUri.mArguments.end() )
        {
          break;
        }

        lReturn << "&";
      }
    }

    return lReturn.str();
  }


  void ClientInterface::dispatch ()
  {
    logging();
    this->predispatch( );
    mDispatchedBuffers.push_back ( & ( *mCurrentBuffers ) );
    this->implementDispatch( );
    NextFillingBuffer ();
  }



  bool ClientInterface::validate ( )
  {
    logging();
    //log ( Debug() , ThisLocation() );
    //check that the results are valid
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    //std::cout << mDispatchedBuffers.size() << std::endl;
    Buffers* lBuffer ( mDispatchedBuffers.front() );
    bool lRet = this->validate ( lBuffer->getSendBuffer() ,
                                 lBuffer->getSendBuffer() + lBuffer->sendCounter() ,
                                 lBuffer->getReplyBuffer().begin() ,
                                 lBuffer->getReplyBuffer().end() );

    //results are valid, so mark returned data as valid
    if ( lRet )
    {
      lBuffer->validate();
    }

    //////log ( Debug() , ThisLocation() );
    mDispatchedBuffers.pop_front();
    //////log ( Debug() , ThisLocation() );
    return lRet;
  }


  uint32_t ClientInterface::getPreambleSize()
  {
    return 0;
  }


  void ClientInterface::preamble()
  {
    logging();
    //////log ( Debug() , ThisLocation() );
  }

  void ClientInterface::predispatch( )
  {
    logging();
    //////log ( Debug() , ThisLocation() );
  }

  // ----------------------------------------------------------------------------------------------------------------------------------------------------------------

  void ClientInterface::CreateFillingBuffer ( )
  {
    logging();
    //////log ( Debug() , ThisLocation() );

    if ( mBuffers.size() )
    {
      return;
    }

    log ( Debug() , "Adding new Buffers to the memory pool" );
    Buffers lBuffers ( this->getMaxSendSize() );
    mBuffers.insert ( mBuffers.end() , 10 , lBuffers );
    mCurrentBuffers = mBuffers.begin();
    mCurrentBuffers->clear();
    this->preamble();
  }

  void ClientInterface::NextFillingBuffer ( )
  {
    logging();
    //////log ( Debug() , ThisLocation() );
    //if there are no existing buffers in the pool, create them
    CreateFillingBuffer ( );
    mCurrentBuffers++;

    if ( mCurrentBuffers == mBuffers.end() )
    {
      mCurrentBuffers = mBuffers.begin();
    }

    //we will wait if the buffer that we are expecting to fill is still waiting for dispatch and validation
    while ( true )
    {
      boost::lock_guard<boost::mutex> lLock ( mMutex );

      if ( & ( *mCurrentBuffers ) != mDispatchedBuffers.front() )
      {
        break;
      }
      else
      {
        log ( Warning() , "The fill queue has caught up with the dispatch queue - should implement a mechanism for expanding the memory pool to handle this case, but for now just wait for the dispatch queue to clear a bit" );
        sleep ( 1 );
      }
    }

    mCurrentBuffers->clear();
    this->preamble();
  }


  std::pair < ValHeader , _ValHeader_* > ClientInterface::CreateValHeader()
  {
    ValHeader lReply;
    return std::make_pair ( lReply , & ( * ( lReply.mMembers ) ) );
  }

  std::pair < ValWord<uint32_t> , _ValWord_<uint32_t>* > ClientInterface::CreateValWord ( const uint32_t& aValue , const uint32_t& aMask )
  {
    ValWord<uint32_t> lReply ( aValue , aMask );
    return std::make_pair ( lReply , & ( * ( lReply.mMembers ) ) );
  }

  std::pair < ValVector<uint32_t> , _ValVector_<uint32_t>* > ClientInterface::CreateValVector ( const uint32_t& aSize )
  {
    ValVector<uint32_t> lReply ( aSize );
    return std::make_pair ( lReply , & ( * ( lReply.mMembers ) ) );
  }


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValHeader ClientInterface::write ( const uint32_t& aAddr, const uint32_t& aSource )
  {
    logging();
    return implementWrite ( aAddr , aSource );
  }

  ValHeader ClientInterface::write ( const uint32_t& aAddr, const uint32_t& aSource, const uint32_t& aMask )
  {
    logging();
    uint32_t lShiftSize ( utilities::TrailingRightBits ( aMask ) );
    uint32_t lBitShiftedSource ( aSource << lShiftSize );

    if ( ( lBitShiftedSource >> lShiftSize ) != aSource )
    {
      log ( Error() , "Source data (" , Integer ( aSource , IntFmt<hex,fixed>() ) , ") has bits which would be shifted outside the register " );
      throw exception::BitsSetWhichAreForbiddenByBitMask();
    }

    uint32_t lOverlap ( lBitShiftedSource & ~aMask );

    if ( lOverlap )
    {
      log ( Error() , "Source data (" , Integer ( aSource , IntFmt<hex,fixed>() ) , ")"
            " has the following bits set outside the bounds allowed by the bit-mask ( ", Integer ( aSource , IntFmt<hex,fixed>() ) , ") : " ,
            Integer ( lOverlap , IntFmt<hex,fixed>() )
          );
      throw exception::BitsSetWhichAreForbiddenByBitMask();
    }

    return ( ValHeader ) ( implementRMWbits ( aAddr , ~aMask , lBitShiftedSource & aMask ) );
  }

  ValHeader ClientInterface::writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode& aMode )
  {
    logging();
    return implementWriteBlock ( aAddr, aSource, aMode );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr )
  {
    logging();
    return implementRead ( aAddr );
  }

  ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr, const uint32_t& aMask )
  {
    logging();
    return implementRead ( aAddr, aMask );
  }

  ValVector< uint32_t > ClientInterface::readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
  {
    logging();
    return implementReadBlock ( aAddr, aSize, aMode );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > ClientInterface::rmw_bits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm )
  {
    logging();
    return implementRMWbits ( aAddr , aANDterm , aORterm );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > ClientInterface::rmw_sum ( const uint32_t& aAddr , const int32_t& aAddend )
  {
    logging();
    return implementRMWsum ( aAddr , aAddend );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


}
