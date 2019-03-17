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


#include <sstream>

#include <boost/shared_ptr.hpp>
#include <boost/thread/lock_guard.hpp>

#include "uhal/Buffers.hpp"
#include "uhal/log/LogLevels.hpp"                              // for BaseLo...
#include "uhal/log/log_inserters.integer.hpp"                  // for Integer
#include "uhal/log/log.hpp"
#include "uhal/utilities/bits.hpp"


namespace uhal
{

  ClientInterface::ClientInterface ( const std::string& aId, const URI& aUri,  const boost::posix_time::time_duration& aTimeoutPeriod ) :
    mBuffers(),
#ifdef NO_PREEMPTIVE_DISPATCH
    mNoPreemptiveDispatchBuffers(),
#endif
    mId ( aId ),
    mTimeoutPeriod ( aTimeoutPeriod ),
    mUri ( aUri )
  {
  }


  ClientInterface::ClientInterface ( ) :
    mBuffers(),
#ifdef NO_PREEMPTIVE_DISPATCH
    mNoPreemptiveDispatchBuffers(),
#endif
    mId ( ),
    mTimeoutPeriod ( boost::posix_time::pos_infin ),
    mUri ( )
  {
  }


  ClientInterface::ClientInterface ( const ClientInterface& aClientInterface ) :
    mBuffers(),
#ifdef NO_PREEMPTIVE_DISPATCH
    mNoPreemptiveDispatchBuffers(),
#endif
    mId ( aClientInterface.mId ),
    mTimeoutPeriod ( aClientInterface.mTimeoutPeriod ),
    mUri ( aClientInterface.mUri )
  {
  }


  ClientInterface& ClientInterface::operator= ( const ClientInterface& aClientInterface )
  {
    deleteBuffers();
    mId  = aClientInterface.mId;
    mUri = aClientInterface.mUri;
    mTimeoutPeriod = aClientInterface.mTimeoutPeriod;
    return *this;
  }


  ClientInterface::~ClientInterface()
  {
    deleteBuffers();
  }

  const std::string& ClientInterface::id() const
  {
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
    std::stringstream lReturn;
    // url is always of the form "protocol://hostname:port"
    lReturn << mUri.mProtocol << "://" << mUri.mHostname;
    if ( !mUri.mPort.empty() )
      lReturn << ":" << mUri.mPort;

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
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );

    try
    {
#ifdef NO_PREEMPTIVE_DISPATCH
      log ( Info() , "mNoPreemptiveDispatchBuffers.size() = " , Integer ( mNoPreemptiveDispatchBuffers.size() ) );

      for ( std::deque < boost::shared_ptr< Buffers > >::iterator lIt = mNoPreemptiveDispatchBuffers.begin(); lIt != mNoPreemptiveDispatchBuffers.end(); ++lIt )
      {
        this->predispatch ( *lIt );
        this->implementDispatch ( *lIt ); //responsibility for *lIt passed to the implementDispatch function
        lIt->reset();
      }

      {
        boost::lock_guard<boost::mutex> lLock ( mBufferMutex );
        mNoPreemptiveDispatchBuffers.clear();
      }

      this->Flush();
#endif

      if ( mCurrentBuffers )
      {
        this->predispatch ( mCurrentBuffers );
        this->implementDispatch ( mCurrentBuffers ); //responsibility for mCurrentBuffers passed to the implementDispatch function
        mCurrentBuffers.reset();
        this->Flush();
      }
    }
    catch ( ... )
    {
      this->dispatchExceptionHandler();
      throw;
    }
  }


  void ClientInterface::Flush ()
  {}


  exception::exception* ClientInterface::validate ( boost::shared_ptr< Buffers > aBuffers )
  {
    exception::exception* lRet = this->validate ( aBuffers->getSendBuffer() ,
                                 aBuffers->getSendBuffer() + aBuffers->sendCounter() ,
                                 aBuffers->getReplyBuffer().begin() ,
                                 aBuffers->getReplyBuffer().end() );

    //results are valid, so mark returned data as valid
    if ( !lRet )
    {
      aBuffers->validate ();
    }

    returnBufferToPool ( aBuffers );
    return lRet;
  }


  uint32_t ClientInterface::getPreambleSize()
  {
    return 0;
  }


  void ClientInterface::preamble ( boost::shared_ptr< Buffers > aBuffers )
  {}


  void ClientInterface::predispatch ( boost::shared_ptr< Buffers > aBuffers )
  {}



  void ClientInterface::returnBufferToPool ( boost::shared_ptr< Buffers >& aBuffers )
  {
    boost::lock_guard<boost::mutex> lLock ( mBufferMutex );

    if ( aBuffers )
    {
      mBuffers.push_back ( aBuffers );
      aBuffers.reset();
    }
  }


  void ClientInterface::returnBufferToPool ( std::deque< boost::shared_ptr< Buffers > >& aBuffers )
  {
    boost::lock_guard<boost::mutex> lLock ( mBufferMutex );

    for ( std::deque < boost::shared_ptr< Buffers > >::iterator lIt = aBuffers.begin(); lIt != aBuffers.end(); ++lIt )
    {
      if ( *lIt )
      {
        mBuffers.push_back ( *lIt );
      }
    }

    aBuffers.clear();
  }


  void ClientInterface::returnBufferToPool ( std::vector< boost::shared_ptr<Buffers> >& aBuffers )
  {
    boost::lock_guard<boost::mutex> lLock ( mBufferMutex );

    for ( std::vector < boost::shared_ptr< Buffers > >::iterator lIt = aBuffers.begin(); lIt != aBuffers.end(); ++lIt )
    {
      if ( *lIt )
      {
        mBuffers.push_back ( *lIt );
      }
    }

    aBuffers.clear();
  }


  void ClientInterface::returnBufferToPool ( std::deque< std::vector< boost::shared_ptr<Buffers> > >& aBuffers )
  {
    boost::lock_guard<boost::mutex> lLock ( mBufferMutex );

    for ( std::deque < std::vector < boost::shared_ptr< Buffers > > >::iterator lIt1 = aBuffers.begin(); lIt1 != aBuffers.end(); ++lIt1 )
    {
      for ( std::vector< boost::shared_ptr<Buffers> >::iterator lIt2 = lIt1->begin(); lIt2 != lIt1->end(); ++lIt2 )
      {
        if ( *lIt2 )
        {
          mBuffers.push_back ( *lIt2 );
        }
      }
    }

    aBuffers.clear();
  }


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  boost::shared_ptr< Buffers > ClientInterface::checkBufferSpace ( const uint32_t& aRequestedSendSize , const uint32_t& aRequestedReplySize , uint32_t& aAvailableSendSize , uint32_t& aAvailableReplySize )
  {
    log ( Debug() , "Checking buffer space" );
    //if there are no existing buffers in the pool, create them
    updateCurrentBuffers();
    uint32_t lSendBufferFreeSpace ( this->getMaxSendSize() - mCurrentBuffers->sendCounter() );
    uint32_t lReplyBufferFreeSpace ( this->getMaxReplySize() - mCurrentBuffers->replyCounter() );

    if ( ( aRequestedSendSize <= lSendBufferFreeSpace ) && ( aRequestedReplySize <= lReplyBufferFreeSpace ) )
    {
      aAvailableSendSize = aRequestedSendSize;
      aAvailableReplySize = aRequestedReplySize;
      return mCurrentBuffers;
    }

    if ( ( lSendBufferFreeSpace > 16 ) && ( lReplyBufferFreeSpace > 16 ) )
    {
      aAvailableSendSize = lSendBufferFreeSpace;
      aAvailableReplySize = lReplyBufferFreeSpace;
      return mCurrentBuffers;
    }

#ifdef NO_PREEMPTIVE_DISPATCH
    mNoPreemptiveDispatchBuffers.push_back ( mCurrentBuffers );
    mCurrentBuffers.reset();
#else
    log ( Debug() , "Triggering automated dispatch" );

    try
    {
      this->predispatch ( mCurrentBuffers );
      this->implementDispatch ( mCurrentBuffers );
      mCurrentBuffers.reset();
    }
    catch ( ... )
    {
      this->dispatchExceptionHandler();
      throw;
    }

#endif
    updateCurrentBuffers();
    lSendBufferFreeSpace = this->getMaxSendSize() - mCurrentBuffers->sendCounter();
    lReplyBufferFreeSpace = this->getMaxReplySize() - mCurrentBuffers->replyCounter();

    if ( ( aRequestedSendSize <= lSendBufferFreeSpace ) && ( aRequestedReplySize <= lReplyBufferFreeSpace ) )
    {
      aAvailableSendSize = aRequestedSendSize;
      aAvailableReplySize = aRequestedReplySize;
      return mCurrentBuffers;
    }

    aAvailableSendSize = lSendBufferFreeSpace;
    aAvailableReplySize = lReplyBufferFreeSpace;
    return mCurrentBuffers;
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  void ClientInterface::updateCurrentBuffers()
  {
    if ( ! mCurrentBuffers )
    {
      {
        boost::lock_guard<boost::mutex> lLock ( mBufferMutex );

        if ( mBuffers.size() == 0 )
        {
          for ( uint32_t i=0; i!=10; ++i )
          {
            mBuffers.push_back ( boost::shared_ptr< Buffers > ( new Buffers ( this->getMaxSendSize() ) ) );
          }
        }

        mCurrentBuffers = mBuffers.front();
        mBuffers.pop_front();
        mCurrentBuffers->clear();
      }
      this->preamble ( mCurrentBuffers );
    }
  }


  void ClientInterface::deleteBuffers()
  {
    boost::lock_guard<boost::mutex> lLock ( mBufferMutex );
    mBuffers.clear();

#ifdef NO_PREEMPTIVE_DISPATCH
    mNoPreemptiveDispatchBuffers.clear();
#endif

    if ( mCurrentBuffers )
    {
      mCurrentBuffers.reset();
    }

  }


  void ClientInterface::dispatchExceptionHandler()
  {
    deleteBuffers();
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
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    return implementWrite ( aAddr , aSource );
  }


  ValHeader ClientInterface::write ( const uint32_t& aAddr, const uint32_t& aSource, const uint32_t& aMask )
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    uint32_t lShiftSize ( utilities::TrailingRightBits ( aMask ) );
    uint32_t lBitShiftedSource ( aSource << lShiftSize );

    if ( ( lBitShiftedSource >> lShiftSize ) != aSource )
    {
      exception::BitsSetWhichAreForbiddenByBitMask lExc;
      log ( lExc , "Source data (" , Integer ( aSource , IntFmt<hex,fixed>() ) , ") has bits which would be shifted outside the register " );
      throw lExc;
    }

    uint32_t lOverlap ( lBitShiftedSource & ~aMask );

    if ( lOverlap )
    {
      exception::BitsSetWhichAreForbiddenByBitMask lExc;
      log ( lExc , "Source data (" , Integer ( aSource , IntFmt<hex,fixed>() ) , ")"
            " has the following bits set outside the bounds allowed by the bit-mask ( ", Integer ( aSource , IntFmt<hex,fixed>() ) , ") : " ,
            Integer ( lOverlap , IntFmt<hex,fixed>() )
          );
      throw lExc;
    }

    return ( ValHeader ) ( implementRMWbits ( aAddr , ~aMask , lBitShiftedSource & aMask ) );
  }


  ValHeader ClientInterface::writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode& aMode )
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    return implementWriteBlock ( aAddr, aSource, aMode );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr )
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    return implementRead ( aAddr );
  }


  ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr, const uint32_t& aMask )
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    return implementRead ( aAddr, aMask );
  }


  ValVector< uint32_t > ClientInterface::readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    return implementReadBlock ( aAddr, aSize, aMode );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > ClientInterface::rmw_bits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm )
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    return implementRMWbits ( aAddr , aANDterm , aORterm );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > ClientInterface::rmw_sum ( const uint32_t& aAddr , const int32_t& aAddend )
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    return implementRMWsum ( aAddr , aAddend );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  void ClientInterface::setTimeoutPeriod ( const uint32_t& aTimeoutPeriod )
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );

    if ( aTimeoutPeriod == 0 )
    {
      mTimeoutPeriod = boost::posix_time::pos_infin;
    }
    else
    {
      mTimeoutPeriod = boost::posix_time::milliseconds ( aTimeoutPeriod );
    }
  }


  uint64_t ClientInterface::getTimeoutPeriod()
  {
    boost::lock_guard<boost::mutex> lLock ( mUserSideMutex );
    return mTimeoutPeriod.total_milliseconds();
  }


  const boost::posix_time::time_duration& ClientInterface::getBoostTimeoutPeriod()
  {
    return mTimeoutPeriod;
  }

}
