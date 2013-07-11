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

#include <boost/bind/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/asio/connect.hpp>
#include <boost/asio/write.hpp>
#include <boost/asio/read.hpp>

#include "uhal/IPbusInspector.hpp"

#include <sys/time.h>

namespace uhal
{

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  template < typename InnerProtocol >
  TCP< InnerProtocol >::TCP ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri ),
    mIOservice ( ),
#ifdef RUN_ASIO_MULTITHREADED
    mIOserviceWork ( mIOservice ),
#endif
    mSocket ( mIOservice ),
    mEndpoint ( boost::asio::ip::tcp::resolver ( mIOservice ).resolve ( boost::asio::ip::tcp::resolver::query ( aUri.mHostname , aUri.mPort ) ) ),
    mDeadlineTimer ( mIOservice ),
    mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
    mTcpDispatchQueue(),
    mTcpReplyQueue(),
    mPacketsInFlight( 0 ),
#endif
    mTcpDispatchBuffers ( NULL ),
    mTcpReplyBuffers ( NULL ),
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }



  template < typename InnerProtocol >
  TCP< InnerProtocol >::TCP ( const TCP< InnerProtocol >& aTCP ) :
    mIOservice ( ),
#ifdef RUN_ASIO_MULTITHREADED
    mIOserviceWork ( mIOservice ),
#endif
    mSocket ( mIOservice ),
    mEndpoint ( boost::asio::ip::tcp::resolver ( mIOservice ).resolve ( boost::asio::ip::tcp::resolver::query ( aTCP.mUri.mHostname , aTCP.mUri.mPort ) ) ),
    mDeadlineTimer ( mIOservice ),
    mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
    mTcpDispatchQueue(),
    mTcpReplyQueue(),
    mPacketsInFlight( 0 ),
#endif
    mTcpDispatchBuffers ( NULL ),
    mTcpReplyBuffers ( NULL ),
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol >
  TCP< InnerProtocol >& TCP< InnerProtocol >::operator= ( const TCP< InnerProtocol >& aTCP )
  {
    mEndpoint =  boost::asio::ip::tcp::resolver ( mIOservice ).resolve ( boost::asio::ip::tcp::resolver::query ( aTCP.mUri.mHostname , aTCP.mUri.mPort ) );
    mSocket.close();

    while ( mSocket.is_open() )
      {}

#ifdef RUN_ASIO_MULTITHREADED
    ClientInterface::returnBufferToPool ( mTcpDispatchQueue );
    ClientInterface::returnBufferToPool ( mTcpReplyQueue );
    mPacketsInFlight = 0;
#endif
    //    ClientInterface::returnBufferToPool ( mTcpDispatchBuffers );
    //    mTcpDispatchBuffers = NULL;
    //    ClientInterface::returnBufferToPool ( mTcpReplyBuffers );
    //    mTcpReplyBuffers = NULL;

    if ( mAsynchronousException )
    {
      delete mAsynchronousException;
      mAsynchronousException = NULL;
    }

    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }




  template < typename InnerProtocol >
  TCP< InnerProtocol >::~TCP()
  {
    try
    {
#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( mTcpMutex );
#endif
      //log( Warning() , "Closing Socket" );
      mSocket.close();

      while ( mSocket.is_open() )
        {}

      mIOservice.stop();
#ifdef RUN_ASIO_MULTITHREADED
      mDispatchThread.join();
      ClientInterface::returnBufferToPool ( mTcpDispatchQueue );
      ClientInterface::returnBufferToPool ( mTcpReplyQueue );
#endif
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
    }
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::implementDispatch ( Buffers* aBuffers )
  {
    //     std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
    if ( mAsynchronousException )
    {
      log ( Error() , "Rethrowing Asynchronous Exception from " , ThisLocation() );
      mAsynchronousException->ThrowAsDerivedType();
    }

    if ( ! mSocket.is_open() )
    {
      connect();
    }

    {
#ifdef RUN_ASIO_MULTITHREADED
      //       std::cout << this->getMaxNumberOfBuffers() << " : " << mTcpDispatchQueue.size() << " : " << mTcpReplyQueue.size() << std::endl;
      //block the next write operation if we are currently waiting for lMaxPacketsInFlight replies
      bool lContinue ( true );
      uint32_t lMaxPacketsInFlight ( this->getMaxNumberOfBuffers() );

      while ( lContinue )
      {
        boost::lock_guard<boost::mutex> lLock ( mTcpMutex );
        lContinue = ( mPacketsInFlight >= lMaxPacketsInFlight );
      }

      boost::lock_guard<boost::mutex> lLock ( mTcpMutex );

      if ( mTcpDispatchBuffers )
      {
        mTcpDispatchQueue.push_back ( aBuffers );
        //   std::cout << "extended mTcpDispatchQueue" << std::endl;
      }
      else
      {
        mTcpDispatchBuffers = aBuffers;
        write ( );
      }

#else
      mTcpDispatchBuffers = aBuffers;
      write ( );
#endif
    }
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::connect()
  {
    log ( Info() , "Attempting to create TCP connection to '" , mEndpoint->host_name() , "' port " , mEndpoint->service_name() , "." );
    mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::async_connect ( mSocket , mEndpoint , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

    do
    {
#ifndef RUN_ASIO_MULTITHREADED
      mIOservice.run_one();
#endif
    }
    while ( lErrorCode == boost::asio::error::would_block );

    if ( lErrorCode )
    {
      mSocket.close();

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        log ( Error() , "ASIO TCP connection timed out" );
      }
      else
      {
        log ( Error() , "ASIO reported an error: " , lErrorCode.message() );
      }

      throw exception::TcpConnectionFailure();
    }

    mSocket.set_option ( boost::asio::ip::tcp::no_delay ( true ) );
    boost::asio::socket_base::non_blocking_io lNonBlocking ( true );
    mSocket.io_control ( lNonBlocking );
    log ( Info() , "TCP connection succeeded" );
  }


  template < typename InnerProtocol >
  void TCP< InnerProtocol >::write ( )
  {
    /*    std::vector<uint32_t>::const_iterator lBegin ( reinterpret_cast<uint32_t*> ( aBuffers->getSendBuffer() ) );
        std::vector<uint32_t>::const_iterator lEnd = lBegin + ( aBuffers->sendCounter() >>2 );
        std::vector<uint32_t> lData;

        for ( ; lBegin!=lEnd ; ++lBegin )
        {
          std::cout << std::setfill ( '0' ) << std::hex << std::setw ( 8 ) <<  *lBegin << std::endl;
        }*/
    mAsioSendBuffer.clear();
    mAsioSendBuffer.push_back ( boost::asio::const_buffer ( mTcpDispatchBuffers->getSendBuffer() , mTcpDispatchBuffers->sendCounter() ) );
    log ( Debug() , "Sending " , Integer ( mTcpDispatchBuffers->sendCounter() ) , " bytes" );
    // log( Warning() , ThisLocation() );
    mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
#ifdef RUN_ASIO_MULTITHREADED
    boost::asio::async_write ( mSocket , mAsioSendBuffer , boost::bind ( &TCP< InnerProtocol >::write_callback, this, _1 ) );
    mPacketsInFlight++;
#else
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::async_write ( mSocket , mAsioSendBuffer , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

    do
    {
      mIOservice.run_one();
    }
    while ( lErrorCode == boost::asio::error::would_block );

    write_callback ( lErrorCode );
#endif
  }


  template < typename InnerProtocol >
  void TCP< InnerProtocol >::write_callback ( const boost::system::error_code& aErrorCode )
  {
#ifdef RUN_ASIO_MULTITHREADED
    {
      boost::lock_guard<boost::mutex> lLock ( mTcpMutex );

      if ( mTcpReplyBuffers )
      {
        mTcpReplyQueue.push_back ( mTcpDispatchBuffers );
        //   std::cout << "extended mTcpReplyQueue" << std::endl;
      }
      else
      {
        mTcpReplyBuffers = mTcpDispatchBuffers;
        read ( );
      }
    }

    if ( mTcpDispatchQueue.size() )
    {
      mTcpDispatchBuffers = mTcpDispatchQueue.front();
      mTcpDispatchQueue.pop_front();
      //std::cout << "reduced mTcpDispatchQueue" << std::endl;
      write();
    }
    else
    {
      mTcpDispatchBuffers = NULL;
    }

#else
    mTcpReplyBuffers = mTcpDispatchBuffers;
    mTcpDispatchBuffers = NULL;
    read ( );
#endif
  }


  template < typename InnerProtocol >
  void TCP< InnerProtocol >::read ( )
  {
    mAsioReplyBuffer.clear();
    mAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( & ( mReplyMemory.at ( 0 ) ) , mTcpReplyBuffers->replyCounter() ) );
    log ( Debug() , "Expecting " , Integer ( mTcpReplyBuffers->replyCounter() ) , " bytes in reply" );
    boost::asio::ip::tcp::endpoint lEndpoint;
    // log( Warning() , ThisLocation() );
    mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
#ifdef RUN_ASIO_MULTITHREADED
    boost::asio::async_read ( mSocket , mAsioReplyBuffer ,  boost::asio::transfer_at_least ( 4 ), boost::bind ( &TCP< InnerProtocol >::read_callback, this, _1 ) );
#else
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::async_read ( mSocket , mAsioReplyBuffer ,  boost::asio::transfer_at_least ( 4 ), boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

    do
    {
      mIOservice.run_one();
    }
    while ( lErrorCode == boost::asio::error::would_block );

    read_callback ( lErrorCode );
#endif
  }


  template < typename InnerProtocol >
  void TCP< InnerProtocol >::read_callback ( const boost::system::error_code& aErrorCode )
  {
    if ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) )
    {
      mSocket.close();

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        log ( Error() , "ASIO reported a Timeout in TCP callback" );
        mAsynchronousException = new exception::TcpTimeout();
        return;
      }

      log ( Error() , "ASIO reported an error: " , Quote ( aErrorCode.message() ) , ". Attempting validation to see if we can get any more info." );

      try
      {
        mAsynchronousException = ClientInterface::validate ( mTcpReplyBuffers ); //Control of the pointer has been passed back to the client interface
        mTcpReplyBuffers = NULL;
      }
      catch ( ... ) {}

      return;
    }

    /*
        TargetToHostInspector< 2 , 0 > lT2HInspector;
        std::vector<uint32_t>::const_iterator lBegin2 ( ( uint32_t* ) ( & mReplyMemory[0] ) );
        std::vector<uint32_t>::const_iterator lEnd2 ( ( uint32_t* ) ( & mReplyMemory[aBuffers->replyCounter() ] ) );
        lT2HInspector.analyze ( lBegin2 , lEnd2 );
    */
    //  std::cout << "Filling reply buffer : " << mTcpReplyBuffers << std::endl;
    std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( mTcpReplyBuffers->getReplyBuffer() );
    uint8_t* lReplyBuf ( & ( mReplyMemory.at ( 0 ) ) );

    for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
    {
      //      log ( Notice() , "Memory location = " , Integer ( ( std::size_t ) ( lIt->first ) , IntFmt<hex,fixed>() ), " Memory value = " , Integer ( * ( std::size_t* ) ( lIt->first ) , IntFmt<hex,fixed>() ), " & size = " , Integer ( lIt->second ) );
      memcpy ( lIt->first, lReplyBuf, lIt->second );
      lReplyBuf += lIt->second;
    }

    /*
    uint32_t lCounter(0);
    for ( std::vector< boost::asio::mutable_buffer >::iterator lIt = lAsioReplyBuffer.begin() ; lIt != lAsioReplyBuffer.end() ; ++lIt )
    {
    uint32_t s1 = boost::asio::buffer_size(*lIt)>>2;
    uint32_t* p1 = boost::asio::buffer_cast<uint32_t*>(*lIt);

    for( uint32_t i(0) ; i!= s1 ; ++i , ++p1 )
    {
    log ( Debug() , Integer ( lCounter++ ) , " : " , Integer ( *p1 , IntFmt<hex,fixed>() ) );
    }
    }
    */
    //log ( Debug() , ThisLocation() );

    try
    {
      mAsynchronousException = ClientInterface::validate ( mTcpReplyBuffers ); //Control of the pointer has been passed back to the client interface
    }
    catch ( exception::exception& aExc )
    {
      mAsynchronousException = new exception::ValidationError ();
      return;
    }

#ifdef RUN_ASIO_MULTITHREADED
    boost::lock_guard<boost::mutex> lLock ( mTcpMutex );

    if ( mTcpReplyQueue.size() )
    {
      mTcpReplyBuffers = mTcpReplyQueue.front();
      mTcpReplyQueue.pop_front();
      // std::cout << "reduced mTcpReplyQueue" << std::endl;
      read();
    }
    else
    {
      mTcpReplyBuffers = NULL;
    }
    mPacketsInFlight--;
#else
    mTcpReplyBuffers = NULL;
#endif
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::CheckDeadline()
  {
    // log( Warning() , ThisLocation() );
    // Check whether the deadline has passed. We compare the deadline against
    // the current time since a new asynchronous operation may have moved the
    // deadline before this actor had a chance to run.
    if ( mDeadlineTimer.expires_at() <= boost::asio::deadline_timer::traits_type::now() )
    {
      // SETTING THE EXCEPTION HERE CAN APPEAR AS A TIMEOUT WHEN NONE ACTUALLY EXISTS
      // The deadline has passed. The socket is closed so that any outstanding
      // asynchronous operations are cancelled.
      mSocket.close();
      // There is no longer an active deadline. The expiry is set to positive
      // infinity so that the actor takes no action until a new deadline is set.
      mDeadlineTimer.expires_at ( boost::posix_time::pos_infin );
      // log ( Error() , "ASIO deadline timer timed out" );
    }

    // Put the actor back to sleep.
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol >
  void TCP< InnerProtocol >::Flush( )
  {
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
    bool lContinue ( true );

    while ( lContinue )
    {
      if ( mAsynchronousException )
      {
        mAsynchronousException->ThrowAsDerivedType();
      }

#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( mTcpMutex );
#endif
      //log ( Warning() , "mTcpDispatchBuffers = " , Pointer ( mTcpDispatchBuffers ) , " mTcpReplyBuffers = " , Pointer ( mTcpReplyBuffers ) );
      lContinue = ( mTcpDispatchBuffers || mTcpReplyBuffers );
    }
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::dispatchExceptionHandler()
  {
    log ( Warning() , "Closing Socket" );
    mSocket.close();

    while ( mSocket.is_open() )
      {}

    if ( mAsynchronousException )
    {
      delete mAsynchronousException;
      mAsynchronousException = NULL;
    }

#ifdef RUN_ASIO_MULTITHREADED
    boost::lock_guard<boost::mutex> lLock ( mTcpMutex );
    ClientInterface::returnBufferToPool ( mTcpDispatchQueue );
    ClientInterface::returnBufferToPool ( mTcpReplyQueue );
#endif
    //    ClientInterface::returnBufferToPool ( mTcpDispatchBuffers );
    //    mTcpDispatchBuffers = NULL;
    //    ClientInterface::returnBufferToPool ( mTcpReplyBuffers );
    //    mTcpReplyBuffers = NULL;
    InnerProtocol::dispatchExceptionHandler();
  }

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}

