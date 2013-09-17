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
// #include "uhal/logo.hpp"

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
    //     mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
    mDispatchQueue(),
    mReplyQueue(),
    mPacketsInFlight ( 0 ),
#endif
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
    //     mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
    mDispatchQueue(),
    mReplyQueue(),
    mPacketsInFlight ( 0 ),
#endif
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol >
  TCP< InnerProtocol >& TCP< InnerProtocol >::operator= ( const TCP< InnerProtocol >& aTCP )
  {
    mEndpoint =  boost::asio::ip::tcp::resolver ( mIOservice ).resolve ( boost::asio::ip::tcp::resolver::query ( aTCP.mUri.mHostname , aTCP.mUri.mPort ) );

    try
    {
      mSocket.close();

      while ( mSocket.is_open() )
        {}
    }
    catch ( const std::exception& aExc )
    {
      exception::ASIOTcpError lExc;
      log ( lExc , "Error closing socket" );
      log ( lExc , "ASIO reported " , Quote ( aExc.what() ) );
      throw lExc;
    }

#ifdef RUN_ASIO_MULTITHREADED
    ClientInterface::returnBufferToPool ( mDispatchQueue );
    ClientInterface::returnBufferToPool ( mReplyQueue );
    mPacketsInFlight = 0;
#endif

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
      mSocket.close();

      while ( mSocket.is_open() )
        {}

      mIOservice.stop();
#ifdef RUN_ASIO_MULTITHREADED
      mDispatchThread.join();
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
      ClientInterface::returnBufferToPool ( mDispatchQueue );
      ClientInterface::returnBufferToPool ( mReplyQueue );
#endif
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
    }
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::implementDispatch ( boost::shared_ptr< Buffers > aBuffers )
  {
    if ( mAsynchronousException )
    {
      log ( *mAsynchronousException , "Rethrowing Asynchronous Exception from " , ThisLocation() );
      mAsynchronousException->ThrowAsDerivedType();
    }

    if ( ! mSocket.is_open() )
    {
      connect();
    }

    {
#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );

      if ( mDispatchBuffers || mPacketsInFlight == this->getMaxNumberOfBuffers() )
      {
        mDispatchQueue.push_back ( aBuffers );
      }
      else
      {
        mDispatchBuffers = aBuffers;
        write ( );
      }

#else
      mDispatchBuffers = aBuffers;
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

      while ( mSocket.is_open() )
        {}

      exception::TcpConnectionFailure lExc;

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        log ( lExc , "ASIO TCP connection timed out" );
      }
      else
      {
        log ( lExc , "ASIO reported an error: " , lErrorCode.message() );
      }

      throw lExc;
    }

    mSocket.set_option ( boost::asio::ip::tcp::no_delay ( true ) );
//    boost::asio::socket_base::non_blocking_io lNonBlocking ( true );
//    mSocket.io_control ( lNonBlocking );
    log ( Info() , "TCP connection succeeded" );
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::write ( )
  {
    if ( !mDispatchBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mDispatchBuffers' was NULL" );
      return;
    }

    std::vector< boost::asio::const_buffer > lAsioSendBuffer;
    mSendByteCounter = htonl ( mDispatchBuffers->sendCounter() );
    lAsioSendBuffer.push_back ( boost::asio::const_buffer ( &mSendByteCounter , 4 ) );
    lAsioSendBuffer.push_back ( boost::asio::const_buffer ( mDispatchBuffers->getSendBuffer() , mDispatchBuffers->sendCounter() ) );
    log ( Debug() , "Sending " , Integer ( mDispatchBuffers->sendCounter() ) , " bytes" );
    mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
#ifdef RUN_ASIO_MULTITHREADED
    boost::asio::async_write ( mSocket , lAsioSendBuffer , boost::bind ( &TCP< InnerProtocol >::write_callback, this, _1 ) );
    mPacketsInFlight++;
#else
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::async_write ( mSocket , lAsioSendBuffer , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

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
    boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );

    if ( mAsynchronousException )
    {
      return;
    }

    if ( mReplyBuffers )
    {
      mReplyQueue.push_back ( mDispatchBuffers );
    }
    else
    {
      mReplyBuffers = mDispatchBuffers;
      read ( );
    }

    if ( mDispatchQueue.size() && mPacketsInFlight != this->getMaxNumberOfBuffers() )
    {
      mDispatchBuffers = mDispatchQueue.front();
      mDispatchQueue.pop_front();
      write();
    }
    else
    {
      mDispatchBuffers.reset();
    }

#else
    mReplyBuffers = mDispatchBuffers;
    mDispatchBuffers.reset();
    read ( );
#endif
  }


  template < typename InnerProtocol >
  void TCP< InnerProtocol >::read ( )
  {
    if ( !mReplyBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mReplyBuffers' was NULL" );
      return;
    }

    //     std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( mReplyBuffers->getReplyBuffer() );
    //     std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
    //     lAsioReplyBuffer.reserve ( lReplyBuffers.size() +1 );
    //     for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
    //     {
    //       lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( lIt->first , lIt->second ) );
    //     }
    //     lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( mReplyBuffers->getSpareSpace() , Buffers::mSpareSpaceSize ) );
    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
    lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( &mReplyByteCounter , 4 ) );
    log ( Debug() , "Getting reply byte counter" );
    boost::asio::ip::tcp::endpoint lEndpoint;
    mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
#ifdef RUN_ASIO_MULTITHREADED
    boost::asio::async_read ( mSocket , lAsioReplyBuffer ,  boost::asio::transfer_exactly ( 4 ), boost::bind ( &TCP< InnerProtocol >::read_callback, this, _1 ) );
#else
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::async_read ( mSocket , lAsioReplyBuffer ,  boost::asio::transfer_exactly ( 4 ), boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

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
    if ( mAsynchronousException )
    {
      return;
    }

    if ( !mReplyBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mReplyBuffers' was NULL" );
      return;
    }

    if ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) )
    {
      try
      {
        mSocket.close();

        while ( mSocket.is_open() )
          {}
      }
      catch ( const std::exception& aExc )
      {
        exception::ASIOTcpError* lExc = new exception::ASIOTcpError();
        log ( *lExc , "Error closing socket" );
        log ( *lExc , "ASIO reported " , Quote ( aExc.what() ) );
        mAsynchronousException = lExc;
        return;
      }

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        exception::TcpTimeout* lExc = new exception::TcpTimeout();
        log ( *lExc , "ASIO reported an error: " , Quote ( aErrorCode.message() ) );
        log ( *lExc , "ASIO reported a Timeout in TCP callback" );
        mAsynchronousException = lExc;
        return;
      }

      exception::ASIOTcpError* lExc = new exception::ASIOTcpError();
      log ( *lExc , "ASIO reported an error: " , Quote ( aErrorCode.message() ) );
      mAsynchronousException = lExc;
      return;
    }

    mReplyByteCounter = ntohl ( mReplyByteCounter );
    log ( Debug() , "Byte Counter says " , Integer ( mReplyByteCounter ) , " bytes are coming" );
    log ( Debug() , "Expecting " , Integer ( mReplyBuffers->replyCounter() ) , " bytes in reply" );
    std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( mReplyBuffers->getReplyBuffer() );
    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
    lAsioReplyBuffer.reserve ( lReplyBuffers.size() +1 );

    for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
    {
      lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( lIt->first , lIt->second ) );
    }

    //     lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( mReplyBuffers->getSpareSpace() , Buffers::mSpareSpaceSize ) );
    //     mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    boost::asio::read ( mSocket , lAsioReplyBuffer ,  boost::asio::transfer_exactly ( mReplyByteCounter ), lErrorCode );
    //     do
    //     {
    //       std::cout << "." << std::flush;
    // #ifndef RUN_ASIO_MULTITHREADED
    //       mIOservice.run_one();
    // #endif
    //     }
    //     while ( lErrorCode == boost::asio::error::would_block );

    if ( lErrorCode && ( lErrorCode != boost::asio::error::eof ) )
    {
      mSocket.close();

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        exception::TcpTimeout* lExc = new exception::TcpTimeout();
        log ( *lExc , "ASIO reported an error: " , Quote ( lErrorCode.message() ) );
        log ( *lExc , "ASIO reported a Timeout in TCP callback" );
        mAsynchronousException = lExc;
        return;
      }

      exception::ASIOTcpError* lExc = new exception::ASIOTcpError();
      log ( *lExc , "ASIO reported an error: " , Quote ( lErrorCode.message() ) );
      mAsynchronousException = lExc;
      return;
    }

    try
    {
      mAsynchronousException = ClientInterface::validate ( mReplyBuffers ); //Control of the pointer has been passed back to the client interface
    }
    catch ( exception::exception& aExc )
    {
      mAsynchronousException = new exception::ValidationError ();
    }

    if ( mAsynchronousException )
    {
      return;
    }

#ifdef RUN_ASIO_MULTITHREADED
    boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );

    if ( mReplyQueue.size() )
    {
      mReplyBuffers = mReplyQueue.front();
      mReplyQueue.pop_front();
      read();
    }
    else
    {
      mReplyBuffers.reset();
    }

    mPacketsInFlight--;

    if ( !mDispatchBuffers && mDispatchQueue.size() && mPacketsInFlight != this->getMaxNumberOfBuffers() )
    {
      mDispatchBuffers = mDispatchQueue.front();
      mDispatchQueue.pop_front();
      write();
    }

#else
    mReplyBuffers.reset();
#endif
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::CheckDeadline()
  {
    // Check whether the deadline has passed. We compare the deadline against the current time since a new asynchronous operation may have moved the deadline before this actor had a chance to run.
    if ( mDeadlineTimer.expires_at() <= boost::asio::deadline_timer::traits_type::now() )
    {
      // SETTING THE EXCEPTION HERE CAN APPEAR AS A TIMEOUT WHEN NONE ACTUALLY EXISTS
      // The deadline has passed. The socket is closed so that any outstanding asynchronous operations are cancelled.
      mSocket.close();
      // There is no longer an active deadline. The expiry is set to positive infinity so that the actor takes no action until a new deadline is set.
      mDeadlineTimer.expires_at ( boost::posix_time::pos_infin );
    }

    // Put the actor back to sleep.
    mDeadlineTimer.async_wait ( boost::bind ( &TCP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol >
  void TCP< InnerProtocol >::Flush( )
  {
    bool lContinue ( true );

    while ( lContinue )
    {
      if ( mAsynchronousException )
      {
        mAsynchronousException->ThrowAsDerivedType();
      }

#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
#endif
      lContinue = ( mDispatchBuffers || mReplyBuffers );
      //
    }
  }



  template < typename InnerProtocol >
  void TCP< InnerProtocol >::dispatchExceptionHandler()
  {
    log ( Warning() , "Closing Socket" );

    if ( mSocket.is_open() )
    {
      try
      {
        mSocket.close();

        while ( mSocket.is_open() )
          {}
      }
      catch ( ... )
        {}
    }

    if ( mAsynchronousException )
    {
      delete mAsynchronousException;
      mAsynchronousException = NULL;
    }

    {
#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
      ClientInterface::returnBufferToPool ( mDispatchQueue );
      ClientInterface::returnBufferToPool ( mReplyQueue );
      mPacketsInFlight = 0;
#endif
      ClientInterface::returnBufferToPool ( mDispatchBuffers );
      mDispatchBuffers.reset();
      ClientInterface::returnBufferToPool ( mReplyBuffers );
      mReplyBuffers.reset();
      mSendByteCounter = 0;
      mReplyByteCounter = 0;
    }

    InnerProtocol::dispatchExceptionHandler();
  }

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}

