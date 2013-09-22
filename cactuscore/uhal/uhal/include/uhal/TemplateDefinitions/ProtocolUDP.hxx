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
  UDP< InnerProtocol >::UDP ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri ),
    mIOservice ( ),
#ifdef RUN_ASIO_MULTITHREADED
    mIOserviceWork ( mIOservice ),
#endif
    mSocket ( mIOservice , boost::asio::ip::udp::endpoint ( boost::asio::ip::udp::v4(), 0 ) ),
    mEndpoint ( *boost::asio::ip::udp::resolver ( mIOservice ).resolve ( boost::asio::ip::udp::resolver::query ( boost::asio::ip::udp::v4() , aUri.mHostname , aUri.mPort ) ) ),
    mDeadlineTimer ( mIOservice ),
    //     mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
    mDispatchQueue(),
    mReplyQueue(),
    mPacketsInFlight ( 0 ),
#endif
    //    mDispatchBuffers ( NULL ),
    //    mReplyBuffers ( NULL ),
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &UDP::CheckDeadline, this ) );
  }



  template < typename InnerProtocol >
  UDP< InnerProtocol >::UDP ( const UDP< InnerProtocol >& aUDP ) :
    mIOservice ( ),
#ifdef RUN_ASIO_MULTITHREADED
    mIOserviceWork ( mIOservice ),
#endif
    mSocket ( mIOservice , boost::asio::ip::udp::endpoint ( boost::asio::ip::udp::v4(), 0 ) ),
    mEndpoint ( *boost::asio::ip::udp::resolver ( mIOservice ).resolve ( boost::asio::ip::udp::resolver::query ( boost::asio::ip::udp::v4() , aUDP.mUri.mHostname , aUDP.mUri.mPort ) ) ),
    mDeadlineTimer ( mIOservice ),
    //     mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
    mDispatchQueue(),
    mReplyQueue(),
    mPacketsInFlight ( 0 ),
#endif
    //    mDispatchBuffers ( NULL ),
    //    mReplyBuffers ( NULL ),
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &UDP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol >
  UDP< InnerProtocol >& UDP< InnerProtocol >::operator= ( const UDP< InnerProtocol >& aUDP )
  {
    mEndpoint =  *boost::asio::ip::udp::resolver ( mIOservice ).resolve ( boost::asio::ip::udp::resolver::query ( boost::asio::ip::udp::v4() , aUDP.mUri.mHostname , aUDP.mUri.mPort ) );
    mSocket.close();

    while ( mSocket.is_open() )
      {}

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

    mDeadlineTimer.async_wait ( boost::bind ( &UDP::CheckDeadline, this ) );
  }




  template < typename InnerProtocol >
  UDP< InnerProtocol >::~UDP()
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
  void UDP< InnerProtocol >::implementDispatch ( boost::shared_ptr< Buffers > aBuffers )
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
        //   std::cout << "extended mDispatchQueue" << std::endl;
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
  void UDP< InnerProtocol >::connect()
  {
    log ( Info() , "Creating new UDP socket, as it appears to have been closed..." );
    //mSocket = boost::asio::ip::udp::socket ( mIOservice , boost::asio::ip::udp::endpoint ( boost::asio::ip::udp::v4(), 0 ) );
    mSocket.open ( boost::asio::ip::udp::v4() );
    //    boost::asio::socket_base::non_blocking_io lNonBlocking ( true );
    //    mSocket.io_control ( lNonBlocking );
    log ( Info() , "UDP socket created successfully." );
  }


  template < typename InnerProtocol >
  void UDP< InnerProtocol >::write ( )
  {
    if ( !mDispatchBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mDispatchBuffers' was NULL" );
      return;
    }

    /*    std::vector<uint32_t>::const_iterator lBegin ( reinterpret_cast<uint32_t*> ( aBuffers->getSendBuffer() ) );
        std::vector<uint32_t>::const_iterator lEnd = lBegin + ( aBuffers->sendCounter() >>2 );
        std::vector<uint32_t> lData;

        for ( ; lBegin!=lEnd ; ++lBegin )
        {
          std::cout << std::setfill ( '0' ) << std::hex << std::setw ( 8 ) <<  *lBegin << std::endl;
        }*/
    //     mAsioSendBuffer.clear();
    std::vector< boost::asio::const_buffer > lAsioSendBuffer;
    lAsioSendBuffer.push_back ( boost::asio::const_buffer ( mDispatchBuffers->getSendBuffer() , mDispatchBuffers->sendCounter() ) );
    log ( Debug() , "Sending " , Integer ( mDispatchBuffers->sendCounter() ) , " bytes" );
    // log( Warning() , ThisLocation() );
    mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
#ifdef RUN_ASIO_MULTITHREADED
    mSocket.async_send_to ( lAsioSendBuffer , mEndpoint , boost::bind ( &UDP< InnerProtocol >::write_callback, this, _1 ) );
    mPacketsInFlight++;
#else
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    mSocket.async_send_to ( lAsioSendBuffer , mEndpoint , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

    do
    {
      mIOservice.run_one();
    }
    while ( lErrorCode == boost::asio::error::would_block );

    write_callback ( lErrorCode );
#endif
  }


  template < typename InnerProtocol >
  void UDP< InnerProtocol >::write_callback ( const boost::system::error_code& aErrorCode )
  {
#ifdef RUN_ASIO_MULTITHREADED
    //     if( !mDispatchBuffers)
    //     {
    //       log( Error() , __PRETTY_FUNCTION__ , " called when 'mDispatchBuffers' was NULL" );
    //       return;
    //     }
    boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );

    if ( mAsynchronousException )
    {
      return;
    }

    if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
    {
      exception::UdpTimeout* lExc = new exception::UdpTimeout();
      log ( *lExc , "ASIO reported a timeout in UDP write callback" );
      if ( aErrorCode )
      {
        log ( *lExc , "ASIO reported an error: " , Quote ( aErrorCode.message() ) );
      }
      mAsynchronousException = lExc;
      return;
    }

    if ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) )
    {
      mSocket.close();

      exception::ASIOUdpError* lExc = new exception::ASIOUdpError();
      log ( *lExc , "ASIO reported an error in write_callback:" , Quote ( aErrorCode.message() ) );
      mAsynchronousException = lExc;
      return;
    }

    if ( mReplyBuffers )
    {
      mReplyQueue.push_back ( mDispatchBuffers );
      //   std::cout << "extended mReplyQueue" << std::endl;
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
      //std::cout << "reduced mDispatchQueue" << std::endl;
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
  void UDP< InnerProtocol >::read ( )
  {
    if ( !mReplyBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mReplyBuffers' was NULL" );
      return;
    }

    //     mAsioReplyBuffer.clear();
    //    mAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( & ( mReplyMemory.at ( 0 ) ) , mReplyBuffers->replyCounter() ) );
    std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( mReplyBuffers->getReplyBuffer() );
    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
    lAsioReplyBuffer.reserve ( lReplyBuffers.size() +1 );

    for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
    {
      lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( lIt->first , lIt->second ) );
    }

    lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( mReplyBuffers->getSpareSpace() , Buffers::mSpareSpaceSize ) );
    log ( Debug() , "Expecting " , Integer ( mReplyBuffers->replyCounter() ) , " bytes in reply" );
    boost::asio::ip::udp::endpoint lEndpoint;
    mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
#ifdef RUN_ASIO_MULTITHREADED
    mSocket.async_receive_from ( lAsioReplyBuffer , lEndpoint , 0 , boost::bind ( &UDP< InnerProtocol >::read_callback, this, _1 ) );
#else
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    mSocket.async_receive_from ( lAsioReplyBuffer , lEndpoint , 0 , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

    do
    {
      mIOservice.run_one();
    }
    while ( lErrorCode == boost::asio::error::would_block );

    read_callback ( lErrorCode );
#endif
  }


  template < typename InnerProtocol >
  void UDP< InnerProtocol >::read_callback ( const boost::system::error_code& aErrorCode )
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

    if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
    {
      exception::UdpTimeout* lExc = new exception::UdpTimeout();
      log ( *lExc , "ASIO reported a timeout in UDP read callback" );
      if ( aErrorCode )
      {
        log ( *lExc , "ASIO reported an error: " , Quote ( aErrorCode.message() ) );
      }
      mAsynchronousException = lExc;
      return;
    }


    if ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) )
    {
      mSocket.close();

      exception::ASIOUdpError* lExc = new exception::ASIOUdpError();
      log ( *lExc , "ASIO reported an error in read_callback: " , Quote ( aErrorCode.message() ) );
      mAsynchronousException = lExc;
      return;
    }

    //     uint32_t lCounter(0);
    //     for ( std::vector< boost::asio::mutable_buffer >::iterator lIt = lAsioReplyBuffer.begin() ; lIt != lAsioReplyBuffer.end() ; ++lIt )
    //     {
    //     uint32_t s1 = boost::asio::buffer_size(*lIt)>>2;
    //     uint32_t* p1 = boost::asio::buffer_cast<uint32_t*>(*lIt);
    //
    //     for( uint32_t i(0) ; i!= s1 ; ++i , ++p1 )
    //     {
    //     log ( Debug() , Integer ( lCounter++ ) , " : " , Integer ( *p1 , IntFmt<hex,fixed>() ) );
    //     }
    //     }
    //         TargetToHostInspector< 2 , 0 > lT2HInspector;
    //         std::vector<uint32_t>::const_iterator lBegin2 ( ( uint32_t* ) ( & mReplyMemory[0] ) );
    //         std::vector<uint32_t>::const_iterator lEnd2 ( ( uint32_t* ) ( & mReplyMemory[aBuffers->replyCounter() ] ) );
    //         lT2HInspector.analyze ( lBegin2 , lEnd2 );
    //  std::cout << "Filling reply buffer : " << mReplyBuffers << std::endl;
    //     std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( mReplyBuffers->getReplyBuffer() );
    //     uint8_t* lReplyBuf ( & ( mReplyMemory.at ( 0 ) ) );
    //
    //     for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
    //     {
    //       memcpy ( lIt->first, lReplyBuf, lIt->second );
    //       lReplyBuf += lIt->second;
    //     }

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
      // std::cout << "reduced mReplyQueue" << std::endl;
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
      //std::cout << "reduced mDispatchQueue" << std::endl;
      write();
    }

#else
    mReplyBuffers.reset();
#endif
  }



  template < typename InnerProtocol >
  void UDP< InnerProtocol >::CheckDeadline()
  {
    // Check whether the deadline has passed. We compare the deadline against
    // the current time since a new asynchronous operation may have moved the
    // deadline before this actor had a chance to run.

    boost::lock_guard<boost::mutex> lLock ( this->mTransportLayerMutex );

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
    mDeadlineTimer.async_wait ( boost::bind ( &UDP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol >
  void UDP< InnerProtocol >::Flush( )
  {
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
    bool lContinue ( true );

    //     logo lLogo;
    while ( lContinue )
    {
      //       lLogo++;
      if ( mAsynchronousException )
      {
        mAsynchronousException->ThrowAsDerivedType();
      }

#ifdef RUN_ASIO_MULTITHREADED
      //             log( Warning() , "Mutex LOCKED @ " , ThisLocation() );
      boost::lock_guard<boost::mutex> lLock ( mTransportLayerMutex );
#endif
      //log ( Warning() , "mDispatchBuffers = " , Pointer ( mDispatchBuffers ) , " mReplyBuffers = " , Pointer ( mReplyBuffers ) );
      lContinue = ( mDispatchBuffers || mReplyBuffers );
    }
  }



  template < typename InnerProtocol >
  void UDP< InnerProtocol >::dispatchExceptionHandler()
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
    }

    InnerProtocol::dispatchExceptionHandler();
  }

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}

