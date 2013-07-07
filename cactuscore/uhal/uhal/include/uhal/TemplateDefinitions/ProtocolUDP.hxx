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
  UDP< InnerProtocol >::UDP ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri ),
    mIOservice ( ),
#ifdef RUN_ASIO_MULTITHREADED
    mIOserviceWork ( mIOservice ),
#endif
    mSocket ( mIOservice , boost::asio::ip::udp::endpoint ( boost::asio::ip::udp::v4(), 0 ) ),
    mEndpoint ( *boost::asio::ip::udp::resolver ( mIOservice ).resolve ( boost::asio::ip::udp::resolver::query ( boost::asio::ip::udp::v4() , aUri.mHostname , aUri.mPort ) ) ),
    mDeadlineTimer ( mIOservice ),
    mReplyMemory ( 65536 , 0x00000000 ),
#ifdef RUN_ASIO_MULTITHREADED
    mDispatchThread ( boost::bind ( &boost::asio::io_service::run , & ( mIOservice ) ) ),
#endif
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ( boost::bind ( &UDP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol >
  UDP< InnerProtocol >::~UDP()
  {
    try
    {
      mSocket.close();
      mIOservice.stop();
#ifdef RUN_ASIO_MULTITHREADED
      mDispatchThread.join();
#endif
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
    }
  }



  template < typename InnerProtocol >
  void UDP< InnerProtocol >::implementDispatch ( Buffers* aBuffers )
  {
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
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
      boost::lock_guard<boost::mutex> lLock ( mUdpMutex );
#endif
      mUdpDispatchQueue.push_back ( aBuffers );

      if ( mUdpDispatchQueue.size() == 1 )
      {
        write ( aBuffers );
      }
    }
  }



  template < typename InnerProtocol >
  void UDP< InnerProtocol >::connect()
  {
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
    log ( Info() , "Creating new UDP socket, as it appears to have been closed..." );
    //mSocket = boost::asio::ip::udp::socket ( mIOservice , boost::asio::ip::udp::endpoint ( boost::asio::ip::udp::v4(), 0 ) );
    mSocket.open ( boost::asio::ip::udp::v4() );
    boost::asio::socket_base::non_blocking_io lNonBlocking ( true );
    mSocket.io_control ( lNonBlocking );
    log ( Info() , "UDP socket created successfully." );
  }


  template < typename InnerProtocol >
  void UDP< InnerProtocol >::write ( Buffers* aBuffers )
  {
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
    /*    log ( Info() , ThisLocation(), ", Buffer: " , Pointer ( aBuffers ) );
        std::vector<uint32_t>::const_iterator lBegin ( reinterpret_cast<uint32_t*> ( aBuffers->getSendBuffer() ) );
        std::vector<uint32_t>::const_iterator lEnd = lBegin + ( aBuffers->sendCounter() >>2 );
        std::vector<uint32_t> lData;

        for ( ; lBegin!=lEnd ; ++lBegin )
        {
          std::cout << std::setfill ( '0' ) << std::hex << std::setw ( 8 ) <<  *lBegin << std::endl;
        }*/
    mAsioSendBuffer.clear();
    mAsioSendBuffer.push_back ( boost::asio::const_buffer ( aBuffers->getSendBuffer() , aBuffers->sendCounter() ) );
    log ( Debug() , "Sending " , Integer ( aBuffers->sendCounter() ) , " bytes" );
    // log( Warning() , ThisLocation() );
    mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
#ifdef RUN_ASIO_MULTITHREADED
    mSocket.async_send_to ( mAsioSendBuffer , mEndpoint , boost::bind ( &UDP< InnerProtocol >::write_callback, this, aBuffers , _1 ) );
#else
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    mSocket.async_send_to ( mAsioSendBuffer , mEndpoint , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );

    do
    {
      mIOservice.run_one();
    }
    while ( lErrorCode == boost::asio::error::would_block );

    write_callback ( aBuffers , lErrorCode );
#endif
  }


  template < typename InnerProtocol >
  void UDP< InnerProtocol >::write_callback ( Buffers* aBuffers , const boost::system::error_code& aErrorCode )
  {
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
    // log( Warning() , ThisLocation() );
#ifdef RUN_ASIO_MULTITHREADED
    boost::lock_guard<boost::mutex> lLock ( mUdpMutex );
#endif
    mUdpReplyQueue.push_back ( aBuffers );

    if ( mUdpReplyQueue.size() == 1 )
    {
      read ( aBuffers );
    }

    mUdpDispatchQueue.pop_front();

    if ( mUdpDispatchQueue.size() )
    {
      write ( mUdpDispatchQueue.front() );
    }
  }


  template < typename InnerProtocol >
  void UDP< InnerProtocol >::read ( Buffers* aBuffers )
  {
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
    mAsioReplyBuffer.clear();
    mAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( & ( mReplyMemory.at ( 0 ) ) , aBuffers->replyCounter() ) );
    log ( Debug() , "Expecting " , Integer ( aBuffers->replyCounter() ) , " bytes in reply" );
    boost::asio::ip::udp::endpoint lEndpoint;
    // log( Warning() , ThisLocation() );
    mDeadlineTimer.expires_from_now ( this->mTimeoutPeriod );
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
#ifdef RUN_ASIO_MULTITHREADED
    mSocket.async_receive_from ( mAsioReplyBuffer , lEndpoint , 0 , boost::bind ( &UDP< InnerProtocol >::read_callback, this, aBuffers , _1 ) );
#else
    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    mSocket.async_receive_from ( mAsioReplyBuffer , lEndpoint , 0 , boost::lambda::var ( lErrorCode ) = boost::lambda::_1 );
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;

    do
    {
      mIOservice.run_one();
    }
    while ( lErrorCode == boost::asio::error::would_block );

    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
    read_callback ( aBuffers , lErrorCode );
#endif
  }


  template < typename InnerProtocol >
  void UDP< InnerProtocol >::read_callback ( Buffers* aBuffers , const boost::system::error_code& aErrorCode )
  {
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
    // log( Warning() , ThisLocation() );
    if ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) )
    {
      mSocket.close();

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        log ( Error() , "ASIO reported a Timeout in UDP callback" );
        mAsynchronousException = new exception::UdpTimeout();
        return;
      }

      log ( Error() , "ASIO reported an error: " , Quote ( aErrorCode.message() ) , ". Attempting validation to see if we can get any more info." );

      try
      {
        mAsynchronousException = ClientInterface::validate ( aBuffers );
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
    std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( aBuffers->getReplyBuffer() );
    uint8_t* lReplyBuf ( & ( mReplyMemory.at ( 0 ) ) );

    for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
    {
      memcpy ( lIt->first, lReplyBuf, lIt->second );
      //log ( Notice() , "Memory location = " , Integer ( ( std::size_t ) ( lIt->first ) , IntFmt<hex,fixed>() ), " Memory value = " , Integer ( * ( std::size_t* ) ( lIt->first ) , IntFmt<hex,fixed>() ), " & size = " , Integer ( lIt->second ) );
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
      mAsynchronousException = ClientInterface::validate ( aBuffers );
    }
    catch ( exception::exception& aExc )
    {
      log ( Error() , "Validation function reported an error when processing buffer: " , Pointer ( aBuffers ) );
      mAsynchronousException = new exception::ValidationError ();
    }

    if ( mAsynchronousException )
    {
      return;
    }

#ifdef RUN_ASIO_MULTITHREADED
    boost::lock_guard<boost::mutex> lLock ( mUdpMutex );
#endif
    mUdpReplyQueue.pop_front();

    if ( mUdpReplyQueue.size() )
    {
      read ( mUdpReplyQueue.front() );
    }
  }



  template < typename InnerProtocol >
  void UDP< InnerProtocol >::CheckDeadline()
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
    mDeadlineTimer.async_wait ( boost::bind ( &UDP::CheckDeadline, this ) );
  }


  template < typename InnerProtocol >
  void UDP< InnerProtocol >::Flush( )
  {
    // std::cout << __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << std::endl;
    bool lContinue ( true );

    do
    {
      if ( mAsynchronousException )
      {
        mAsynchronousException->ThrowAsDerivedType();
      }

#ifdef RUN_ASIO_MULTITHREADED
      boost::lock_guard<boost::mutex> lLock ( this->mUdpMutex );
#endif
      //      std::cout << mUdpDispatchQueue.size() << " " << mUdpReplyQueue.size() << std::endl;
      lContinue = ( mUdpDispatchQueue.size() || mUdpReplyQueue.size() );
    }
    while ( lContinue );
  }


  template < typename InnerProtocol >
  void UDP< InnerProtocol >::dispatchExceptionHandler()
  {
    if ( mAsynchronousException )
    {
      delete mAsynchronousException;
      mAsynchronousException = NULL;
    }

    mSocket.close();
    mUdpReplyQueue.clear();
    mUdpDispatchQueue.clear();
    InnerProtocol::dispatchExceptionHandler();
  }

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

}

