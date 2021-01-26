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

#include "uhal/ProtocolTCP.hpp"


#include <chrono>
#include <mutex>
#include <sys/time.h>

#include <boost/asio/connect.hpp>
#include <boost/asio/write.hpp>
#include <boost/asio/read.hpp>
#include <boost/asio/placeholders.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "uhal/Buffers.hpp"
#include "uhal/grammars/URI.hpp"
#include "uhal/IPbusInspector.hpp"
#include "uhal/log/LogLevels.hpp"
#include "uhal/log/log.hpp"
#include "uhal/log/log_inserters.integer.hpp"
#include "uhal/log/log_inserters.quote.hpp"
#include "uhal/log/log_inserters.type.hpp"
#include "uhal/ProtocolIPbus.hpp"
#include "uhal/ProtocolControlHub.hpp"


namespace uhal
{

  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  TCP< InnerProtocol, nr_buffers_per_send >::TCP ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri ),
    mMaxPayloadSize (350 * 4),
    mIOservice ( ),
    mSocket ( mIOservice ),
    mEndpoint ( boost::asio::ip::tcp::resolver ( mIOservice ).resolve ( boost::asio::ip::tcp::resolver::query ( aUri.mHostname , aUri.mPort ) ) ),
    mDeadlineTimer ( mIOservice ),
    mIOserviceWork ( mIOservice ),
    mDispatchThread ( [this] () { mIOservice.run(); } ),
    mDispatchQueue(),
    mReplyQueue(),
    mPacketsInFlight ( 0 ),
    mFlushStarted ( false ),
    mFlushDone ( true ),
    mAsynchronousException ( NULL )
  {
    mDeadlineTimer.async_wait ([this] (const boost::system::error_code&) { this->CheckDeadline(); });

    // Extract value of 'max_payload_size' attribute, if present
    for (const auto& lArg : aUri.mArguments) {
      if (lArg.first == "max_payload_size") {
        try {
          mMaxPayloadSize = boost::lexical_cast<size_t>(lArg.second);
        }
        catch (const boost::bad_lexical_cast&) {
          throw exception::InvalidURI("Client URI \"" + this->uri() + "\": Invalid value, \"" + lArg.second + "\", specified for attribute \"" + lArg.first + "\"");
        }
        log (Info(), "Client with URI ", Quote(this->uri()), ": Maximum UDP payload size set to ", std::to_string(mMaxPayloadSize), " bytes");
      }
    }
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  TCP< InnerProtocol , nr_buffers_per_send >::~TCP()
  {
    try
    {
      mSocket.close();

      while ( mSocket.is_open() )
        {}

      mIOservice.stop();
      mDispatchThread.join();
      ClientInterface::returnBufferToPool ( mDispatchQueue );
      for (size_t i = 0; i < mReplyQueue.size(); i++)
        ClientInterface::returnBufferToPool ( mReplyQueue.at(i).first );
      ClientInterface::returnBufferToPool ( mDispatchBuffers );
      ClientInterface::returnBufferToPool ( mReplyBuffers.first );
    }
    catch ( const std::exception& aExc )
    {
      log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught in " , Type<TCP< InnerProtocol , nr_buffers_per_send > >(), " destructor" );
    }
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::implementDispatch ( std::shared_ptr< Buffers > aBuffers )
  {
    std::lock_guard<std::mutex> lLock ( mTransportLayerMutex );

    if ( mAsynchronousException )
    {
      log ( *mAsynchronousException , "Rethrowing Asynchronous Exception from 'implementDispatch' method of " , Type<TCP< InnerProtocol , nr_buffers_per_send > >() );
      mAsynchronousException->throwAsDerivedType();
    }

    if ( ! mSocket.is_open() )
    {
      connect();
    }

    mFlushStarted = false;
    mDispatchQueue.push_back ( aBuffers );

    if ( mDispatchBuffers.empty() && ( mDispatchQueue.size() >= nr_buffers_per_send ) && ( mPacketsInFlight < this->getMaxNumberOfBuffers() ) )
    {
      write ( );
    }
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  uint32_t TCP< InnerProtocol , nr_buffers_per_send >::getMaxSendSize()
  {
    return mMaxPayloadSize;
  }

 
  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  uint32_t TCP< InnerProtocol , nr_buffers_per_send >::getMaxReplySize()
  {
    return mMaxPayloadSize;
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::connect()
  {
    log ( Info() , "Attempting to create TCP connection to '" , mEndpoint->host_name() , "' port " , mEndpoint->service_name() , "." );
    mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    boost::system::error_code lErrorCode;
    boost::asio::connect ( mSocket , mEndpoint , lErrorCode );

    if ( lErrorCode )
    {
      mSocket.close();

      while ( mSocket.is_open() )
        {}

      exception::TcpConnectionFailure lExc;
      std::ostringstream oss;

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        oss << "Timeout (" << this->getBoostTimeoutPeriod().total_milliseconds() << " milliseconds) occurred when connecting to ";
      }
      else if ( lErrorCode == boost::asio::error::connection_refused )
      {
        oss << "Connection refused for ";
      }
      else
      {
        oss << "Error \"" << lErrorCode.message() << "\" encountered when connecting to ";
      }

      log ( lExc , oss.str() , ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) ,
            " at ", Quote ( this->mUri.mHostname + ":" + this->mUri.mPort ) , ". URI=" , Quote ( this->uri() ) );
      throw lExc;
    }

    mSocket.set_option ( boost::asio::ip::tcp::no_delay ( true ) );
    //    boost::asio::socket_base::non_blocking_io lNonBlocking ( true );
    //    mSocket.io_control ( lNonBlocking );
    log ( Info() , "TCP connection succeeded" );
  }



  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::write ( )
  {
    NotifyConditionalVariable ( false );

    std::vector< boost::asio::const_buffer > lAsioSendBuffer;
    lAsioSendBuffer.push_back ( boost::asio::const_buffer ( &mSendByteCounter , 4 ) );
    mSendByteCounter = 0;
    std::size_t lNrBuffersToSend = std::min ( mDispatchQueue.size(), nr_buffers_per_send );
    mDispatchBuffers.reserve ( lNrBuffersToSend );

    for ( std::size_t i = 0; i < lNrBuffersToSend; i++ )
    {
      mDispatchBuffers.push_back ( mDispatchQueue.front() );
      mDispatchQueue.pop_front();
      const std::shared_ptr<Buffers>& lBuffer = mDispatchBuffers.back();
      mSendByteCounter += lBuffer->sendCounter();
      lAsioSendBuffer.push_back ( boost::asio::const_buffer ( lBuffer->getSendBuffer() , lBuffer->sendCounter() ) );
    }

    log ( Debug() , "Sending " , Integer ( mSendByteCounter ) , " bytes from ", Integer ( mDispatchBuffers.size() ), " buffers" );
    mSendByteCounter = htonl ( mSendByteCounter );

    mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );

    // Patch for suspected bug in using boost asio with boost python; see https://svnweb.cern.ch/trac/cactus/ticket/323#comment:7
    while ( mDeadlineTimer.expires_from_now() < boost::posix_time::microseconds ( 600 ) )
    {
      log ( Debug() , "Resetting deadline timer since it just got set to strange value, likely due to a bug within boost (expires_from_now was: ", mDeadlineTimer.expires_from_now() , ")." );
      mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    }

    boost::asio::async_write ( mSocket , lAsioSendBuffer , [&] (const boost::system::error_code& e, std::size_t n) { this->write_callback(e, n); });
    mPacketsInFlight += mDispatchBuffers.size();

    SteadyClock_t::time_point lNow = SteadyClock_t::now();
    if (mLastSendQueued > SteadyClock_t::time_point())
      mInterSendTimeStats.add(mLastSendQueued, lNow);
    mLastSendQueued = lNow;
  }



  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::write_callback ( const boost::system::error_code& aErrorCode , std::size_t aBytesTransferred )
  {
    std::lock_guard<std::mutex> lLock ( mTransportLayerMutex );
    mSendByteCounter = ntohl ( mSendByteCounter );

    if ( mAsynchronousException )
    {
      NotifyConditionalVariable ( true );
      return;
    }

    if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
    {
      mAsynchronousException = new exception::TcpTimeout();
      log ( *mAsynchronousException , "Timeout (" , Integer ( this->getBoostTimeoutPeriod().total_milliseconds() ) , " milliseconds) occurred for send to ",
            ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: ", this->uri() );

      if ( aErrorCode && aErrorCode != boost::asio::error::operation_aborted )
      {
        log ( *mAsynchronousException , "ASIO reported an error: " , Quote ( aErrorCode.message() ) );
      }

      NotifyConditionalVariable ( true );
      return;
    }

    if ( ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) ) || ( aBytesTransferred != ( mSendByteCounter+4 ) ) )
    {
      mAsynchronousException = new exception::ASIOTcpError();
      log ( *mAsynchronousException , "Error ", Quote ( aErrorCode.message() ) , " encountered during send to ",
            ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: " , this->uri() );

      try
      {
        mSocket.close();

        while ( mSocket.is_open() )
          {}
      }
      catch ( const std::exception& aExc )
      {
        log ( *mAsynchronousException , "Error closing TCP socket following the ASIO send error" );
      }

      if ( aBytesTransferred != ( mSendByteCounter + 4 ) )
      {
        log ( *mAsynchronousException , "Attempted to send " , Integer ( mSendByteCounter ) , " bytes to ",
             ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) ,
             " with URI "  , Quote ( this->uri() ) , ", but only sent " , Integer ( aBytesTransferred ) , " bytes" );
      }

      NotifyConditionalVariable ( true );
      return;
    }

    if ( ! mReplyBuffers.first.empty() )
    {
      mReplyQueue.push_back ( std::make_pair(mDispatchBuffers, mLastSendQueued) );
    }
    else
    {
      mReplyBuffers = std::make_pair(mDispatchBuffers, mLastSendQueued);
      read ( );
    }

    mDispatchBuffers.clear();

    if ( ( mDispatchQueue.size() >= (mFlushStarted ? 1 : nr_buffers_per_send) ) && ( mPacketsInFlight < this->getMaxNumberOfBuffers() ) )
    {
      write();
    }

  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::read ( )
  {
    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;
    lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( &mReplyByteCounter , 4 ) );
    log ( Debug() , "Getting reply byte counter" );
    boost::asio::ip::tcp::endpoint lEndpoint;
    mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );

    // Patch for suspected bug in using boost asio with boost python; see https://svnweb.cern.ch/trac/cactus/ticket/323#comment:7
    while ( mDeadlineTimer.expires_from_now() < boost::posix_time::microseconds ( 600 ) )
    {
      log ( Debug() , "Resetting deadline timer since it just got set to strange value, likely due to a bug within boost (expires_from_now was: ", mDeadlineTimer.expires_from_now() , ")." );
      mDeadlineTimer.expires_from_now ( this->getBoostTimeoutPeriod() );
    }

    boost::asio::async_read ( mSocket , lAsioReplyBuffer ,  boost::asio::transfer_exactly ( 4 ), [&] (const boost::system::error_code& e, std::size_t n) { this->read_callback(e, n); });
    SteadyClock_t::time_point lNow = SteadyClock_t::now();
    if (mLastRecvQueued > SteadyClock_t::time_point())
      mInterRecvTimeStats.add(mLastRecvQueued, lNow);
    mLastRecvQueued = lNow;
  }





  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::read_callback ( const boost::system::error_code& aErrorCode , std::size_t aBytesTransferred )
  {
    std::size_t lNrReplyBuffers = 1;
    uint32_t lRequestBytes = 0;
    uint32_t lExpectedReplyBytes = 0;

    for (const auto& lBuf: mReplyBuffers.first)
    {
      lNrReplyBuffers += lBuf->getReplyBuffer().size();
      lRequestBytes += lBuf->sendCounter();
      lExpectedReplyBytes += lBuf->replyCounter();
    }

    {
      std::lock_guard<std::mutex> lLock ( mTransportLayerMutex );
      if ( mAsynchronousException )
      {
        NotifyConditionalVariable ( true );
        return;
      }

      SteadyClock_t::time_point lNow = SteadyClock_t::now();

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        exception::TcpTimeout* lExc = new exception::TcpTimeout();
        log ( *lExc , "Timeout (" , Integer ( this->getBoostTimeoutPeriod().total_milliseconds() ) , " ms) occurred for receive (header) from ",
              ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI '", this->uri(), "'. ",
              Integer(mPacketsInFlight), " packets in flight, ", Integer(mReplyBuffers.first.size()), " in this chunk (",
              Integer(lRequestBytes), "/", Integer(lExpectedReplyBytes), " bytes sent/expected). Last send / receive queued ",
              std::chrono::duration<float, std::milli>(lNow - mLastSendQueued).count(), " / ",
              std::chrono::duration<float, std::milli>(lNow - mLastRecvQueued).count(), " ms ago.");

        log ( Error(), "Extra timeout-related info - round-trip times: ", mRTTStats);
        log ( Error(), "Extra timeout-related info - send-recv  times: ", mLSTStats);
        log ( Error(), "Extra timeout-related info - inter-send times: ", mInterSendTimeStats);
        log ( Error(), "Extra timeout-related info - inter-recv times: ", mInterRecvTimeStats);

        if ( aErrorCode && aErrorCode != boost::asio::error::operation_aborted )
        {
          log ( *lExc , "ASIO reported an error: " , Quote ( aErrorCode.message() ) );
        }

        mAsynchronousException = lExc;
        NotifyConditionalVariable ( true );
        return;
      }
    }

    if ( ( aErrorCode && ( aErrorCode != boost::asio::error::eof ) ) || ( aBytesTransferred != 4 ) )
    {
      std::lock_guard<std::mutex> lLock ( mTransportLayerMutex );
      mAsynchronousException = new exception::ASIOTcpError();

      if ( aErrorCode )
      {
        log ( *mAsynchronousException , "Error ", Quote ( aErrorCode.message() ) , " encountered during receive from ",
              ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: " , this->uri() );
      }

      if ( aBytesTransferred != 4 )
      {
        log ( *mAsynchronousException, "Expected to receive 4-byte header in async read from ",
             ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) ,
             " with URI "  , Quote ( this->uri() ) , ", but only received " , Integer ( aBytesTransferred ) , " bytes" );
      }

      try
      {
        mSocket.close();
        while ( mSocket.is_open() )
          {}
      }
      catch ( const std::exception& aExc )
      {
        log ( *mAsynchronousException , "Error closing socket following ASIO read error" );
      }

      NotifyConditionalVariable ( true );
      return;
    }

    SteadyClock_t::time_point lReadHeaderTimestamp = SteadyClock_t::now();
    mRTTStats.add(mReplyBuffers.second, lReadHeaderTimestamp);
    mLSTStats.add(mLastSendQueued, lReadHeaderTimestamp);

    mReplyByteCounter = ntohl ( mReplyByteCounter );
    log ( Debug() , "Byte Counter says " , Integer ( mReplyByteCounter ) , " bytes are coming" );
    std::vector< boost::asio::mutable_buffer > lAsioReplyBuffer;

    lAsioReplyBuffer.reserve ( lNrReplyBuffers );
    log ( Debug() , "Expecting " , Integer ( lExpectedReplyBytes ) , " bytes in reply, for ", Integer ( mReplyBuffers.first.size() ), " buffers" );

    for ( std::vector< std::shared_ptr<Buffers> >::const_iterator lBufIt = mReplyBuffers.first.begin(); lBufIt != mReplyBuffers.first.end(); lBufIt++ )
    {
      std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( ( *lBufIt )->getReplyBuffer() );

      for ( std::deque< std::pair< uint8_t* , uint32_t > >::iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
      {
        lAsioReplyBuffer.push_back ( boost::asio::mutable_buffer ( lIt->first , lIt->second ) );
      }
    }

    boost::system::error_code lErrorCode = boost::asio::error::would_block;
    std::size_t lBytesTransferred = boost::asio::read ( mSocket , lAsioReplyBuffer ,  boost::asio::transfer_exactly ( mReplyByteCounter ), lErrorCode );

    if ( ( lErrorCode && ( lErrorCode != boost::asio::error::eof ) ) || ( lBytesTransferred != mReplyByteCounter ) )
    {
      std::lock_guard<std::mutex> lLock ( mTransportLayerMutex );
      mSocket.close();

      if ( mDeadlineTimer.expires_at () == boost::posix_time::pos_infin )
      {
        mAsynchronousException = new exception::TcpTimeout();
        log ( *mAsynchronousException , "Timeout (" , Integer ( this->getBoostTimeoutPeriod().total_milliseconds() ) , " milliseconds) occurred for receive (chunk) from ",
              ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: ", this->uri() );
      }
      else
      {
        mAsynchronousException = new exception::ASIOTcpError();
        log ( *mAsynchronousException , "Error ", Quote ( aErrorCode.message() ) , " encountered during receive from ",
              ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) , " with URI: " , this->uri() );
      }

      if ( lBytesTransferred != mReplyByteCounter )
      {
        log ( *mAsynchronousException, "Expected to receive " , Integer ( mReplyByteCounter ) , " bytes in read from ",
             ( this->uri().find ( "chtcp-" ) == 0 ? "ControlHub" : "TCP server" ) ,
             " with URI "  , Quote ( this->uri() ) , ", but only received " , Integer ( lBytesTransferred ) , " bytes" );
      }

      NotifyConditionalVariable ( true );
      return;
    }


    for (const auto& lBuf: mReplyBuffers.first)
    {
      try
      {
        if ( uhal::exception::exception* lExc =  ClientInterface::validate ( lBuf ) ) //Control of the pointer has been passed back to the client interface
        {
          std::lock_guard<std::mutex> lLock ( mTransportLayerMutex );
          mAsynchronousException = lExc;
        }
      }
      catch ( exception::exception& aExc )
      {
        std::lock_guard<std::mutex> lLock ( mTransportLayerMutex );
        mAsynchronousException = new exception::ValidationError ();
        log ( *mAsynchronousException , "Exception caught during reply validation for TCP device with URI " , Quote ( this->uri() ) , "; what returned: " , Quote ( aExc.what() ) );
      }

      if ( mAsynchronousException )
      {
        NotifyConditionalVariable ( true );
        return;
      }
    }

    std::lock_guard<std::mutex> lLock ( mTransportLayerMutex );
    mPacketsInFlight -= mReplyBuffers.first.size();

    if ( mReplyQueue.size() )
    {
      mReplyBuffers = mReplyQueue.front();
      mReplyQueue.pop_front();
      read();
    }
    else
    {
      mReplyBuffers.first.clear();
    }

    if ( mDispatchBuffers.empty() && ( mDispatchQueue.size() >= (mFlushStarted ? 1 : nr_buffers_per_send) ) && ( mPacketsInFlight < this->getMaxNumberOfBuffers() ) )
    {
      write();
    }

    if ( mDispatchBuffers.empty() && mReplyBuffers.first.empty() )
    {
      mDeadlineTimer.expires_from_now ( boost::posix_time::seconds(60) );
      NotifyConditionalVariable ( true );
    }
  }



  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::CheckDeadline()
  {
    std::lock_guard<std::mutex> lLock ( this->mTransportLayerMutex );

    // Check whether the deadline has passed. We compare the deadline against the current time since a new asynchronous operation may have moved the deadline before this actor had a chance to run.
    if ( mDeadlineTimer.expires_at() <= boost::asio::deadline_timer::traits_type::now() )
    {
      // SETTING THE EXCEPTION HERE CAN APPEAR AS A TIMEOUT WHEN NONE ACTUALLY EXISTS
      if (  mDispatchBuffers.size() || mReplyBuffers.first.size() )
      {
        log ( Warning() , "Closing TCP socket for device with URI " , Quote ( this->uri() ) , " since deadline has passed" );
      }
      else
      {
        log ( Debug() , "Closing TCP socket for device with URI " , Quote ( this->uri() ) , " since no communication in 60 seconds" );
      }

      // The deadline has passed. The socket is closed so that any outstanding asynchronous operations are cancelled.
      mSocket.close();
      // There is no longer an active deadline. The expiry is set to positive infinity so that the actor takes no action until a new deadline is set.
      mDeadlineTimer.expires_at ( boost::posix_time::pos_infin );
    }

    // Put the actor back to sleep.
    mDeadlineTimer.async_wait ([this] (const boost::system::error_code&) { this->CheckDeadline(); });
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::Flush( )
  {
    {
      std::lock_guard<std::mutex> lLock ( mTransportLayerMutex );
      mFlushStarted = true;

      if ( mDispatchQueue.size() && mDispatchBuffers.empty() )
      {
        write();
      }
    }

    WaitOnConditionalVariable();

    std::lock_guard<std::mutex> lLock ( mTransportLayerMutex ); 

    if ( mAsynchronousException )
    {
      mAsynchronousException->throwAsDerivedType();
    }
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::dispatchExceptionHandler()
  {
    if ( mSocket.is_open() )
    {
      log ( Warning() , "Closing TCP socket for device with URI " , Quote ( this->uri() ) , " since exception detected." );

      try
      {
        mSocket.close();

        while ( mSocket.is_open() )
          {}
      }
      catch ( ... )
        {}
    }

    NotifyConditionalVariable ( true );

    if ( mAsynchronousException )
    {
      delete mAsynchronousException;
      mAsynchronousException = NULL;
    }

    ClientInterface::returnBufferToPool ( mDispatchQueue );
      for (size_t i = 0; i < mReplyQueue.size(); i++)
        ClientInterface::returnBufferToPool ( mReplyQueue.at(i).first );
    mPacketsInFlight = 0;
    ClientInterface::returnBufferToPool ( mDispatchBuffers );
    ClientInterface::returnBufferToPool ( mReplyBuffers.first );

    mSendByteCounter = 0;
    mReplyByteCounter = 0;

    InnerProtocol::dispatchExceptionHandler();
  }


  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::NotifyConditionalVariable ( const bool& aValue )
  {
    {
      std::lock_guard<std::mutex> lLock ( mConditionalVariableMutex );
      mFlushDone = aValue;
    }
    mConditionalVariable.notify_one();
  }

  template < typename InnerProtocol , std::size_t nr_buffers_per_send >
  void TCP< InnerProtocol , nr_buffers_per_send >::WaitOnConditionalVariable()
  {
    std::unique_lock<std::mutex> lLock ( mConditionalVariableMutex );

    while ( !mFlushDone )
    {
      mConditionalVariable.wait ( lLock );
    }
  }


  template class TCP< IPbus< 1 , 3 > , 1 >;
  template class TCP< IPbus< 2 , 0 > , 1 >;

  template class TCP< ControlHub < IPbus< 1 , 3 > > , 3 >;
  template class TCP< ControlHub < IPbus< 2 , 0 > > , 3 >;
}

