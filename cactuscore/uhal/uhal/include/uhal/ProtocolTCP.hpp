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

/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_ProtocolTCP_hpp_
#define _uhal_ProtocolTCP_hpp_

#include "uhal/ClientInterface.hpp"
#include "uhal/log/exception.hpp"
#include "uhal/log/log.hpp"

#include <iostream>
#include <iomanip>

#include <boost/shared_ptr.hpp>
#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/deadline_timer.hpp>

#ifdef RUN_ASIO_MULTITHREADED
#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
#endif

#include <string>

namespace uhal
{


  namespace exception
  {
    //! Exception class to handle the case where the TCP connection timed out.
    ExceptionClass ( TcpTimeout , "Exception class to handle the case where the TCP connection timed out." );
    //! Exception class to handle a failure to create a TCP socket.
    ExceptionClass ( ErrorAtTcpSocketCreation , "Exception class to handle a failure to create a TCP socket." );
    //! Exception class to handle the case where the error flag was raised in the asynchronous callback system.
    ExceptionClass ( TcpConnectionFailure , "Exception class to handle the case where the TCP connection was refused or aborted." );
    //! Exception class to handle the case where ASIO returned an error.
    ExceptionClass ( ASIOTcpError , "Exception class to handle the case where ASIO returned an error." );
  }

  //! Transport protocol to transfer an IPbus buffer via TCP
  template < typename InnerProtocol >
  class TCP : public InnerProtocol
  {

    public:
      //! Functor class to perform the actual transport, Like this to allow multithreading if desirable.

      /**
      	Constructor
      	@param aId the uinique identifier that the client will be given.
      	@param aUri a struct containing the full URI of the target.
      */
      TCP ( const std::string& aId, const URI& aUri );


      TCP ( const TCP& aTCP );

      TCP& operator= ( const TCP& aTCP );


      /**
      	Destructor
      */
      virtual ~TCP();

      /**
      	Send the IPbus buffer to the target, read back the response and call the packing-protocol's validate function
      	@param aBuffers the buffer object wrapping the send and recieve buffers that are to be transported
      	If multithreaded, adds buffer to the dispatch queue and returns. If single-threaded, calls the dispatch-worker dispatch function directly and blocks until the response is validated.
      */
      void implementDispatch ( boost::shared_ptr< Buffers > aBuffers );

      /**
      Concrete implementation of the synchronization function to block until all buffers have been sent, all replies received and all data validated
       */
      virtual void Flush( );


    protected:
      virtual void dispatchExceptionHandler();


    private:

      void connect();

      void write ( );
      void write_callback ( const boost::system::error_code& aErrorCode );
      void read ( );
      void read_callback ( const boost::system::error_code& aErrorCode );


      void CheckDeadline();


    private:
      //! The boost::asio::io_service used to create the connections
      boost::asio::io_service mIOservice;

#ifdef RUN_ASIO_MULTITHREADED
      boost::asio::io_service::work mIOserviceWork;
#endif

      //! A shared pointer to a boost::asio tcp socket through which the operation will be performed
      boost::asio::ip::tcp::socket mSocket;

      //! A shared pointer to a boost::asio tcp endpoint stored as a member as TCP as no concept of a connection
      boost::asio::ip::tcp::resolver::iterator mEndpoint;

      boost::asio::deadline_timer mDeadlineTimer;

      //       std::vector<uint8_t> mReplyMemory;

#ifdef RUN_ASIO_MULTITHREADED
      boost::thread mDispatchThread;
#endif

      //       std::vector< boost::asio::const_buffer > mAsioSendBuffer;
      //       std::vector< boost::asio::mutable_buffer > mAsioReplyBuffer;

      //! A MutEx lock used to make sure the access functions are thread safe
#ifdef RUN_ASIO_MULTITHREADED
      boost::mutex mTransportLayerMutex;

      std::deque < boost::shared_ptr< Buffers > > mDispatchQueue;
      std::deque < boost::shared_ptr< Buffers > > mReplyQueue;

      uint32_t mPacketsInFlight;
#endif

      boost::shared_ptr< Buffers > mDispatchBuffers;
      boost::shared_ptr< Buffers > mReplyBuffers;


      uhal::exception::exception* mAsynchronousException;

  };


}

#include "uhal/TemplateDefinitions/ProtocolTCP.hxx"

#endif
