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

#ifndef _uhal_ProtocolUDP_hpp_
#define _uhal_ProtocolUDP_hpp_

#include "uhal/ClientInterface.hpp"
#include "uhal/log/exception.hpp"
#include "uhal/log/log.hpp"

#include <iostream>
#include <iomanip>

#include <boost/shared_ptr.hpp>
#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/udp.hpp>
#include <boost/asio/deadline_timer.hpp>

#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>

#include <string>

namespace uhal
{

  namespace exception
  {
    //! Exception class to handle the case where the UDP connection timed out.
    ExceptionClass ( UdpTimeout , "Exception class to handle the case where the UDP connection timed out." );
    //! Exception class to handle a failure to create a UDP socket.
    ExceptionClass ( ErrorAtUdpSocketCreation , "Exception class to handle a failure to create a UDP socket." );
  }

  //! Transport protocol to transfer an IPbus buffer via UDP
  template < typename InnerProtocol >
  class UDP : public InnerProtocol
  {

    public:
      //! Functor class to perform the actual transport, Like this to allow multithreading if desirable.

      /**
      	Constructor
      	@param aId the uinique identifier that the client will be given.
      	@param aUri a struct containing the full URI of the target.
      */
      UDP ( const std::string& aId, const URI& aUri );

      /**
      	Destructor
      */
      virtual ~UDP();

      /**
      	Send the IPbus buffer to the target, read back the response and call the packing-protocol's validate function
      	@param aBuffers the buffer object wrapping the send and recieve buffers that are to be transported
      	If multithreaded, adds buffer to the dispatch queue and returns. If single-threaded, calls the dispatch-worker dispatch function directly and blocks until the response is validated.
      */
      void implementDispatch ();

      /**
      Concrete implementation of the synchronization function to block until all buffers have been sent, all replies received and all data validated
       */
      virtual void Flush( );


    protected:
      virtual void dispatchExceptionHandler();


    private:

      void connect();

      void write ( Buffers* aBuffers );
      void write_callback ( Buffers* aBuffers , const boost::system::error_code& aErrorCode );
      void read ( Buffers* aBuffers );
      void read_callback ( Buffers* aBuffers , const boost::system::error_code& aErrorCode );


      void CheckDeadline();


    private:
      //! The boost::asio::io_service used to create the connections
      boost::asio::io_service mIOservice;

      boost::asio::io_service::work mIOserviceWork;

      //! A shared pointer to a boost::asio udp socket through which the operation will be performed
      boost::asio::ip::udp::socket mSocket;

      //! A shared pointer to a boost::asio udp endpoint stored as a member as UDP as no concept of a connection
      boost::asio::ip::udp::endpoint mEndpoint;

      boost::asio::deadline_timer mDeadlineTimer;

      std::vector<uint8_t> mReplyMemory;

      boost::thread mDispatchThread;

      std::vector< boost::asio::const_buffer > mAsioSendBuffer;
      std::vector< boost::asio::mutable_buffer > mAsioReplyBuffer;

      //! A MutEx lock used to make sure the access functions are thread safe
      boost::mutex mUdpMutex;
      std::deque < Buffers* > mDispatchQueue;
      std::deque < Buffers* > mReplyQueue;

      uhal::exception::exception* mAsynchronousException;

  };


}

#include "uhal/TemplateDefinitions/ProtocolUDP.hxx"

#endif
