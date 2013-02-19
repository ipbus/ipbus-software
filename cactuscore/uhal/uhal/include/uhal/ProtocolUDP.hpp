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
//#include <boost/bind.hpp>
//#include <boost/asio.hpp>
#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/udp.hpp>
#include <boost/asio/deadline_timer.hpp>

#ifdef USE_UDP_MULTITHREADED
//#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
#endif

#include <string>

namespace uhal
{

  namespace exception
  {
    //! Exception class to handle the case where the UDP connection timed out.
    ExceptionClass ( UdpTimeout , "Exception class to handle the case where the UDP connection timed out." );
    //! Exception class to handle the case where the error flag was raised in the asynchronous callback system.
    ExceptionClass ( ErrorInUdpCallback , "Exception class to handle the case where the error flag was raised in the asynchronous callback system." );
    //! Exception class to handle a failure to create a UDP socket.
    ExceptionClass ( ErrorAtUdpSocketCreation , "Exception class to handle a failure to create a UDP socket." );
  }

  //! Transport protocol to transfer an IPbus buffer via UDP
  template < typename InnerProtocol >
  class UDP : public InnerProtocol
  {

    public:
      //! Functor class to perform the actual transport, Like this to allow multithreading if desirable.
      class DispatchWorker
      {
        public:
          /**
          Constructor
          @param aUDP a link to the parent
          @param aHostname the target hostname or IP address
          @param aServiceOrPort the target port
          */
          DispatchWorker ( UDP< InnerProtocol > & aUDP , const std::string& aHostname , const std::string& aServiceOrPort );

          /**
          Destructor
          */
          virtual ~DispatchWorker();

          /**
          The functor-function used for launching a thread in multithreaded mode.
          Starts a loop which just monitors the buffer queue and, if there is a buffer waiting,
          pass it to dispatch function for the actual dispatch
          */
          void operator() ();

          /**
          Concrete implementation to send the IPbus buffer to the target, read back the response and call the packing-protocol's validate function
          @param aBuffers the buffer object wrapping the send and recieve buffers that are to be transported
          Can be called directly for single-threaded operation or from the dispatch worker's functor-function for multithreaded operation
          */
          void dispatch ( Buffers* aBuffers );


          void CheckDeadline();

        private:

          //! A reference to the parent of this DispatchWorker
          UDP< InnerProtocol >& mUDP;

          //! The boost::asio::io_service used to create the connections
          boost::shared_ptr< boost::asio::io_service > mIOservice;

          //! A shared pointer to a boost::asio udp socket through which the operation will be performed
          boost::shared_ptr< boost::asio::ip::udp::socket > mSocket;

          //! A shared pointer to a boost::asio udp endpoint stored as a member as UDP as no concept of a connection
          boost::shared_ptr< boost::asio::ip::udp::endpoint > mEndpoint;

          //! Error code for the async callbacks to fill
          boost::system::error_code mErrorCode;

          boost::asio::deadline_timer mDeadlineTimer;

          std::vector<uint8_t> mReplyMemory;

      };

      //! Make the dispatch worker a friend so that it can access our private members
      friend class DispatchWorker;

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


    private:
      // /**
      // Concrete implementation of the synchronization function to block until all buffers have been sent, all replies received and all data validated
      // If multithreaded, block until all buffers have been sent, recieved and validated.
      // If single-threaded, just returns since all buffers are validated in the call to Dispatch().
      // */
      // virtual void Flush( );

      /**
        	Function which dispatch calls when the reply is received to check that the headers are as expected
        	@return whether the returned packet is valid
        */
      virtual bool validate ();

      //! A shared pointer to the DispatchWorker which performs the actual transport
      boost::shared_ptr< DispatchWorker > mDispatchWorker;


      Buffers* getCurrentDispatchBuffers();

      const boost::posix_time::time_duration& getRawTimeoutPeriod();

#ifdef USE_UDP_MULTITHREADED
      //! A shared pointer to a thread for multithreaded running
      boost::shared_ptr< boost::thread > mDispatchThread;
      //! A pointer to an exception which is NULL when operation is good and which is created >>new<< when an exception occurs in the thread since exceptions are not naturally propogated out of the thread
      uhal::exception* mAsynchronousException;
#endif

  };


}

#include "uhal/TemplateDefinitions/ProtocolUDP.hxx"

#endif
