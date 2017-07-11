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

#ifndef _uhal_ProtocolPCIe_hpp_
#define _uhal_ProtocolPCIe_hpp_

#include "uhal/ClientInterface.hpp"
#include "uhal/log/exception.hpp"
#include "uhal/log/log.hpp"

#include <iostream>
#include <iomanip>

#include <boost/shared_ptr.hpp>

#include <string>

namespace uhal
{

  namespace exception
  {
    //! Exceptions get defined here
    // UHAL_DEFINE_EXCEPTION_CLASS ( UdpTimeout , "Exception class to handle the case where the PCIe connection timed out." )
  }

  //! Transport protocol to transfer an IPbus buffer via PCIe
  template < typename InnerProtocol >
  class PCIe : public InnerProtocol
  {

    public:
      //! Functor class to perform the actual transport, Like this to allow multithreading if desirable.

      /**
      	Constructor
      	@param aId the uinique identifier that the client will be given.
      	@param aUri a struct containing the full URI of the target.
      */
      PCIe ( const std::string& aId, const URI& aUri );

      /**
        Copy Constructor
        This creates a new socket, dispatch queue, dispatch thread, etc. which connects to the same target ip/port
        @param aPCIe a PCIe-protocol object to copy
      */
      PCIe ( const PCIe& aPCIe );

      /**
       Assignment operator
       This reassigns the endpoint, closes the existing socket and cleans up the buffers, etc. On the next call which requires the socket, it will be reopened with the new endpoint.
       @param aPCIe a PCIe-protocol object to copy
       @return reference to the current object to allow chaining of assignments
            */
      PCIe& operator= ( const PCIe& aPCIe );


      /**
      	Destructor
      */
      virtual ~PCIe();

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
      /**
        Function which tidies up this protocol layer in the event of an exception
       */
      virtual void dispatchExceptionHandler();


    private:
      /**
        Initialize performing the next PCIe write operation
      */
      void write ( );

      /**
        Initialize performing the next PCIe read operation
      */
      void read ( );

    private:

      //! The send operation currently in progress
      boost::shared_ptr< Buffers > mDispatchBuffers;
      //! The receive operation currently in progress or the next to be done
      boost::shared_ptr< Buffers > mReplyBuffers;

  };


}

#include "uhal/TemplateDefinitions/ProtocolPCIe.hxx"

#endif
