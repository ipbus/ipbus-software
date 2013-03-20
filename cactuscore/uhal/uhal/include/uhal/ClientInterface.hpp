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
	@author Marc Magrans De Abril
	@date 2012
*/

#ifndef _uhal_ClientInterface_hpp_
#define _uhal_ClientInterface_hpp_

#include "uhal/log/exception.hpp"
#include "uhal/definitions.hpp"
#include "uhal/ValMem.hpp"

#include "uhal/log/log.hpp"

#include "uhal/Buffers.hpp"

#include "uhal/grammars/URLGrammar.hpp"

#include <vector>
#include <deque>
#include <iostream>

#include <map>

//#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
// #include <uhal/performance.hpp>



namespace uhal
{
  namespace exception
  {
    // //! Exception class to handle the case where an Atomic Transaction was requested but could not be performed. Uses the base uhal::exception implementation of what()
    // class AtomicTransactionSize : public exception {};
    //! Exception class to handle the case where pinging of a client failed.
    ExceptionClass ( PingFailed , "Exception class to handle the case where pinging of a client failed." );

    //! Exception class to handle the case where a masked write was attempted with a data source which has non-zero bits outside the bit-mask's bounds.
    ExceptionClass ( BitsSetWhichAreForbiddenByBitMask , "Exception class to handle the case where a masked write was attempted with a data source which has non-zero bits outside the bit-mask's bounds." );

    //! Exception class to handle the case where we were unable to validate the packet.
    ExceptionClass ( ValidationError , "Exception class to handle the case where we were unable to validate the packet." );
  }

  //! An abstract base class for defining the interface to the various IPbus clients as well as providing the generalized packing funcationality
  class ClientInterface
  {
    protected:
      /**
      	Constructor
      	@param aId the uinique identifier that the client will be given.
      	@param aUri a struct containing the full URI of the target.
      */
      ClientInterface ( const std::string& aId, const URI& aUri );

    private:
      /**
      	Null Constructor
      */
      ClientInterface ();

      /**
      	Copy Constructor
      	@param aClientInterface a ClientInterface to copy
      */
      ClientInterface ( const ClientInterface& aClientInterface );

      /**
      	Assignment operator
      	@param aClientInterface a ClientInterface to copy
      	@return reference to this object for chained assignment
      */
      virtual ClientInterface& operator= ( const ClientInterface& aClientInterface );

    public:
      /**
      	Destructor
      */
      virtual ~ClientInterface();

      /**
      	Return the identifier of the target for this client
      	@return the identifier of the target for this client
      */
      const std::string& id() const;

      // /**
      // Ping the target for this client
      // */
      // void ping();

      /**
      	Return the url of the target for this client
      	@return the url of the target for this client
      */
      std::string uri() const;

      /**
      	Method to dispatch all IPbus packets which are in the queue of IPbusPacketInfo's
      */
      void dispatch ();

      /**
      	A method to modify the timeout period for any pending or future transactions
      	@param aTimeoutPeriod the desired timeout period in milliseconds
      */
      virtual void setTimeoutPeriod ( const uint32_t& aTimeoutPeriod  = 0 ) = 0;

      /**
      	A method to retrieve the timeout period currently being used
      	@return the timeout period currently being used in milliseconds
      */
      virtual uint64_t getTimeoutPeriod() = 0;


      /**
      	Write a single, unmasked word to a register
      	@param aAddr the address of the register to write
      	@param aValue the value to write to the register
      */
      ValHeader write ( const uint32_t& aAddr, const uint32_t& aValue );

      /**
      	Write a single, masked word to a register
      	@param aAddr the address of the register to write
      	@param aValue the value to write to the register
      	@param aMask the mask to apply to the value
      */
      ValHeader write ( const uint32_t& aAddr, const uint32_t& aValue, const uint32_t& aMask );

      /**
      	Write a block of data to a block of registers or a block-write port
      	@param aAddr the address of the register to write
      	@param aValues the values to write to the registers or a block-write port
      	@param aMode whether we are writing to a block of registers (INCREMENTAL) or a block-write port (NON_INCREMENTAL)
      */
      ValHeader writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aValues, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

      /**
      	Read a single, unmasked, unsigned word
      	@param aAddr the address of the register to read
      	@return a Validated Memory which wraps the location to which the reply data is to be written
      */
      ValWord< uint32_t > read ( const uint32_t& aAddr );

      /**
      	Read a single, masked, unsigned word
      	@param aAddr the address of the register to read
      	@param aMask the mask to apply to the value after reading
      	@return a Validated Memory which wraps the location to which the reply data is to be written
      */
      ValWord< uint32_t > read ( const uint32_t& aAddr, const uint32_t& aMask );

      /**
      	Read a block of unsigned data from a block of registers or a block-read port
      	@param aAddr the lowest address in the block of registers or the address of the block-read port
      	@param aSize the number of words to read
      	@param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
      	@return a Validated Memory which wraps the location to which the reply data is to be written
      */
      ValVector< uint32_t > readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

      /**
      	Read the value of a register, apply the AND-term, apply the OR-term, set the register to this new value and return a copy of the new value to the user
      	@param aAddr the address of the register to read, modify, write
      	@param aANDterm the AND-term to apply to existing value in the target register
      	@param aORterm the OR-term to apply to existing value in the target register
      	@return a Validated Memory which wraps the location to which the reply data is to be written
      */
      ValWord< uint32_t > rmw_bits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm );

      /**
      	Read the value of a register, add the addend, set the register to this new value and return a copy of the new value to the user
      	@param aAddr the address of the register to read, modify, write
      	@param aAddend the addend to add to the existing value in the target register
      	@return a Validated Memory which wraps the location to which the reply data is to be written
      */
      ValWord< uint32_t > rmw_sum ( const uint32_t& aAddr , const int32_t& aAddend );

    protected:


      /**
      Send a byte order transaction
      */
      virtual void implementDispatch( ) = 0;

      /**
      Send a byte order transaction
      */
      virtual ValHeader implementBOT( ) = 0;

      /**
      Write a single, unmasked word to a register
      @param aAddr the address of the register to write
      @param aValue the value to write to the register
      */
      virtual ValHeader implementWrite ( const uint32_t& aAddr, const uint32_t& aValue ) = 0;

      /**
      Write a block of data to a block of registers or a block-write port
      @param aAddr the address of the register to write
      @param aValues the values to write to the registers or a block-write port
      @param aMode whether we are writing to a block of registers (INCREMENTAL) or a block-write port (NON_INCREMENTAL)
      */
      virtual ValHeader implementWriteBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aValues, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL ) = 0;

      /**
      Read a single, masked, unsigned word
      @param aAddr the address of the register to read
      @param aMask the mask to apply to the value after reading
      @return a Validated Memory which wraps the location to which the reply data is to be written
      */
      virtual ValWord< uint32_t > implementRead ( const uint32_t& aAddr, const uint32_t& aMask = defs::NOMASK ) = 0;

      /**
      Read a block of unsigned data from a block of registers or a block-read port
      @param aAddr the lowest address in the block of registers or the address of the block-read port
      @param aSize the number of words to read
      @param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
      @return a Validated Memory which wraps the location to which the reply data is to be written
      */
      virtual ValVector< uint32_t > implementReadBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL ) = 0;


      /**
      Read the value of a register, apply the AND-term, apply the OR-term, set the register to this new value and return a copy of the new value to the user
      @param aAddr the address of the register to read, modify, write
      @param aANDterm the AND-term to apply to existing value in the target register
      @param aORterm the OR-term to apply to existing value in the target register
      @return a Validated Memory which wraps the location to which the reply data is to be written
      */
      virtual ValWord< uint32_t > implementRMWbits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm ) = 0;

      /**
      Read the value of a register, add the addend, set the register to this new value and return a copy of the new value to the user
      @param aAddr the address of the register to read, modify, write
      @param aAddend the addend to add to the existing value in the target register
      @return a Validated Memory which wraps the location to which the reply data is to be written
      */
      virtual ValWord< uint32_t > implementRMWsum ( const uint32_t& aAddr , const int32_t& aAddend ) = 0;

      /**
      	Add a preamble to an IPbus buffer
      */
      virtual void preamble( );

      /**
        	Return the size of the preamble
        */
      virtual uint32_t getPreambleSize();

      /**
      	Finalize the buffer before it is transmitted
      */
      virtual void predispatch( );


      std::pair < ValHeader , _ValHeader_* > CreateValHeader();
      std::pair < ValWord<uint32_t> , _ValWord_<uint32_t>* > CreateValWord ( const uint32_t& aValue , const uint32_t& aMask = defs::NOMASK );
      std::pair < ValVector<uint32_t> , _ValVector_<uint32_t>* > CreateValVector ( const uint32_t& aSize );

      /**
        	Function which dispatch calls when the reply is received to check that the headers are as expected
        	@return whether the returned packet is valid
        */
      virtual bool validate ();


    protected:
      /**
      	Function which the dispatch calls when the reply is received to check that the headers are as expected
      	@param aSendBufferStart a pointer to the start of the first word of IPbus data which was sent (i.e. with no preamble)
      	@param aSendBufferEnd a pointer to the end of the last word of IPbus data which was sent
      	@param aReplyStartIt an iterator to the start of the list of memory locations in to which the reply was written
      	@param aReplyEndIt an iterator to the end (one past last valid entry) of the list of memory locations in to which the reply was written
      	@return whether the returned IPbus packet is valid
      */
      virtual bool validate ( uint8_t* aSendBufferStart ,
                              uint8_t* aSendBufferEnd ,
                              std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt ,
                              std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt ) = 0;

      virtual void dispatchExceptionHandler();

    protected:
      //! A MutEx lock used to make sure the access functions are thread safe
      boost::mutex mMutex;

      //! A memory pool of buffers which will be dispatched
      std::deque < Buffers > mBuffers;

    protected:
      //! A pointer to a buffer-wrapper object
      std::deque < Buffers >::iterator mCurrentBuffers;

      //! A queue of buffers that that have been dispatched and for which we are waiting for the transportation to complete and for them to be validated
      std::deque < Buffers* > mDispatchedBuffers;

    protected:
      //! the identifier of the target for this client
      std::string mId;
      //! a struct containing the full URI of the target for this client
      URI mUri;

      void NextFillingBuffer ();
      void CreateFillingBuffer ();

      virtual uint32_t getMaxSendSize() = 0;
      virtual uint32_t getMaxReplySize() = 0;

  };



}

#endif

