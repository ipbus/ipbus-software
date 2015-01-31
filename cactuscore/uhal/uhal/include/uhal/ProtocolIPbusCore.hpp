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

#ifndef _uhal_ProtocolIPbusCore_hpp_
#define _uhal_ProtocolIPbusCore_hpp_

#include <deque>

#include "boost/date_time/posix_time/posix_time_duration.hpp"

#include "uhal/ClientInterface.hpp"

namespace uhal
{

  /**
  	Enumerated type to define the IPbus transaction type.
  	Note that they are stored here as (raw_type << 3) so that the LSL operation does not need to be performed every time a new transaction is created
  */
  enum eIPbusTransactionType
  {
    B_O_T,
    READ,
    WRITE,
    RMW_BITS,
    RMW_SUM,
    R_A_I,
    NI_READ,
    NI_WRITE
  };
}

/**
  Streaming operator for formatting objects of the uhal::eIPbusTransactionType
  @param aStr a stream to which to append the formatted data
  @param aIPbusTransactionType an enum object to be formatted
  @return a reference to the stream for chaining stram calls
*/
std::ostream& operator<< ( std::ostream& aStr , const uhal::eIPbusTransactionType& aIPbusTransactionType );


namespace uhal
{
  namespace exception
  {
    //! Exception class to handle the case where the IPbus header could not be parsed.
    ExceptionClass ( IPbusCoreUnparsableTransactionHeader , "Exception class to handle the case where the IPbus transaction header could not be parsed." )
    //     ExceptionClass ( IPbusCoreZeroSizeTransaction , "Exception class to handle the case where a transaction of size zero was requested." )
    //! Exception class to handle the case where the IPbus transaction header response code indicated an error.
    ExceptionClass ( IPbusCoreResponseCodeSet , "Exception class to handle the case where the IPbus transaction header response code indicated an error." )
    //! Exception class to handle the case where the IPbus transaction type came back as the wrong type.
    ExceptionClass ( IPbusTransactionTypeIncorrect , "Exception class to handle the case where the IPbus transaction type came back as the wrong type." )
    //! Exception class to handle the case where the IPbus transaction id came back as incorrect.
    ExceptionClass ( IPbusTransactionIdIncorrect , "Exception class to handle the case where the IPbus transaction id came back as incorrect." )

  }


  //! A class providing the core IPbus packing functionality
  class IPbusCore : public ClientInterface
  {

    public:
      /**
      	Default constructor
      	@param aId the uinique identifier that the client will be given.
      	@param aUri a struct containing the full URI of the target.
       	@param aMaxSendSize The size of the buffer in the target device for receiving data packets from uhal
      	@param aMaxReplySize The size of the buffer in the target device for sending data packets to uhal
      	@param aTimeoutPeriod the default timeout period (can be changed later)
      */
      IPbusCore ( const std::string& aId, const URI& aUri , const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize , const boost::posix_time::time_duration& aTimeoutPeriod );

      /**
      	Destructor
      */
      virtual ~IPbusCore();

      //       /*
      //       	A method to modify the timeout period for any pending or future transactions
      //       	@param aTimeoutPeriod the desired timeout period in milliseconds
      //       */
      //       virtual void setTimeoutPeriod ( const uint32_t& aTimeoutPeriod  = 0 ) ;
      //
      //       /*
      //       	A method to retrieve the timeout period currently being used
      //       	@return the timeout period currently being used in milliseconds
      //       */
      //       virtual uint64_t getTimeoutPeriod();

    protected:

      /**
        Return the maximum size to be sent based on the buffer size in the target
        @return the maximum size to be sent
      */
      virtual uint32_t getMaxSendSize();
      /**
        Return the maximum size of reply packet based on the buffer size in the target
        @return the maximum size of reply packet
      */
      virtual uint32_t getMaxReplySize();

      /**
      Send a byte order transaction
      */
      virtual ValHeader implementBOT( );

      /**
      Write a single, unmasked word to a register
      @param aAddr the address of the register to write
      @param aValue the value to write to the register
      */
      virtual ValHeader implementWrite ( const uint32_t& aAddr, const uint32_t& aValue );

      /**
      Write a block of data to a block of registers or a block-write port
      @param aAddr the address of the register to write
      @param aValues the values to write to the registers or a block-write port
      @param aMode whether we are writing to a block of registers (INCREMENTAL) or a block-write port (NON_INCREMENTAL)
      */
      virtual ValHeader implementWriteBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aValues, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

      /**
      Read a single, masked, unsigned word
      @param aAddr the address of the register to read
      @param aMask the mask to apply to the value after reading
      @return a Validated Memory which wraps the location to which the reply data is to be written
      */
      virtual ValWord< uint32_t > implementRead ( const uint32_t& aAddr, const uint32_t& aMask = defs::NOMASK );

      /**
      Read a block of unsigned data from a block of registers or a block-read port
      @param aAddr the lowest address in the block of registers or the address of the block-read port
      @param aSize the number of words to read
      @param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
      @return a Validated Memory which wraps the location to which the reply data is to be written
      */
      virtual ValVector< uint32_t > implementReadBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );


      /**
      Read the value of a register, apply the AND-term, apply the OR-term, set the register to this new value and return a copy of the new value to the user
      @param aAddr the address of the register to read, modify, write
      @param aANDterm the AND-term to apply to existing value in the target register
      @param aORterm the OR-term to apply to existing value in the target register
      @return a Validated Memory which wraps the location to which the reply data is to be written
      */
      virtual ValWord< uint32_t > implementRMWbits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm );

      /**
      Read the value of a register, add the addend, set the register to this new value and return a copy of the new value to the user
      @param aAddr the address of the register to read, modify, write
      @param aAddend the addend to add to the existing value in the target register
      @return a Validated Memory which wraps the location to which the reply data is to be written
      */
      virtual ValWord< uint32_t > implementRMWsum ( const uint32_t& aAddr , const int32_t& aAddend );


    protected:
      /**
      	Function which the transport protocol calls when the IPbus reply is received to check that the headers are as expected
      	@param aSendBufferStart a pointer to the start of the first word of IPbus data which was sent (i.e. with no preamble)
      	@param aSendBufferEnd a pointer to the end of the last word of IPbus data which was sent
      	@param aReplyStartIt an iterator to the start of the list of memory locations in to which the reply was written
      	@param aReplyEndIt an iterator to the end (one past last valid entry) of the list of memory locations in to which the reply was written
      	@return whether the returned IPbus packet is valid
      */
      virtual  exception::exception* validate ( uint8_t* aSendBufferStart ,
          uint8_t* aSendBufferEnd ,
          std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt ,
          std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt );

      /**
      Abstract interface of function to calculate the IPbus header for a particular protocol version
      @param aType the type of the IPbus transaction
      @param aWordCount the word count field of the IPbus header
      @param aTransactionId the TransactionId of the IPbus header
      @param aInfoCode the response status of the transaction      
      @return an IPbus header
      */
      virtual uint32_t implementCalculateHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount , const uint32_t& aTransactionId , const uint8_t& aInfoCode ) = 0;

      /**
      Abstract interface of function to parse an IPbus header for a particular protocol version
      @param aHeader an IPbus header to be parsed
      @param aType return the type of the IPbus transaction
      @param aWordCount return the word count field of the IPbus header
      @param aTransactionId return the TransactionId of the IPbus header
      @param aInfoCode return the response status of the IPbus header
      @return whether extraction succeeded
      */
      virtual bool implementExtractHeader ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aInfoCode ) = 0;

      //! Returns the InfoCode for request transactions in this IPbus version.
      virtual uint8_t requestTransactionInfoCode() const = 0;

      //! Returns the maximum value of the word count in the transaction header, for each IPbus version
      virtual uint32_t getMaxTransactionWordCount() const = 0;

      //! Function which is called when an exception is thrown
      virtual void dispatchExceptionHandler();

    private:

      //! The transaction counter which will be incremented in the sent IPbus headers
      uint32_t mTransactionCounter;

      //! The size of the buffer in the target device for receiving IPbus data packets from uhal
      uint32_t mMaxSendSize;

      //! The size of the buffer in the target device for sending IPbus data packets to uhal
      uint32_t mMaxReplySize;


  };



}


#endif
