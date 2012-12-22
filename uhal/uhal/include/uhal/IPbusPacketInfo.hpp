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

#ifndef IPbusPacketInfo_hpp
#define IPbusPacketInfo_hpp

// define statements

// C++ includes
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <vector>
#include <deque>
#include <map>

// Custom includes
#include "uhal/ValMem.hpp"
#include "uhal/log/log.hpp"

// Using the uhal namespace
namespace uhal
{

  //! Exception class to handle the case where we were unable to validate the IPbus header. Uses the base uhal::exception implementation of what()
  class IPbusValidationError : public uhal::exception {};


  /**
  	Enumerated type to define the IPbus transaction type.
  	Note that they are stored here as (raw_type << 3) so that the LSL operation does not need to be performed every time a new transaction is created
  */
  enum eIPbusTransactionType
  {
    B_O_T = 0xF8,
    READ = 0x18,
    WRITE = 0x20,
    RMW_BITS = 0x28,
    RMW_SUM = 0x30,
    R_A_I = 0xF0,
    NI_READ = 0x40,
    NI_WRITE = 0x48
  };

  /**
  	Enumerated type to define the IPbus protocol version.
  */
  enum eIPbusProtocolVersion
  {
    IPbus_1_2,
    IPbus_1_3,
    IPbus_1_4,
    IPbus_2_0
  };

  /**
  	Helper function to convert an enumerated type into a formatted string
  	@param aIPbusProtocolVersion The IPbus protocol version we wish to be formatted
  	@return a formatted string representing the enumerated type
  */
  std::string toString ( const eIPbusProtocolVersion& aIPbusProtocolVersion );

  //! A helper struct to allow us to use template specialization rather than if/else statements for types which are known at compile time
  template< eIPbusProtocolVersion IPbusProtocolVersion >
  struct IPbusHeaderHelper
  {
    /**
    	Calculate an IPbus transaction header from individual fields
    	@param aType an enumerated IPbus instruction type
    	@param aWordCount the word count for the IPbus instruction
    	@param aTransactionId the transaction ID field
    	@return an IPbus transaction header
    */
    static uint32_t calculate ( const eIPbusTransactionType& aType , const uint32_t& aWordCount , const uint32_t& aTransactionId );

    /**
    	Extract individual fields from an IPbus header
    	@param aHeader an IPbus transaction header to decode
    	@param aType the enumerated IPbus instruction type of the decoded transaction header
    	@param aWordCount the word count of the decoded transaction header
    	@param aTransactionId the transaction ID of the decoded transaction header
    	@param aResponseGood the response field of the decoded transaction header
    	@return whether extraction succeeded or not
    */
    static bool extract ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood );
  };

}

#include "uhal/TemplateDefinitions/IPbusPacketInfo.hxx"

#endif
