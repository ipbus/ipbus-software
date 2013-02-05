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

#ifndef _uhal_PackingProtocol_IPbusHwAccess_hpp_
#define _uhal_PackingProtocol_IPbusHwAccess_hpp_

#include "uhal/ProtocolInterfaces.hpp"

namespace uhal
{

  //! A concrete class implementing the packing necessary for a direct connection to IPbus hardware
  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  class IPbusHwAccessPackingProtocol : public PackingProtocol
  {
    public:
      /**
      	Constructor
      	@param aMaxSendSize The size of the buffer in the target device for receiving IPbus data packets from uhal
      	@param aMaxReplySize The size of the buffer in the target device for sending IPbus data packets to uhal
      */
      IPbusHwAccessPackingProtocol ( const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize );

      /**
      	Destructor
      */
      virtual ~IPbusHwAccessPackingProtocol();

      /**
      	Concrete implementaion of function to calculate the IPbus header for a particular protocol version
      	@param aType the type of the IPbus transaction
      	@param aWordCount the word count field of the IPbus header
      	@return an IPbus header
      */
      virtual uint32_t calculateIPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount );


      /**
      	Concrete implementaion of function to parse an IPbus header for a particular protocol version
      	@param aHeader an IPbus header to be parsed
      	@param aType return the type of the IPbus transaction
      	@param aWordCount return the word count field of the IPbus header
      	@param aTransactionId return the TransactionId of the IPbus header
      	@param aResponseGood return the response status of the IPbus header
      	@return whether extraction succeeded
      */
      virtual bool extractIPbusHeader ( const uint32_t& aHeader , eIPbusTransactionType& aType , uint32_t& aWordCount , uint32_t& aTransactionId , uint8_t& aResponseGood );

      void Predispatch( );

    private:
      //! The transaction counter which will be incremented in the sent IPbus headers
      uint32_t mTransactionCounter;

  };

}

#include "uhal/TemplateDefinitions/PackingProtocol_IPbusHwAccess.hxx"


#endif
