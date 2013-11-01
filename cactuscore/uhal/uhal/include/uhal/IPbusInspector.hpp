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
	@date 2013
*/

#ifndef IPbusInspector_hpp
#define IPbusInspector_hpp

//#include "uhal/uhal.hpp"
#include "uhal/log/log.hpp"
#include "uhal/ProtocolIPbus.hpp"

// Using the uhal namespace
namespace uhal
{

  /*  namespace exception
  {
    // Exception class to handle the case where we are unable to parse a badly formed IPbus header.
    ExceptionClass ( UnableToParseHeader , "Exception class to handle the case where we are unable to parse a badly formed IPbus header." );
    ExceptionClass ( IllegalPacketHeader , "Exception class to handle the case where an illegal packet header is received." );
  }
  */

  /**
    Helper class to decode IPbus packets as passed from the Client to the Target
  */
  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  class HostToTargetInspector
  {
    public:
      /**
        Constructor
      */
      HostToTargetInspector( );
      /**
        Destructor
      */
      virtual ~HostToTargetInspector( );

    protected:
      //! The current raw transaction header
      uint32_t mHeader;
      //! The current transaction type
      eIPbusTransactionType mType;
      //! The current word count
      uint32_t mWordCounter;
      //! The current transaction id
      uint32_t mTransactionId;
      //! The current error code/flag
      uint8_t mResponseGood;

      //! The current raw IPbus2 packet header
      uint32_t mPacketHeader;
      //! The current IPbus2 packet counter
      uint32_t mPacketCounter;
      //! The current IPbus2 packet type
      uint32_t mPacketType;

    public:
      /**
        Analyse an IPbus packet held as a vector of uint32_t's
        @param aIt Starting iterator of the IPbus packet
        @param aEnd End point of the IPbus packet
        @param aContinueOnError set the behaviour after errors
        @return success or failure
      */
      bool analyze ( std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd , const bool& aContinueOnError = true );
    protected:
      virtual void bot();
      virtual void ni_read ( const uint32_t& aAddress );

      virtual void read ( const uint32_t& aAddress );

      virtual void ni_write ( const uint32_t& aAddress , std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd );
      virtual void write ( const uint32_t& aAddress , std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd );
      virtual void rmw_sum ( const uint32_t& aAddress , const uint32_t& aAddend );
      virtual void rmw_bits ( const uint32_t& aAddress , const uint32_t& aAndTerm , const uint32_t& aOrTerm );
      virtual void unknown_type();

      virtual bool control_packet_header ();

      virtual void status_packet_header();

      virtual void resend_packet_header();

      virtual void unknown_packet_header();

  };





  /**
    Helper class to decode IPbus packets as passed from the Target to the Client
  */
  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  class TargetToHostInspector
  {
    public:
      /**
        Constructor
      */
      TargetToHostInspector( );
      /**
        Destructor
      */
      virtual ~TargetToHostInspector( );

    protected:
      //! The current raw transaction header
      uint32_t mHeader;
      //! The current transaction type
      eIPbusTransactionType mType;
      //! The current word count
      uint32_t mWordCounter;
      //! The current transaction id
      uint32_t mTransactionId;
      //! The current error code/flag
      uint8_t mResponseGood;

      //! The current raw IPbus2 packet header
      uint32_t mPacketHeader;
      //! The current IPbus2 packet counter
      uint32_t mPacketCounter;
      //! The current IPbus2 packet type
      uint32_t mPacketType;

    public:

      /**
        Analyse an IPbus packet held as a vector of uint32_t's
        @param aIt Starting iterator of the IPbus packet
        @param aEnd End point of the IPbus packet
        @param aContinueOnError set the behaviour after errors
        @return success or failure
      */
      bool analyze ( std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd , const bool& aContinueOnError = true );

    protected:
      virtual void bot();

      virtual void ni_read ( std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd );

      virtual void read ( std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd );

      virtual void ni_write ( );

      virtual void write ( );

      virtual void rmw_sum ( const uint32_t& aNewValue );

      virtual void rmw_bits ( const uint32_t& aNewValue );

      virtual void unknown_type();

      virtual bool control_packet_header ();

      virtual void status_packet_header();

      virtual void unknown_packet_header();

  };
}
#include "uhal/TemplateDefinitions/IPbusInspector.hxx"

#endif
