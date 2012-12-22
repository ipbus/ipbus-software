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

// #include <uhal/performance.hpp>

namespace uhal
{

  template< eIPbusProtocolVersion IPbusProtocolVersion >

  IPbusHwAccessPackingProtocol<  IPbusProtocolVersion >::IPbusHwAccessPackingProtocol ( const uint32_t& aMaxSendSize , const uint32_t& aMaxReplySize ) :
    PackingProtocol ( aMaxSendSize<<2 , aMaxReplySize<<2 ),
    mTransactionCounter ( 0 )
  {
    logging();
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  IPbusHwAccessPackingProtocol<  IPbusProtocolVersion >::~IPbusHwAccessPackingProtocol()
  {
    logging();
  }

  template< eIPbusProtocolVersion IPbusProtocolVersion >
  uint32_t IPbusHwAccessPackingProtocol<  IPbusProtocolVersion >::calculateIPbusHeader ( const eIPbusTransactionType& aType , const uint32_t& aWordCount )
  {
    logging();
    return IPbusHeaderHelper<IPbusProtocolVersion>::calculate ( aType , aWordCount , mTransactionCounter++ );
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  bool IPbusHwAccessPackingProtocol<  IPbusProtocolVersion >::extractIPbusHeader (
    const uint32_t& aHeader ,
    eIPbusTransactionType& aType ,
    uint32_t& aWordCount ,
    uint32_t& aTransactionId ,
    uint8_t& aResponseGood )
  {
    logging();
    return IPbusHeaderHelper<IPbusProtocolVersion>::extract ( aHeader , aType , aWordCount , aTransactionId , aResponseGood );
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  void IPbusHwAccessPackingProtocol<  IPbusProtocolVersion >::Predispatch( )
  {
    logging();
    uint32_t lWords ( mCurrentBuffers->sendCounter()  >> 2 );
    log ( Debug() , "Predispatch size : " , Integer ( lWords ) , " words " );

    if ( lWords < 8 )
    {
      log ( Debug() , "Adding " , Integer ( 8 - lWords ) , " words of padding." );

      for ( ; lWords != 8 ; ++lWords )
      {
        this->Padding();
      }
    }
  }

}
