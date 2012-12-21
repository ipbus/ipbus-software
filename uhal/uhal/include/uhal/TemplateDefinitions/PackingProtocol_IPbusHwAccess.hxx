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
