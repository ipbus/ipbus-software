
#define ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary 8

namespace uhal
{
  // ----------------------------------------------------------------------------------------------------------------

  //! A class to directly access locally-connected devices via IPbus over UDP
  template< eIPbusProtocolVersion IPbusProtocolVersion >

  IPBusUDPClient< IPbusProtocolVersion >::IPBusUDPClient ( const std::string& aId , const URI& aUri ) :
    ClientInterface ( aId , aUri ),
    mPackingProtocol ( mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }




  template< eIPbusProtocolVersion IPbusProtocolVersion >

  IPBusUDPClient< IPbusProtocolVersion >::IPBusUDPClient ( const IPBusUDPClient< IPbusProtocolVersion >& aIPBusUDPClient ) :
    ClientInterface ( aIPBusUDPClient ),
    mPackingProtocol ( mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }




  template< eIPbusProtocolVersion IPbusProtocolVersion >
  std::string IPBusUDPClient< IPbusProtocolVersion >::description()
  {
    logging();
    return "Direct access to hardware via UDP, using " + toString ( IPbusProtocolVersion );
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  PackingProtocol& IPBusUDPClient< IPbusProtocolVersion >::getPackingProtocol()
  {
    logging();
    return mPackingProtocol;
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  TransportProtocol& IPBusUDPClient< IPbusProtocolVersion >::getTransportProtocol()
  {
    return mTransportProtocol;
    logging();
  }







  //! A class to directly access locally-connected devices via IPbus over TCP
  template< eIPbusProtocolVersion IPbusProtocolVersion >

  IPBusTCPClient< IPbusProtocolVersion >::IPBusTCPClient ( const std::string& aId , const URI& aUri ) :
    ClientInterface ( aId , aUri ),
    mPackingProtocol ( mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }



  template< eIPbusProtocolVersion IPbusProtocolVersion >

  IPBusTCPClient< IPbusProtocolVersion >::IPBusTCPClient ( const IPBusTCPClient< IPbusProtocolVersion > &aIPBusTCPClient ) :
    ClientInterface ( aIPBusTCPClient ),
    mPackingProtocol ( mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }



  template< eIPbusProtocolVersion IPbusProtocolVersion >
  std::string IPBusTCPClient< IPbusProtocolVersion >::description()
  {
    logging();
    return "Direct access to hardware via TCP, using " + toString ( IPbusProtocolVersion );
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  PackingProtocol& IPBusTCPClient< IPbusProtocolVersion >::getPackingProtocol()
  {
    logging();
    return mPackingProtocol;
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  TransportProtocol& IPBusTCPClient< IPbusProtocolVersion >::getTransportProtocol()
  {
    logging();
    return mTransportProtocol;
  }



















  template< eIPbusProtocolVersion IPbusProtocolVersion >

  ControlHubClient< IPbusProtocolVersion >::ControlHubClient ( const std::string& aId , const URI& aUri ) :
    ClientInterface ( aId , aUri ),
    mTargetId ( ExtractTargetID ( aUri ) ),
    mPackingProtocol ( mTargetId.first , mTargetId.second , mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }




  template< eIPbusProtocolVersion IPbusProtocolVersion >

  ControlHubClient< IPbusProtocolVersion >::ControlHubClient ( const ControlHubClient< IPbusProtocolVersion > &aControlHubClient ) :
    ClientInterface ( aControlHubClient ),
    mTargetId ( aControlHubClient.mTargetId ),
    mPackingProtocol ( mTargetId.first , mTargetId.second , mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }




  template< eIPbusProtocolVersion IPbusProtocolVersion >
  std::string ControlHubClient< IPbusProtocolVersion >::description()
  {
    logging();
    return "Hardware access via the Control Hub, using " + toString ( IPbusProtocolVersion );
  }



  template< eIPbusProtocolVersion IPbusProtocolVersion >
  PackingProtocol& ControlHubClient< IPbusProtocolVersion >::getPackingProtocol()
  {
    logging();
    return mPackingProtocol;
  }


  template< eIPbusProtocolVersion IPbusProtocolVersion >
  TransportProtocol& ControlHubClient< IPbusProtocolVersion >::getTransportProtocol()
  {
    logging();
    return mTransportProtocol;
  }















}

