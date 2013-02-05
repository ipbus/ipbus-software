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


#define ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary 8

namespace uhal
{
  // ----------------------------------------------------------------------------------------------------------------

  //! A class to directly access locally-connected devices via IPbus over UDP
  template< uint8_t IPbus_major , uint8_t IPbus_minor >

  IPBusUDPClient< IPbus_major , IPbus_minor >::IPBusUDPClient ( const std::string& aId , const URI& aUri ) :
    ClientInterface ( aId , aUri ),
    mPackingProtocol ( mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }




  template< uint8_t IPbus_major , uint8_t IPbus_minor >

  IPBusUDPClient< IPbus_major , IPbus_minor >::IPBusUDPClient ( const IPBusUDPClient< IPbus_major , IPbus_minor >& aIPBusUDPClient ) :
    ClientInterface ( aIPBusUDPClient ),
    mPackingProtocol ( mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }




  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  std::string IPBusUDPClient< IPbus_major , IPbus_minor >::description()
  {
    logging();
    std::stringstream lStr;
    lStr << "Direct access to hardware via UDP, using IPbus version "<<IPbus_major<<"."<<IPbus_minor;
    return lStr.str();
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  PackingProtocol& IPBusUDPClient< IPbus_major , IPbus_minor >::getPackingProtocol()
  {
    logging();
    return mPackingProtocol;
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  TransportProtocol& IPBusUDPClient< IPbus_major , IPbus_minor >::getTransportProtocol()
  {
    return mTransportProtocol;
    logging();
  }







  //! A class to directly access locally-connected devices via IPbus over TCP
  template< uint8_t IPbus_major , uint8_t IPbus_minor >

  IPBusTCPClient< IPbus_major , IPbus_minor >::IPBusTCPClient ( const std::string& aId , const URI& aUri ) :
    ClientInterface ( aId , aUri ),
    mPackingProtocol ( mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }



  template< uint8_t IPbus_major , uint8_t IPbus_minor >

  IPBusTCPClient< IPbus_major , IPbus_minor >::IPBusTCPClient ( const IPBusTCPClient< IPbus_major , IPbus_minor > &aIPBusTCPClient ) :
    ClientInterface ( aIPBusTCPClient ),
    mPackingProtocol ( mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }



  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  std::string IPBusTCPClient< IPbus_major , IPbus_minor >::description()
  {
    logging();
    std::stringstream lStr;
    lStr << "Direct access to hardware via TCP, using IPbus version "<<IPbus_major<<"."<<IPbus_minor;
    return lStr.str();
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  PackingProtocol& IPBusTCPClient< IPbus_major , IPbus_minor >::getPackingProtocol()
  {
    logging();
    return mPackingProtocol;
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  TransportProtocol& IPBusTCPClient< IPbus_major , IPbus_minor >::getTransportProtocol()
  {
    logging();
    return mTransportProtocol;
  }



















  template< uint8_t IPbus_major , uint8_t IPbus_minor >

  ControlHubClient< IPbus_major , IPbus_minor >::ControlHubClient ( const std::string& aId , const URI& aUri ) :
    ClientInterface ( aId , aUri ),
    mTargetId ( ExtractTargetID ( aUri ) ),
    mPackingProtocol ( mTargetId.first , mTargetId.second , mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }




  template< uint8_t IPbus_major , uint8_t IPbus_minor >

  ControlHubClient< IPbus_major , IPbus_minor >::ControlHubClient ( const ControlHubClient< IPbus_major , IPbus_minor > &aControlHubClient ) :
    ClientInterface ( aControlHubClient ),
    mTargetId ( aControlHubClient.mTargetId ),
    mPackingProtocol ( mTargetId.first , mTargetId.second , mMaxPacketLength , mMaxPacketLength - ThisIsAHackToPatchFirmwareUndersizedPacketHandlingProblemAndShouldNotBeNeccessary ),
    mTransportProtocol ( mUri.mHostname , mUri.mPort , mDefaultTimeoutPeriod )
  {
    logging();
    Link ( mTransportProtocol , mPackingProtocol );
  }




  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  std::string ControlHubClient< IPbus_major , IPbus_minor >::description()
  {
    logging();
    std::stringstream lStr;
    lStr << "Hardware access via the Control Hub, using IPbus version "<<IPbus_major<<"."<<IPbus_minor;
    return lStr.str();
  }



  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  PackingProtocol& ControlHubClient< IPbus_major , IPbus_minor >::getPackingProtocol()
  {
    logging();
    return mPackingProtocol;
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  TransportProtocol& ControlHubClient< IPbus_major , IPbus_minor >::getTransportProtocol()
  {
    logging();
    return mTransportProtocol;
  }















}

