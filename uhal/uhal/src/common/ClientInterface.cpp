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

#include "uhal/ClientInterface.hpp"

#include "uhal/Utilities.hpp"
#include <sstream>

namespace uhal
{

  ClientInterface::ClientInterface ( const std::string& aId, const URI& aUri ) :
    mId ( aId ),
    mUri ( aUri )
  {
    logging();
  }



  ClientInterface::ClientInterface ( )
  {
    logging();
  }



  ClientInterface::ClientInterface ( const ClientInterface& aClientInterface ) :
    mId ( aClientInterface.mId ),
    mUri ( aClientInterface.mUri )
  {
    logging();
  }


  ClientInterface& ClientInterface::operator= ( const ClientInterface& aClientInterface )
  {
    logging();
    mId  = aClientInterface.mId;
    mUri = aClientInterface.mUri;
    return *this;
  }


  ClientInterface::~ClientInterface()
  {
    logging();
  }

  const std::string& ClientInterface::id() const
  {
    logging();
    return mId;
  }


  // void ClientInterface::ping()
  // {
  // try
  // {
  // std::string lInstruction ( "ping -q -c 1 " + mUri.mHostname + " &> /dev/null" );
  // log ( Info() , "Pinging " ,  Quote ( mId ) , " with instruction : " , lInstruction );
  // //Cant use ICMP here because it requires raw socket (and hence superuser) access, so use system PING instead
  // int lPingStatus = system ( lInstruction.c_str() );

  // if ( WEXITSTATUS ( lPingStatus ) )
  // {
  // log ( Error() , "Pinging " , Quote ( mId ) , " at address " , Quote( mUri.mHostname ) , " returned exit status ", Integer ( WEXITSTATUS ( lPingStatus ) ) );
  // throw exception::// PingFailed();
  // }
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }


  std::string ClientInterface::uri() const
  {
    logging();
    std::stringstream lReturn;
    // url is always of the form "protocol://hostname:port"
    lReturn << mUri.mProtocol << "://" << mUri.mHostname << ":" << mUri.mPort;

    // there is sometimes a path
    if ( mUri.mPath != "" )
    {
      lReturn << "/" << mUri.mPath;
    }

    // there is sometimes a filename extension
    if ( mUri.mExtension != "" )
    {
      lReturn << "." << mUri.mExtension;
    }

    // there are sometimes arguments
    if ( mUri.mArguments.size() )
    {
      lReturn << "?";
      uhal::NameValuePairVectorType::const_iterator lIt = mUri.mArguments.begin();

      while ( true )
      {
        lReturn << lIt->first << "=" << lIt->second;

        if ( ++lIt == mUri.mArguments.end() )
        {
          break;
        }

        lReturn << "&";
      }
    }

    return lReturn.str();
  }


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValHeader ClientInterface::write ( const uint32_t& aAddr, const uint32_t& aSource )
  {
    logging();
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    return getPackingProtocol().write ( aAddr , aSource );
  }

  ValHeader ClientInterface::write ( const uint32_t& aAddr, const uint32_t& aSource, const uint32_t& aMask )
  {
    logging();
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    uint32_t lShiftSize ( utilities::TrailingRightBits ( aMask ) );
    uint32_t lBitShiftedSource ( aSource << lShiftSize );

    if ( ( lBitShiftedSource >> lShiftSize ) != aSource )
    {
      log ( Error() , "Source data (" , Integer ( aSource , IntFmt<hex,fixed>() ) , ") has bits which would be shifted outside the register " );
      throw exception::BitsSetWhichAreForbiddenByBitMask();
    }

    uint32_t lOverlap ( lBitShiftedSource & ~aMask );

    if ( lOverlap )
    {
      log ( Error() , "Source data (" , Integer ( aSource , IntFmt<hex,fixed>() ) , ")"
            " has the following bits set outside the bounds allowed by the bit-mask ( ", Integer ( aSource , IntFmt<hex,fixed>() ) , ") : " ,
            Integer ( lOverlap , IntFmt<hex,fixed>() )
          );
      throw exception::BitsSetWhichAreForbiddenByBitMask();
    }

    return ( ValHeader ) ( getPackingProtocol().rmw_bits ( aAddr , ~aMask , lBitShiftedSource & aMask ) );
  }

  ValHeader ClientInterface::writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& aSource, const defs::BlockReadWriteMode& aMode )
  {
    logging();
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    return getPackingProtocol().writeBlock ( aAddr, aSource, aMode );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr )
  {
    logging();
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    return getPackingProtocol().read ( aAddr );
  }

  ValWord< uint32_t > ClientInterface::read ( const uint32_t& aAddr, const uint32_t& aMask )
  {
    logging();
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    return getPackingProtocol().read ( aAddr, aMask );
  }

  ValVector< uint32_t > ClientInterface::readBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
  {
    logging();
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    return getPackingProtocol().readBlock ( aAddr, aSize, aMode );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  // //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // ValWord< int32_t > ClientInterface::readSigned ( const uint32_t& aAddr )
  // {
  // try
  // {
  // return getPackingProtocol().readSigned ( aAddr );
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }

  // ValWord< int32_t > ClientInterface::readSigned ( const uint32_t& aAddr, const uint32_t& aMask )
  // {
  // try
  // {
  // return getPackingProtocol().readSigned ( aAddr , aMask );
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }

  // ValVector< int32_t > ClientInterface::readBlockSigned ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
  // {
  // try
  // {
  // return getPackingProtocol().readBlockSigned ( aAddr, aSize, aMode );
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.throw r;
  // }
  // catch ( const std::exception& aExc )
  // {
  // throw // StdException ( aExc );
  // }
  // }
  // //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  // //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // ValVector< uint32_t > ClientInterface::readReservedAddressInfo ()
  // {
  // try
  // {
  // return getPackingProtocol().readReservedAddressInfo ();
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.rethrowFrom( ThisLocation() );
  // }
  // catch ( const std::exception& aExc )
  // {
  // log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );	// uhal::StdException lExc( aExc );
  // lExc.throwFrom( ThisLocation() );
  // }
  // }
  // //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > ClientInterface::rmw_bits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm )
  {
    logging();
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    return getPackingProtocol().rmw_bits ( aAddr , aANDterm , aORterm );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ValWord< uint32_t > ClientInterface::rmw_sum ( const uint32_t& aAddr , const int32_t& aAddend )
  {
    logging();
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    return getPackingProtocol().rmw_sum ( aAddr , aAddend );
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  void ClientInterface::dispatch ()
  {
    logging();
    log ( Debug() , "Manual dispatch" );
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    getPackingProtocol().Dispatch();
  }
  //-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  void ClientInterface::setTimeoutPeriod ( const uint32_t& aTimeoutPeriod )
  {
    logging();
    boost::lock_guard<boost::mutex> lLock ( mMutex );

    if ( aTimeoutPeriod == 0 )
    {
      getTransportProtocol().setTimeoutPeriod ( boost::posix_time::pos_infin );
    }
    else
    {
      getTransportProtocol().setTimeoutPeriod ( boost::posix_time::milliseconds ( aTimeoutPeriod ) );
    }
  }

  uint64_t ClientInterface::getTimeoutPeriod()
  {
    logging();
    boost::lock_guard<boost::mutex> lLock ( mMutex );
    return getTransportProtocol().getTimeoutPeriod().total_milliseconds();
  }

}
