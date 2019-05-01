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

#include "uhal/IPbusInspector.hpp"


#include "uhal/ProtocolIPbus.hpp"
#include "uhal/log/LogLevels.hpp"
#include "uhal/log/log_inserters.integer.hpp"
#include "uhal/log/log.hpp"


namespace uhal
{

  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  HostToTargetInspector<IPbus_major , IPbus_minor>::HostToTargetInspector( ) :
    mHeader ( 0 ),
    mWordCounter ( 0 ),
    mTransactionId ( 0 ),
    mResponseGood ( 0 ),
    mPacketHeader ( 0 ),
    mPacketCounter ( 0 ),
    mPacketType ( 0 )
  {}


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  HostToTargetInspector<IPbus_major , IPbus_minor>::~HostToTargetInspector( ) {}


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  bool HostToTargetInspector<IPbus_major , IPbus_minor>::analyze ( std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd , const bool& aContinueOnError )
  {
    for ( std::vector<uint32_t>::const_iterator lIt ( aIt ); lIt != aEnd; ++lIt )
    {
      log ( Debug , Integer ( *lIt, IntFmt<hex,fixed>() ) );
    }

    // log ( Notice() , Pointer(&(*aIt)) , " : " , Pointer(&(*aEnd)) , "(", Integer((&(*aEnd)-&(*aIt))*4)  ,")" );
    uint32_t lAddress , lAddend , lAndTerm , lOrTerm ;
    std::vector<uint32_t>::const_iterator lPayloadBegin, lPayloadEnd;

    if ( IPbus_major != 1 )
    {
      mPacketHeader = *aIt++;
      mPacketCounter = ( mPacketHeader>>8 ) &0xFFFF ;
      mPacketType = mPacketHeader&0x0F ;
    }

    switch ( mPacketType )
    {
      case 0:

        if ( IPbus_major != 1 )
        {
          if ( !control_packet_header ( ) )
          {
            return true;
          }
        }

        do
        {
          mHeader = *aIt++;

          if ( ! IPbus< IPbus_major , IPbus_minor >::ExtractHeader (
                 mHeader ,
                 mType ,
                 mWordCounter ,
                 mTransactionId ,
                 mResponseGood )
             )
          {
            log ( Error() , "Unable to parse send header " , Integer ( mHeader, IntFmt<hex,fixed>() ) );

            if ( IPbus_major != 1 )
            {
              if ( ! aContinueOnError )
              {
                aIt--;
                return true;
              }

              log ( Warning() , "Attempting to see if it is because the bad header was, in fact, a packet header" );
              aIt--;
              return this->analyze ( aIt , aEnd );
            }
            else
            {
              return false;
            }
          }

          if ( ( IPbus_major==1 && mResponseGood != 0 ) || ( IPbus_major==2 && mResponseGood != 0xf ) )
          {
            log ( Error(), "Bad InfoCode value of ", Integer ( mResponseGood ), " detected in IPbus transaction request header ", Integer ( mHeader, IntFmt<hex,fixed>() ) );
            return false;
          }

          switch ( mType )
          {
            case B_O_T:
              bot();
              break;
            case NI_READ:
              lAddress = *aIt++;
              ni_read ( lAddress );
              break;
            case READ:
              lAddress = *aIt++;
              read ( lAddress );
              break;
            case CONFIG_SPACE_READ:
              lAddress = *aIt++;
              readConfigurationSpace( lAddress );
              break;
            case NI_WRITE:
              lAddress = *aIt++;
              lPayloadBegin = aIt;
              lPayloadEnd = aIt + mWordCounter;
              ni_write ( lAddress , lPayloadBegin , lPayloadEnd );
              aIt += mWordCounter;
              break;
            case WRITE:
              lAddress = *aIt++;
              lPayloadBegin = aIt;
              lPayloadEnd = aIt + mWordCounter;
              write ( lAddress , lPayloadBegin , lPayloadEnd );
              aIt += mWordCounter;
              break;
            case RMW_SUM:
              lAddress = *aIt++;
              lAddend = *aIt++;
              rmw_sum ( lAddress , lAddend );
              break;
            case RMW_BITS:
              lAddress = *aIt++;
              lAndTerm = *aIt++;
              lOrTerm = *aIt++;
              rmw_bits ( lAddress , lAndTerm , lOrTerm );
              break;
            default:
              unknown_type();
              return false;
          }
        }
        while ( aIt!=aEnd );

        break;
      case 1:
        aIt=aEnd;
        status_packet_header();
        break;
      case 2:
        aIt=aEnd;
        resend_packet_header();
        break;
      default:
        unknown_packet_header( );
        return false;
    }

    return true;
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void HostToTargetInspector<IPbus_major , IPbus_minor>::bot()
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | BOT, transaction ID " , Integer ( mTransactionId ) );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void HostToTargetInspector<IPbus_major , IPbus_minor>::ni_read ( const uint32_t& aAddress )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Non-incrementing read, size " , Integer ( mWordCounter ) , ", transaction ID " , Integer ( mTransactionId ) );
    log ( Notice() , Integer ( aAddress, IntFmt<hex,fixed>() ) , " |  > Address" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void  HostToTargetInspector<IPbus_major , IPbus_minor>::read ( const uint32_t& aAddress )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Incrementing read, size " , Integer ( mWordCounter ) , ", transaction ID " , Integer ( mTransactionId ) );
    log ( Notice() , Integer ( aAddress, IntFmt<hex,fixed>() ) , " |  > Address" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void  HostToTargetInspector<IPbus_major , IPbus_minor>::readConfigurationSpace ( const uint32_t& aAddress )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Incrementing 'configuration space' read, size " , Integer ( mWordCounter ) , ", transaction ID " , Integer ( mTransactionId ) );
    log ( Notice() , Integer ( aAddress, IntFmt<hex,fixed>() ) , " |  > Address" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void HostToTargetInspector<IPbus_major , IPbus_minor>::ni_write ( const uint32_t& aAddress , std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Non-incrementing write, size " , Integer ( mWordCounter ) , ", transaction ID " , Integer ( mTransactionId ) );
    log ( Notice() , Integer ( aAddress, IntFmt<hex,fixed>() ) , " |  > Address" );
    uint32_t lCounter ( 0 );

    while ( aIt != aEnd )
    {
      log ( Notice() , Integer ( *aIt++, IntFmt<hex,fixed>() ) , " |  > Data [" , Integer ( lCounter++ ) , "]" );
    }
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void HostToTargetInspector<IPbus_major , IPbus_minor>::write ( const uint32_t& aAddress , std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Incrementing write, size " , Integer ( mWordCounter ) , ", transaction ID " , Integer ( mTransactionId ) );
    log ( Notice() , Integer ( aAddress, IntFmt<hex,fixed>() ) , " |  > Address" );
    uint32_t lCounter ( 0 );

    while ( aIt != aEnd )
    {
      log ( Notice() , Integer ( *aIt++, IntFmt<hex,fixed>() ) , " |  > Data [" , Integer ( lCounter++ ) , "]" );
    }
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void  HostToTargetInspector<IPbus_major , IPbus_minor>::rmw_sum ( const uint32_t& aAddress , const uint32_t& aAddend )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Read-modify-write sum, transaction ID " , Integer ( mTransactionId ) );
    log ( Notice() , Integer ( aAddress, IntFmt<hex,fixed>() ) , " |  > Address" );
    log ( Notice() , Integer ( aAddend, IntFmt<hex,fixed>() ) , " |  > Addend" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void HostToTargetInspector<IPbus_major , IPbus_minor>::rmw_bits ( const uint32_t& aAddress , const uint32_t& aAndTerm , const uint32_t& aOrTerm )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Read-modify-write bits, transaction ID " , Integer ( mTransactionId ) );
    log ( Notice() , Integer ( aAddress, IntFmt<hex,fixed>() ) , " |  > Address" );
    log ( Notice() , Integer ( aAndTerm, IntFmt<hex,fixed>() ) , " |  > And-term" );
    log ( Notice() , Integer ( aOrTerm, IntFmt<hex,fixed>() ) , " |  > Or-term" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void HostToTargetInspector<IPbus_major , IPbus_minor>::unknown_type()
  {
    log ( Error() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Unknown Transaction Header" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  bool HostToTargetInspector<IPbus_major , IPbus_minor>::control_packet_header ()
  {
    log ( Notice() , Integer ( mPacketHeader , IntFmt<hex,fixed>() ) , " | Control (Instruction) Packet Header , Packet Counter " , Integer ( mPacketCounter ) );
    return true;
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void HostToTargetInspector<IPbus_major , IPbus_minor>::status_packet_header()
  {
    log ( Notice() , Integer ( mPacketHeader , IntFmt<hex,fixed>() ) , " | Status Packet Header" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void HostToTargetInspector<IPbus_major , IPbus_minor>::resend_packet_header()
  {
    log ( Notice() , Integer ( mPacketHeader , IntFmt<hex,fixed>() ) , " | Resend Request Packet Header" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void HostToTargetInspector<IPbus_major , IPbus_minor>:: unknown_packet_header()
  {
    log ( Error() , Integer ( mPacketHeader, IntFmt<hex,fixed>() ) , " | Unknown Packet Header" );
  }



  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  TargetToHostInspector<IPbus_major , IPbus_minor>::
  TargetToHostInspector( ) :
    mHeader ( 0 ),
    mWordCounter ( 0 ),
    mTransactionId ( 0 ),
    mResponseGood ( 0 ),
    mPacketHeader ( 0 ),
    mPacketCounter ( 0 ),
    mPacketType ( 0 )
  {}


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  TargetToHostInspector<IPbus_major , IPbus_minor>::~TargetToHostInspector( ) {}


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  bool TargetToHostInspector<IPbus_major , IPbus_minor>::analyze ( std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd , const bool& aContinueOnError )
  {
    uint32_t lNewValue;
    std::vector<uint32_t>::const_iterator lPayloadBegin, lPayloadEnd;

    if ( IPbus_major != 1 )
    {
      mPacketHeader = *aIt++;
      mPacketCounter = ( mPacketHeader>>8 ) &0xFFFF ;
      mPacketType = mPacketHeader&0x0F ;
    }

    switch ( mPacketType )
    {
      case 0:

        if ( IPbus_major != 1 )
        {
          if ( !control_packet_header ( ) )
          {
            return false;
          }
        }

        do
        {
          mHeader = *aIt++;

          if ( ! IPbus< IPbus_major , IPbus_minor >::ExtractHeader (
                 mHeader ,
                 mType ,
                 mWordCounter ,
                 mTransactionId ,
                 mResponseGood )
             )
          {
            log ( Error() , "Unable to parse reply header " , Integer ( mHeader, IntFmt<hex,fixed>() ) );

            if ( IPbus_major != 1 )
            {
              if ( ! aContinueOnError )
              {
                aIt--;
                return true;
              }

              log ( Warning() , "Attempting to see if it is because the bad header was, in fact, a packet header" );
              aIt--;
              return this->analyze ( aIt , aEnd );
            }
            else
            {
              return false;
            }
          }

          switch ( mType )
          {
            case B_O_T:
              bot();
              break;
            case NI_READ:
              lPayloadBegin = aIt;
              lPayloadEnd = aIt + mWordCounter;
              ni_read ( lPayloadBegin , lPayloadEnd );
              aIt += mWordCounter;
              break;
            case READ:
              lPayloadBegin = aIt;
              lPayloadEnd = aIt + mWordCounter;
              read ( lPayloadBegin , lPayloadEnd );
              aIt += mWordCounter;
              break;
            case NI_WRITE:
              ni_write ();
              break;
            case WRITE:
              write ();
              break;
            case RMW_SUM:
              lNewValue = *aIt++;
              rmw_sum ( lNewValue );
              break;
            case RMW_BITS:
              lNewValue = *aIt++;
              rmw_bits ( lNewValue );
              break;
            default:
              unknown_type();
              return false;
          }
        }
        while ( aIt!=aEnd );

        break;
      case 1:
        aIt=aEnd;
        status_packet_header( );
        break;
      default:
        unknown_packet_header( );
        return false;
    }

    return true;
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void TargetToHostInspector<IPbus_major , IPbus_minor>::bot()
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | BOT, transaction ID " , Integer ( mTransactionId ) );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void TargetToHostInspector<IPbus_major , IPbus_minor>::ni_read ( std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Non-incrementing read, size " , Integer ( mWordCounter ) , ", transaction ID " , Integer ( mTransactionId ) );
    uint32_t lCounter ( 0 );

    while ( aIt != aEnd )
    {
      log ( Notice() , Integer ( *aIt++, IntFmt<hex,fixed>() ) , " |  > Data [" , Integer ( lCounter++ ) , "]" );
    }
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void TargetToHostInspector<IPbus_major , IPbus_minor>::read ( std::vector<uint32_t>::const_iterator& aIt , const std::vector<uint32_t>::const_iterator& aEnd )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Incrementing read, size " , Integer ( mWordCounter ) , ", transaction ID " , Integer ( mTransactionId ) );
    uint32_t lCounter ( 0 );

    while ( aIt != aEnd )
    {
      log ( Notice() , Integer ( *aIt++, IntFmt<hex,fixed>() ) , " |  > Data [" , Integer ( lCounter++ ) , "]" );
    }
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void TargetToHostInspector<IPbus_major , IPbus_minor>::ni_write ( )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Non-incrementing write, size " , Integer ( mWordCounter ) , ", transaction ID " , Integer ( mTransactionId ) );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void TargetToHostInspector<IPbus_major , IPbus_minor>::write ( )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Incrementing write, size " , Integer ( mWordCounter ) , ", transaction ID " , Integer ( mTransactionId ) );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void TargetToHostInspector<IPbus_major , IPbus_minor>::rmw_sum ( const uint32_t& aNewValue )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Read-modify-write sum, transaction ID " , Integer ( mTransactionId ) );
    log ( Notice() , Integer ( aNewValue, IntFmt<hex,fixed>() ) , " |  > Data" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void TargetToHostInspector<IPbus_major , IPbus_minor>::rmw_bits ( const uint32_t& aNewValue )
  {
    log ( Notice() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Read-modify-write bits, transaction ID " , Integer ( mTransactionId ) );
    log ( Notice() , Integer ( aNewValue, IntFmt<hex,fixed>() ) , " |  > Data" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void TargetToHostInspector<IPbus_major , IPbus_minor>::unknown_type()
  {
    log ( Error() , Integer ( mHeader, IntFmt<hex,fixed>() ) , " | Unknown Transaction Header" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  bool TargetToHostInspector<IPbus_major , IPbus_minor>::control_packet_header ()
  {
    log ( Notice() , Integer ( mPacketHeader , IntFmt<hex,fixed>() ) , " | Control (Instruction) Packet Header , Packet Counter " , Integer ( mPacketCounter ) );
    return true;
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void TargetToHostInspector<IPbus_major , IPbus_minor>::status_packet_header()
  {
    log ( Notice() , Integer ( mPacketHeader , IntFmt<hex,fixed>() ) , " | Status Packet Header" );
  }


  template< uint8_t IPbus_major , uint8_t IPbus_minor >
  void TargetToHostInspector<IPbus_major , IPbus_minor>::unknown_packet_header()
  {
    log ( Error() , Integer ( mPacketHeader, IntFmt<hex,fixed>() ) , " | Unknown Packet Header" );
  }


  template class HostToTargetInspector<1, 3>;
  template class HostToTargetInspector<2, 0>;
  template class TargetToHostInspector<1, 3>;
  template class TargetToHostInspector<2, 0>;
}
