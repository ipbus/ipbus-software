
#include "uhal/uhal.hpp"
#include "uhal/log/log.hpp"

#include "uhal/IPbusInspector.hpp"

#include <string>
#include <sstream>
#include <fstream>

#include <iomanip>
#include <arpa/inet.h>


using namespace uhal;


// --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

struct ethernet_frame
{
  private:
    uint8_t d_MAC[6];  //0-5
    uint8_t s_MAC[6]; //6-11
    uint8_t junk1[14]; //12-25
    uint8_t s_IP[4]; //26-29
    uint8_t d_IP[4]; //30-33
    uint8_t s_port[2]; //34-35
    uint8_t d_port[2]; //36-37
    uint8_t l[2]; //38-39
    uint8_t junk2[2]; //40-41
    uint8_t d[1458]; //to the end of the ethernet ethernet_frame
  public:


    uint64_t source_MAC()
    {
      return ( uint64_t ( s_MAC[0] ) << 40 ) |	( uint64_t ( s_MAC[1] ) << 32 ) | ( uint64_t ( s_MAC[2] ) << 24 ) | ( uint64_t ( s_MAC[3] ) << 16 ) | ( uint64_t ( s_MAC[4] ) << 8 ) | ( uint64_t ( s_MAC[5] ) );
    }
    uint32_t source_IP()
    {
      return ntohl ( *reinterpret_cast<uint32_t*> ( s_IP ) );
    }
    uint16_t source_port()
    {
      return ntohs ( *reinterpret_cast<uint16_t*> ( s_port ) );
    }

    uint64_t destination_MAC()
    {
      return ( uint64_t ( d_MAC[0] ) << 40 ) | ( uint64_t ( d_MAC[1] ) << 32 ) | ( uint64_t ( d_MAC[2] ) << 24 ) | ( uint64_t ( d_MAC[3] ) << 16 ) |	( uint64_t ( d_MAC[4] ) << 8 ) | ( uint64_t ( d_MAC[5] ) );
    }
    uint32_t destination_IP()
    {
      return ntohl ( *reinterpret_cast<uint32_t*> ( d_IP ) );
    }
    uint16_t destination_port()
    {
      return ntohs ( *reinterpret_cast<uint16_t*> ( d_port ) );
    }

    uint16_t length()
    {
      return ntohs ( *reinterpret_cast<uint16_t*> ( l ) );
    }

    std::vector<uint32_t> data()
    {
      uint32_t lLength ( ( length()-8 ) >>2 );
      std::vector<uint32_t> lReturn;

      for ( uint32_t i=0 ; i!=lLength ; ++i )
      {
        lReturn.push_back ( *reinterpret_cast<uint32_t*> ( &d[i<<2] ) );
      }

      return lReturn;
    }
};

// --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

// main function
int main ( int argc, char* argv[] )
{
  HostToTargetInspector< 1 , 3 > lHostToTarget;
  TargetToHostInspector< 1 , 3 > lTargetToHost;
  std::ifstream file ( "etc/uhal/tests/wireshark.txt" );

  if ( file.is_open() )
  {
    std::string line;
    uint16_t lTemp , lValue;
    uint8_t block[1500] = { 0 };
    uint8_t* block_ptr = block;

    while ( file.good() )
    {
      getline ( file,line );

      if ( line.length() )
      {
        log ( Info() , "Raw : " , line );
        std::stringstream lStr ( line );
        lStr >> std::hex >> lTemp;

        for ( uint32_t i=0 ; i!=16 ; ++i )
        {
          lStr >> lValue;
          * ( block_ptr++ ) = uint8_t ( lValue );
        }
      }
      else
      {
        if ( block_ptr != block )
        {
          ethernet_frame y = ethernet_frame ( * reinterpret_cast< ethernet_frame* > ( block ) );
          log ( Notice() , "Destination : MAC = " , Integer ( y.destination_MAC(), IntFmt<hex,fixed>() )
                , ", IP = " , Integer ( y.destination_IP(), IntFmt<hex,fixed>() )
                , ", Port = " , Integer ( y.destination_port() )
              );
          log ( Notice() , "     Source : MAC = " , Integer ( y.source_MAC(), IntFmt<hex,fixed>() )
                , ", IP = " , Integer ( y.source_IP(), IntFmt<hex,fixed>() )
                , ", Port = " , Integer ( y.source_port() )
              );
          std::vector<uint32_t> lData ( y.data() );
          std::vector<uint32_t>::const_iterator lBegin ( lData.begin() );
          std::vector<uint32_t>::const_iterator lEnd ( lData.end() );

          if ( y.destination_port() == 50001 )
          {
            log ( Notice() , "Host To Target" );
            lHostToTarget.analyze ( lBegin , lEnd );
          }
          else
          {
            log ( Notice() , "Target to Host" );
            lTargetToHost.analyze ( lBegin , lEnd );
          }

          block_ptr = block;
          std::cout << std::string ( 122 , '-' ) << std::endl;
        }
      }
    }

    file.close();
  }
}











