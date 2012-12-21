// /*
// @file
// @author Andrew W. Rose
// @date 2012
// */

#include "uhal/IPbusPacketInfo.hpp"

// Using the uhal namespace
namespace uhal
{
  logging();
  std::string toString ( const eIPbusProtocolVersion& aIPbusProtocolVersion )
  {
    switch ( aIPbusProtocolVersion )
    {
      case IPbus_1_2:
        return "IPbus version 1.2";
      case IPbus_1_3:
        return "IPbus version 1.3";
      case IPbus_1_4:
        return "IPbus version 1.4";
      case IPbus_2_0:
        return "IPbus version 2.0";
    }

    return "IPbus version unknown!";
  }

}

