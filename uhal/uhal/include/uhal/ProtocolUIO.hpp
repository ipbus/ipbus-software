/*
---------------------------------------------------------------------------

    This is an extension of uHAL to directly access AXI slaves via the linux
    UIO driver. 

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

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

      Dan Gastler, Boston University 
      email: dgastler <AT> bu.edu
      
---------------------------------------------------------------------------
*/
/**
	@file
	@author Siqi Yuan / Dan Gastler / Theron Jasper Tarigo
*/

#ifndef _uiouhal_ProtocolUIO_hpp_
#define _uiouhal_ProtocolUIO_hpp_

#include <uhal/ClientInterface.hpp>
#include <uhal/ValMem.hpp>
#include "uhal/log/exception.hpp"
#include <signal.h> //for handling of SIG_BUS signals

namespace uioaxi {

//should only change the ADDR_DEV_BITS
const int ADDR_DEV_BITS = 8;
const int ADDR_DEV_OFFSET = 32-ADDR_DEV_BITS;
const uint32_t ADDR_WORD_MASK = (0x1 << ADDR_DEV_OFFSET) - 1;
const uint32_t ADDR_DEV_MASK  = ~ADDR_WORD_MASK;
const int DEVICES_MAX = 0x1 << ADDR_DEV_BITS;
const int DEVNUMPRLEN = 1+(ADDR_DEV_BITS/4-1);

typedef uint8_t devnum_t;
typedef uint16_t wordnum_t;

struct DevAddr {
  devnum_t device;
  wordnum_t word;
};

}

namespace uhal {

  namespace exception
  {
    UHAL_DEFINE_EXCEPTION_CLASS ( UnmatchedLabel , "Exception class to handle the case where matching a label to a device failed." )
    UHAL_DEFINE_EXCEPTION_CLASS ( BadUIODevice , "Exception class to handle the case where uio device cannot be opened." )
    UHAL_DEFINE_EXCEPTION_CLASS ( UnimplementedFunction , "Exception class to handle the case where an unimplemented function is called." )
    UHAL_DEFINE_EXCEPTION_CLASS ( UIOBusError , "Exception class for when an axi transaction causes a BUS_ERROR." )
  }

class UIO : public ClientInterface {
  public:
    UIO (
      const std::string& aId, const URI& aUri,
      const boost::posix_time::time_duration&aTimeoutPeriod =
        boost::posix_time::milliseconds(10)
    );
    virtual ~UIO ();
  protected:
    ValHeader implementWrite (const uint32_t& aAddr, const uint32_t& aValue);
    ValWord<uint32_t> implementRead (const uint32_t& aAddr,
      const uint32_t& aMask = defs::NOMASK);
    void implementDispatch (boost::shared_ptr<Buffers> aBuffers) /*override*/ ;

    ValHeader implementBOT();
    ValHeader implementWriteBlock (const uint32_t& aAddr, const std::vector<uint32_t>& aValues, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL);
    ValVector< uint32_t > implementReadBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );
    ValWord< uint32_t > implementRMWbits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm );
    ValWord< uint32_t > implementRMWsum ( const uint32_t& aAddr , const int32_t& aAddend );
    uint32_t getMaxNumberOfBuffers() {return 0;}
    uint32_t getMaxSendSize() {return 0;}
    uint32_t getMaxReplySize() {return 0;}

    virtual  exception::exception* validate ( uint8_t* aSendBufferStart ,
    uint8_t* aSendBufferEnd ,
    std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyStartIt ,
    std::deque< std::pair< uint8_t* , uint32_t > >::iterator aReplyEndIt ) {
      return NULL;}
  private:
    int fd[uioaxi::DEVICES_MAX];
    uint32_t volatile * hw[uioaxi::DEVICES_MAX];
    uint32_t addrs[uioaxi::DEVICES_MAX];
    char uionames[uioaxi::DEVICES_MAX][128];
    uioaxi::DevAddr decodeAddress (uint32_t uaddr);
    std::vector< ValWord<uint32_t> > valwords;
    void primeDispatch ();
    void openDevice (int devnum, uint32_t size, const char *name);
    int checkDevice (int devnum);
    struct sigaction saBusError;
    struct sigaction saBusError_old;
};

}
#endif
