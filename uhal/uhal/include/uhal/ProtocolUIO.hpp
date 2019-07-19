#ifndef _uiouhal_ProtocolUIO_hpp_
#define _uiouhal_ProtocolUIO_hpp_

#include <uhal/ClientInterface.hpp>
#include <uhal/ValMem.hpp>
#include "uhal/log/exception.hpp"

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
    // //! Exception class to handle the case where an Atomic Transaction was requested but could not be performed. Uses the base uhal::exception implementation of what()
    // class AtomicTransactionSize : public exception {};
    //! Exception class to handle the case where pinging of a client failed.
    //UHAL_DEFINE_EXCEPTION_CLASS ( PingFailed , "Exception class to handle the case where pinging of a client failed." )

    //! Exception class to handle the case where a masked write was attempted with a data source which has non-zero bits outside the bit-mask's bounds.
    //UHAL_DEFINE_EXCEPTION_CLASS ( BitsSetWhichAreForbiddenByBitMask , "Exception class to handle the case where a masked write was attempted with a data source which has non-zero bits outside the bit-mask's bounds." )

    //! Exception class to handle the case where we were unable to validate the packet.
    //UHAL_DEFINE_EXCEPTION_CLASS ( ValidationError , "Exception class to handle the case where we were unable to validate the packet." )

    //! Exception class to handle a NULL buffer being passed to the transport class.
    //UHAL_DEFINE_EXCEPTION_CLASS ( NullBufferException , "Exception class to handle a NULL buffer being passed to the transport class." )

    //UHAL_DEFINE_EXCEPTION_CLASS ( PacketLevelError, "Base exception class covering situations in which a packet-level error occurs.")

    //UHAL_DEFINE_EXCEPTION_CLASS ( TransactionLevelError, "Base exception class covering situations in which a transaction-level error occurs (i.e. error occurs in individual read/write).")

    //UHAL_DEFINE_EXCEPTION_CLASS ( ClientTimeout, "Base exception class covering timeouts when waiting for reply from device")

    //UHAL_DEFINE_EXCEPTION_CLASS ( TransportLayerError, "Base exception class covering non-timeout transport-layer-specific errors.")
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

    ValHeader implementBOT() {}
    ValHeader implementWriteBlock (const uint32_t& aAddr, const std::vector<uint32_t>& aValues, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL) {}
    ValVector< uint32_t > implementReadBlock ( const uint32_t& aAddr, const uint32_t& aSize, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL ) {}
    ValWord< uint32_t > implementRMWbits ( const uint32_t& aAddr , const uint32_t& aANDterm , const uint32_t& aORterm ) {}
    ValWord< uint32_t > implementRMWsum ( const uint32_t& aAddr , const int32_t& aAddend ) {}
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
};

}
#endif
