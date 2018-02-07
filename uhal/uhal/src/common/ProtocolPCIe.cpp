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

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

---------------------------------------------------------------------------
*/

/**
	@file
	@author Tom Williams
	@date September 2017
*/

#include "uhal/ProtocolPCIe.hpp"


#include <algorithm>                                        // for min
#include <assert.h>
#include <cstdlib>
#include <fcntl.h>
#include <iomanip>                                          // for operator<<
#include <iostream>                                         // for operator<<
#include <sys/stat.h>
#include <stdlib.h>                                         // for size_t, free
#include <string.h>                                         // for memcpy
#include <unistd.h>

#include <boost/chrono/duration.hpp>                        // for operator>
#include <boost/chrono/time_point.hpp>                      // for operator-
#include <boost/thread/thread.hpp>                          // for sleep_for
#include <boost/date_time/posix_time/posix_time_types.hpp>  // for time_dura...

#include "uhal/grammars/URI.hpp"                            // for URI
#include "uhal/log/LogLevels.hpp"                           // for BaseLogLevel
#include "uhal/log/log_inserters.integer.hpp"               // for Integer
#include "uhal/log/log_inserters.quote.hpp"                 // for Quote
#include "uhal/log/log.hpp"
#include "uhal/Buffers.hpp"
#include "uhal/ClientFactory.hpp"


namespace uhal {

PCIe::PCIe ( const std::string& aId, const URI& aUri ) :
  IPbus< 2 , 0 > ( aId , aUri ),
  mDevicePathHostToFPGA(aUri.mHostname.substr(0, aUri.mHostname.find(","))),
  mDevicePathFPGAToHost(aUri.mHostname.substr(aUri.mHostname.find(",")+1)),
  mDeviceFileHostToFPGA(-1),
  mDeviceFileFPGAToHost(-1),
  mNumberOfPages(0),
  mPageSize(0),
  mIndexNextPage(0),
  mPublishedReplyPageCount(0),
  mAsynchronousException ( NULL )
{
  if ( getenv("UHAL_ENABLE_IPBUS_PCIE") == NULL ) {
    exception::ProtocolDoesNotExist lExc;
    log(lExc, "The IPbus 2.0 PCIe client is still an experimental feature, since the software-driver interface could change in the future.");
    log(lExc, "In order to enable the IPbus 2.0 PCIe client, you need to define the environment variable 'UHAL_ENABLE_IPBUS_PCIE'");
    throw lExc;
  }

  if ( aUri.mHostname.find(",") == std::string::npos ) {
    exception::PCIeInitialisationError lExc;
    log(lExc, "No comma found in hostname of PCIe client URI '" + uri() + "'; cannot construct 2 paths for device files");
    throw lExc;
  }
  else if ( aUri.mHostname.find(",") == 0 || aUri.mHostname.find(",") == aUri.mHostname.size()-1) {
    exception::PCIeInitialisationError lExc;
    log(lExc, "Hostname of PCIe client URI '" + uri() + "' starts/ends with a comma; cannot construct 2 paths for device files");
    throw lExc;
  }
}


PCIe::~PCIe()
{
  disconnect();
}


void PCIe::implementDispatch ( boost::shared_ptr< Buffers > aBuffers )
{
  log(Debug(), "PCIe client (URI: ", Quote(uri()), ") : implementDispatch method called");

  if ( (mDeviceFileFPGAToHost < 0) && (mDeviceFileHostToFPGA < 0) )
    connect();

  if ( mReplyQueue.size() == mNumberOfPages )
    read();
  write(aBuffers);
}


void PCIe::Flush( )
{
  log(Debug(), "PCIe client (URI: ", Quote(uri()), ") : Flush method called");
  while ( !mReplyQueue.empty() )
    read();

}


void PCIe::dispatchExceptionHandler()
{
  // FIXME: Adapt to PCIe implementation
  log(Notice(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : closing device files since exception detected");

  ClientInterface::returnBufferToPool ( mReplyQueue );
  disconnect();

  InnerProtocol::dispatchExceptionHandler();
}


uint32_t PCIe::getMaxSendSize()
{
  if ( (mDeviceFileFPGAToHost < 0) && (mDeviceFileHostToFPGA < 0) )
    connect();

  return (mPageSize - 1) * 4;
}


uint32_t PCIe::getMaxReplySize()
{
  if ( (mDeviceFileFPGAToHost < 0) && (mDeviceFileHostToFPGA < 0) )
    connect();

  return (mPageSize - 1) * 4;
}


void PCIe::connect()
{
  log ( Debug() , "PCIe client is opening device file " , Quote ( mDevicePathHostToFPGA ) , " (client-to-device)" );

  mDeviceFileHostToFPGA = open(mDevicePathHostToFPGA.c_str(), O_RDWR );
  if ( mDeviceFileHostToFPGA < 0 ) {
    exception::PCIeInitialisationError lExc;
    log(lExc, "Cannot open host-to-FPGA device file '" + mDevicePathHostToFPGA + "'");
    throw lExc;
  }


  log ( Debug() , "PCIe client is opening device file " , Quote ( mDevicePathHostToFPGA ) , " (device-to-client)" );
  mDeviceFileFPGAToHost = open(mDevicePathFPGAToHost.c_str(), O_RDWR | O_NONBLOCK  /* for read might need O_RDWR | O_NONBLOCK */ );
  if ( mDeviceFileFPGAToHost < 0 ) {
    exception::PCIeInitialisationError lExc;
    log(lExc, "Cannot open FPGA-to-host device file '" + mDevicePathFPGAToHost + "'");
    throw lExc;
  }

  std::vector<uint32_t> lValues;
  dmaRead(mDeviceFileFPGAToHost, 0x0, 4, lValues);

  mNumberOfPages = lValues.at(0);
  mPageSize = lValues.at(1);
  mIndexNextPage = lValues.at(2);
  mPublishedReplyPageCount = lValues.at(3);

  log ( Info() , "PCIe client connected to device at ", Quote(mDevicePathHostToFPGA), ", ", Quote(mDevicePathHostToFPGA), "; FPGA has ", Integer(mNumberOfPages), " pages, each of size ", Integer(mPageSize), " words, index ", Integer(mIndexNextPage), " should be filled next" );
}


void PCIe::disconnect()
{
  // FIXME: Add proper implementation
  mDeviceFileHostToFPGA = -1;
  mDeviceFileFPGAToHost = -1;
}


class PacketFmt {
public:
  PacketFmt(const uint8_t* const, const size_t);
  PacketFmt(const std::vector< std::pair<const uint8_t*, size_t> >& aData);
  ~PacketFmt();

  const std::vector< std::pair<const uint8_t*, size_t> > mData;
};

PacketFmt::PacketFmt(const uint8_t* const aPtr, const size_t aNrBytes) :
  mData(1, std::pair<const uint8_t*, size_t>(aPtr, aNrBytes))
{
   // mData.push_back(std::pair<const uint8_t*, size_t>(aPtr, aNrBytes));
}

PacketFmt::PacketFmt(const std::vector< std::pair<const uint8_t*, size_t> >& aData) :
  mData(aData)
{}

PacketFmt::~PacketFmt()
{}

std::ostream& operator<<(std::ostream& aStream, const PacketFmt& aPacket)
{
  std::ios::fmtflags lOrigFlags( aStream.flags() );

  size_t lNrBytesWritten = 0;
  for (size_t i = 0; i < aPacket.mData.size(); i++) {
    for (const uint8_t* lPtr = aPacket.mData.at(i).first; lPtr != (aPacket.mData.at(i).first + aPacket.mData.at(i).second); lPtr++, lNrBytesWritten++) {
      if ((lNrBytesWritten & 3) == 0)
        aStream << std::endl << "   @ " << std::setw(3) << std::dec << (lNrBytesWritten >> 2) << " :  x";
      aStream << std::setw(2) << std::hex << uint16_t(*lPtr) << " ";
    }
  }

  aStream.flags( lOrigFlags );
  return aStream;
}


void PCIe::write(const boost::shared_ptr<Buffers>& aBuffers)
{
  log (Info(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : writing ", Integer(aBuffers->sendCounter() / 4), "-word packet to page ", Integer(mIndexNextPage), " in ", Quote(mDevicePathHostToFPGA));

  const uint32_t lHeaderWord = (0x10000 | (((aBuffers->sendCounter() / 4) - 1) & 0xFFFF));
  std::vector<std::pair<const uint8_t*, size_t> > lDataToWrite;
  lDataToWrite.push_back( std::make_pair(reinterpret_cast<const uint8_t*>(&lHeaderWord), sizeof lHeaderWord) );
  lDataToWrite.push_back( std::make_pair(aBuffers->getSendBuffer(), aBuffers->sendCounter()) );
  dmaWrite(mDeviceFileHostToFPGA, mIndexNextPage * 4 * mPageSize, lDataToWrite);

  log (Debug(), "Writing " , Integer((aBuffers->sendCounter() / 4) + 1), " 32-bit words at address " , Integer(mIndexNextPage * 4 * mPageSize), " ... ", PacketFmt(lDataToWrite));

  mIndexNextPage = (mIndexNextPage + 1) % mNumberOfPages;
  mReplyQueue.push_back(aBuffers);
}


// -------------------------------------------------------------------------------

void PCIe::read()
{
  const size_t lPageIndexToRead = (mIndexNextPage - mReplyQueue.size() + mNumberOfPages) % mNumberOfPages;
  SteadyClock_t::time_point lStartTime = SteadyClock_t::now();

  uint32_t lHwPublishedPageCount = 0x0;
  unsigned int rx_event[1] = {0};
  int rc = 0;
  
  mDeviceFileFPGAEvent = open("/dev/xdma/card0/events0", O_RDONLY|O_NONBLOCK);
  assert(mDeviceFileFPGAEvent >= 0);

  // wait for interrupt; read events file node to see if user interrupt has come
  while (true){
    rx_event[0] = 0;
    rc = 0;

    rc = ::read(mDeviceFileFPGAEvent, rx_event, 4);
    if(rx_event[0] == 1) {
        //std::cout <<" \n Interrupt recieved " << std::endl;
     break;
     }
    
    
    if (SteadyClock_t::now() - lStartTime > boost::chrono::microseconds(getBoostTimeoutPeriod().total_microseconds())) {
      exception::PCIeTimeout lExc;
      log(lExc, "Next page (index ", Integer(lPageIndexToRead), " count ", Integer(mPublishedReplyPageCount+1), ") of PCIe device '" + mDevicePathHostToFPGA + "' is not ready after timeout period");
      throw lExc;
     }
    
    log(Debug(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Waiting for interrupt  ", "; sleeping for a while ...");
    // boost::this_thread::sleep_for( boost::chrono::microseconds(50));
 
   } // end of while (true)
    
   close(mDeviceFileFPGAEvent);
   

  // Polling 
  /*
  while ( true ) {
    std::vector<uint32_t> lValues;
    // FIXME : Improve by simply adding dmaWrite method that takes uint32_t ref as argument (or returns uint32_t)
    dmaRead(mDeviceFileFPGAToHost, 3, 1, lValues);
    lHwPublishedPageCount = lValues.at(0);

    if (lHwPublishedPageCount != mPublishedReplyPageCount) {
      mPublishedReplyPageCount++;
      break;
    }
    // FIXME: Throw if published page count is invalid number

    if (SteadyClock_t::now() - lStartTime > boost::chrono::microseconds(getBoostTimeoutPeriod().total_microseconds())) {
      exception::PCIeTimeout lExc;
      log(lExc, "Next page (index ", Integer(lPageIndexToRead), " count ", Integer(mPublishedReplyPageCount+1), ") of PCIe device '" + mDevicePathHostToFPGA + "' is not ready after timeout period");
      throw lExc;
    }

    log(Debug(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Trying to read page index ", Integer(lPageIndexToRead), " = count ", Integer(mPublishedReplyPageCount+1), "; published page count is ", Integer(lHwPublishedPageCount), "; sleeping for a while ...");
    boost::this_thread::sleep_for( boost::chrono::microseconds(50));
  }
 */


  log(Info(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Reading page ", Integer(lPageIndexToRead), " (published count ", Integer(lHwPublishedPageCount), ", surpasses required, ", Integer(mPublishedReplyPageCount), ")");

  // PART 1 : Read the page
  boost::shared_ptr<Buffers> lBuffers = mReplyQueue.front();
  mReplyQueue.pop_front();

  std::vector<uint32_t> lPageContents;
  dmaRead(mDeviceFileFPGAToHost, 4 + lPageIndexToRead * 4 * mPageSize, 1 + (lBuffers->replyCounter() >> 2), lPageContents);
  log (Debug(), "Read " , Integer(1 + (lBuffers->replyCounter() >> 2)), " 32-bit words from address " , Integer(4 + lPageIndexToRead * 4 * mPageSize), " ... ", PacketFmt((const uint8_t*)lPageContents.data(), 4 * lPageContents.size()));


  // PART 2 : Transfer to reply buffer
  const std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( lBuffers->getReplyBuffer() );
  size_t lNrWordsInPacket = (lPageContents.at(0) >> 16) + (lPageContents.at(0) & 0xFFFF);
  if (lNrWordsInPacket != (lBuffers->replyCounter() >> 2))
    log (Warning(), "Expected reply packet to contain ", Integer(lBuffers->replyCounter() >> 2), " words, but it actually contains ", Integer(lNrWordsInPacket), " words");

  size_t lNrBytesCopied = 0;
  for ( std::deque< std::pair< uint8_t* , uint32_t > >::const_iterator lIt = lReplyBuffers.begin() ; lIt != lReplyBuffers.end() ; ++lIt )
  {
    // Don't copy more of page than was written to, for cases when less data received than expected
    if ( lNrBytesCopied >= 4*lNrWordsInPacket)
      break;

    size_t lNrBytesToCopy = std::min( lIt->second , uint32_t(4*lNrWordsInPacket - lNrBytesCopied) );
    memcpy ( lIt->first, &lPageContents.at(1 + (lNrBytesCopied / 4)), lNrBytesToCopy );
    lNrBytesCopied += lNrBytesToCopy;
  }

  // PART 3 : Validate the packet contents
  try
  {
    if ( uhal::exception::exception* lExc = ClientInterface::validate ( lBuffers ) ) //Control of the pointer has been passed back to the client interface
    {
      mAsynchronousException = lExc;
    }
  }
  catch ( exception::exception& aExc )
  {
    mAsynchronousException = new exception::ValidationError ();
    log ( *mAsynchronousException , "Exception caught during reply validation for PCIe device with URI " , Quote ( this->uri() ) , "; what returned: " , Quote ( aExc.what() ) );
  }

  if ( mAsynchronousException )
  {
    mAsynchronousException->ThrowAsDerivedType();
  }
}


void PCIe::dmaRead(int aFileDescriptor, const uint32_t aAddr, const uint32_t aNrWords, std::vector<uint32_t>& aValues)
{
  char *allocated = NULL;
  posix_memalign((void **)&allocated, 4096/*alignment*/, 4*aNrWords + 4096);
  assert(allocated);

  /* select AXI MM address */
  char* buffer = allocated;
  off_t off = lseek(aFileDescriptor, 4*aAddr, SEEK_SET);
  if ( off != (4 * aAddr)) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Offset returned by lseek, ", Integer(off), ", does not match that requested, ", Integer(4*aAddr), " (in preparation for read of ", Integer(aNrWords), " words)");
    throw lExc;
  }

  /* read data from AXI MM into buffer using SGDMA */
  int rc = ::read(aFileDescriptor, buffer, 4*aNrWords);
  assert(rc >= 0);
  assert((rc % 4) == 0);
  if ((rc > 0) && (size_t(rc) < 4*aNrWords)) {
    std::cout << "Short read of " << rc << " bytes into a " << 4*aNrWords << " bytes buffer, could be a packet read?\n";
  }

  aValues.insert(aValues.end(), reinterpret_cast<uint32_t*>(buffer), reinterpret_cast<uint32_t*>(buffer)+ aNrWords);
//    for (unsigned i=0; i<aNrWords; i++) {
//      //uint32_t val = (buffer[4*i]&0xff)+((buffer[4*i+1]&0xff)<<8)+((buffer[4*i+2]&0xff)<<16)
//      //  +((buffer[4*i+3]&0xff)<<24);
//      uint32_t val = *reinterpret_cast<uint32_t*>(buffer);
//      printf("ipbus address : 0x%08x\n", aAddr);
//      printf("readback value: 0x%08x\n\n",val);
//    }

  free(allocated);
}


bool PCIe::dmaWrite(int aFileDescriptor, const uint32_t aAddr, const std::vector<uint32_t>& aValues)
{
  return dmaWrite(aFileDescriptor, 4 * aAddr, reinterpret_cast<const uint8_t* const>(aValues.data()), 4 * aValues.size());
}


bool PCIe::dmaWrite(int aFileDescriptor, const uint32_t aAddr, const uint8_t* const aPtr, const size_t aNrBytes)
{
  assert((aNrBytes % 4) == 0);

  char *allocated = NULL;
  posix_memalign((void **)&allocated, 4096/*alignment*/, aNrBytes + 4096);
  assert(allocated);

  // data to write to register address
  char* buffer = allocated;
  memcpy(buffer, aPtr, aNrBytes);

  /* select AXI MM address */
  off_t off = lseek(aFileDescriptor, aAddr, SEEK_SET);
  if ( off != aAddr) {
    struct stat st;
    if (fstat(aFileDescriptor, &st) or (not S_ISFIFO(st.st_mode))) {
      exception::PCIeCommunicationError lExc;
      log(lExc, "Offset returned by lseek, ", Integer(off), ", does not match that requested, ", Integer(aAddr), " (in preparation for write of ", Integer(aNrBytes), " bytes)");
      throw lExc;
    }
  }

  /* write buffer to AXI MM address using SGDMA */
  int rc = ::write(aFileDescriptor, buffer, aNrBytes);
  assert((rc > 0) && (size_t(rc) == aNrBytes));

  free(allocated);

  return true;
}


bool PCIe::dmaWrite(int aFileDescriptor, const uint32_t aAddr, const std::vector<std::pair<const uint8_t*, size_t> >& aData)
{
  size_t lNrBytes = 0;
  for (size_t i = 0; i < aData.size(); i++)
    lNrBytes += aData.at(i).second;

  assert((lNrBytes % 4) == 0);

  char *allocated = NULL;
  posix_memalign((void **)&allocated, 4096/*alignment*/, lNrBytes + 4096);
  assert(allocated);

  // data to write to register address
  char* buffer = allocated;
  size_t lNrBytesCopied = 0;
  for (size_t i = 0; i < aData.size(); i++) {
    memcpy(buffer + lNrBytesCopied, aData.at(i).first, aData.at(i).second);
    lNrBytesCopied += aData.at(i).second;
  }

  /* select AXI MM address */
  off_t off = lseek(aFileDescriptor, aAddr, SEEK_SET);
  if ( off != aAddr) {
    struct stat st;
    if (fstat(aFileDescriptor, &st) or (not S_ISFIFO(st.st_mode))) {
      exception::PCIeCommunicationError lExc;
      log(lExc, "Offset returned by lseek, ", Integer(off), ", does not match that requested, ", Integer(aAddr), " (in preparation for write of ", Integer(lNrBytes), " bytes)");
      throw lExc;
    }
  }

  /* write buffer to AXI MM address using SGDMA */
  int rc = ::write(aFileDescriptor, buffer, lNrBytes);
  assert((rc > 0) && (size_t(rc) == lNrBytes));

  free(allocated);

  return true;
}


} // end ns uhal
