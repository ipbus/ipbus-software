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
#include <errno.h>
#include <fcntl.h>
#include <iomanip>                                          // for operator<<
#include <iostream>                                         // for operator<<
#include <sys/stat.h>
#include <stdlib.h>                                         // for size_t, free
#include <string.h>                                         // for memcpy
#include <unistd.h>

#include <boost/chrono/duration.hpp>                        // for operator>
#include <boost/chrono/time_point.hpp>                      // for operator-
#include <boost/lexical_cast.hpp>
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


PCIe::PacketFmt::PacketFmt(const uint8_t* const aPtr, const size_t aNrBytes) :
  mData(1, std::pair<const uint8_t*, size_t>(aPtr, aNrBytes))
{}


PCIe::PacketFmt::PacketFmt(const std::vector< std::pair<const uint8_t*, size_t> >& aData) :
  mData(aData)
{}


PCIe::PacketFmt::~PacketFmt()
{}


std::ostream& operator<<(std::ostream& aStream, const PCIe::PacketFmt& aPacket)
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




PCIe::File::File(const std::string& aPath, int aFlags) :
  mPath(aPath),
  mFd(-1),
  mFlags(aFlags)
{
}


PCIe::File::~File()
{
  close();
}


const std::string& PCIe::File::getPath() const
{
  return mPath;
}


void PCIe::File::setPath(const std::string& aPath)
{
  mPath = aPath;
}


void PCIe::File::open()
{
  if (mFd != -1)
    return;

  mFd = ::open(mPath.c_str(), mFlags);
  if ( mFd == -1 ) {
    exception::PCIeInitialisationError lExc;
    log(lExc, "Failed to open device file ", Quote(mPath), "; errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
    throw lExc;
  }
}


void PCIe::File::close()
{
  if (mFd != -1) {
    int rc = ::close(mFd);
    mFd = -1;
    if (rc == -1)
      log (Error(), "Failed to close file ", Quote(mPath), "; errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
  }
}


void PCIe::File::read(const uint32_t aAddr, const uint32_t aNrWords, std::vector<uint32_t>& aValues)
{
  if (mFd == -1)
    open();

  char *allocated = NULL;
  posix_memalign((void **)&allocated, 4096/*alignment*/, 4*aNrWords + 4096);
  if (allocated == NULL) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Failed to allocate ", Integer(4*aNrWords + 4096), " bytes in File::read function");
    throw lExc;
  }

  /* select AXI MM address */
  char* buffer = allocated;
  off_t off = lseek(mFd, 4*aAddr, SEEK_SET);
  if ( off != off_t(4 * aAddr)) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Offset returned by lseek, ", Integer(off), ", does not match that requested, ", Integer(4*aAddr), " (in preparation for read of ", Integer(aNrWords), " words)");
    throw lExc;
  }

  /* read data from AXI MM into buffer using SGDMA */
  int rc = ::read(mFd, buffer, 4*aNrWords);
  if (rc == -1) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Read of ", Integer(4*aNrWords), " bytes at address ", Integer(4 * aAddr), " failed! errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
    throw lExc;
  }
  else if (size_t(rc) < 4*aNrWords) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Only ", Integer(rc), " bytes transferred in read of ", Integer(4*aNrWords), " bytes at address ", Integer(4 * aAddr));
    throw lExc;
  }

  aValues.insert(aValues.end(), reinterpret_cast<uint32_t*>(buffer), reinterpret_cast<uint32_t*>(buffer)+ aNrWords);

  free(allocated);
}


void PCIe::File::write(const uint32_t aAddr, const std::vector<uint32_t>& aValues)
{
  write(4 * aAddr, reinterpret_cast<const uint8_t* const>(aValues.data()), 4 * aValues.size());
}


void PCIe::File::write(const uint32_t aAddr, const uint8_t* const aPtr, const size_t aNrBytes)
{
  if (mFd == -1)
    open();

  assert((aNrBytes % 4) == 0);

  char *allocated = NULL;
  posix_memalign((void **)&allocated, 4096/*alignment*/, aNrBytes + 4096);
  if (allocated == NULL) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Failed to allocate ", Integer(aNrBytes + 4096), " bytes in File::write/3 function");
    throw lExc;
  }

  // data to write to register address
  char* buffer = allocated;
  memcpy(buffer, aPtr, aNrBytes);

  /* select AXI MM address */
  off_t off = lseek(mFd, aAddr, SEEK_SET);
  if ( off != off_t(aAddr)) {
    struct stat st;
    if (fstat(mFd, &st) or (not S_ISFIFO(st.st_mode))) {
      exception::PCIeCommunicationError lExc;
      log(lExc, "Offset returned by lseek, ", Integer(off), ", does not match that requested, ", Integer(aAddr), " (in preparation for write of ", Integer(aNrBytes), " bytes)");
      throw lExc;
    }
  }

  /* write buffer to AXI MM address using SGDMA */
  int rc = ::write(mFd, buffer, aNrBytes);
  if (rc == -1) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Write of ", Integer(aNrBytes), " bytes at address ", Integer(aAddr), " failed! errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
    throw lExc;
  }
  else if (size_t(rc) < aNrBytes) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Only ", Integer(rc), " bytes transferred in write of ", Integer(aNrBytes), " bytes at address ", Integer(aAddr));
    throw lExc;
  }

  free(allocated);
}


void PCIe::File::write(const uint32_t aAddr, const std::vector<std::pair<const uint8_t*, size_t> >& aData)
{
  if (mFd == -1)
    open();

  size_t lNrBytes = 0;
  for (size_t i = 0; i < aData.size(); i++)
    lNrBytes += aData.at(i).second;

  assert((lNrBytes % 4) == 0);

  char *allocated = NULL;
  posix_memalign((void **)&allocated, 4096/*alignment*/, lNrBytes + 4096);
  if (allocated == NULL) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Failed to allocate ", Integer(lNrBytes + 4096), " bytes in File::write/2 function");
    throw lExc;
  }

  // data to write to register address
  char* buffer = allocated;
  size_t lNrBytesCopied = 0;
  for (size_t i = 0; i < aData.size(); i++) {
    memcpy(buffer + lNrBytesCopied, aData.at(i).first, aData.at(i).second);
    lNrBytesCopied += aData.at(i).second;
  }

  /* select AXI MM address */
  off_t off = lseek(mFd, aAddr, SEEK_SET);
  if ( off != off_t(aAddr)) {
    struct stat st;
    if (fstat(mFd, &st) or (not S_ISFIFO(st.st_mode))) {
      exception::PCIeCommunicationError lExc;
      log(lExc, "Offset returned by lseek, ", Integer(off), ", does not match that requested, ", Integer(aAddr), " (in preparation for write of ", Integer(lNrBytes), " bytes)");
      throw lExc;
    }
  }

  /* write buffer to AXI MM address using SGDMA */
  int rc = ::write(mFd, buffer, lNrBytes);
  if (rc == -1) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Write of ", Integer(lNrBytes), " bytes at address ", Integer(aAddr), " failed! errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
    throw lExc;
  }
  else if (size_t(rc) < lNrBytes) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Only ", Integer(rc), " bytes transferred in write of ", Integer(lNrBytes), " bytes at address ", Integer(aAddr));
    throw lExc;
  }

  free(allocated);
}




PCIe::PCIe ( const std::string& aId, const URI& aUri ) :
  IPbus< 2 , 0 > ( aId , aUri ),
  mConnected(false),
  mDeviceFileHostToFPGA(aUri.mHostname.substr(0, aUri.mHostname.find(",")), O_RDWR ),
  mDeviceFileFPGAToHost(aUri.mHostname.substr(aUri.mHostname.find(",")+1), O_RDWR | O_NONBLOCK  /* for read might need O_RDWR | O_NONBLOCK */),
  mDeviceFileFPGAEvent("", O_RDONLY),
  mXdma7seriesWorkaround(false),
  mUseInterrupt(false),
  mNumberOfPages(0),
  mMaxInFlight(0),
  mPageSize(0),
  mMaxPacketSize(0),
  mIndexNextPage(0),
  mPublishedReplyPageCount(0),
  mReadReplyPageCount(0),
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

  mSleepDuration = boost::chrono::microseconds(mUseInterrupt ? 0 : 50);

  for (NameValuePairVectorType::const_iterator lIt = aUri.mArguments.begin(); lIt != aUri.mArguments.end(); lIt++) {
    if (lIt->first == "events") {
      if (mUseInterrupt) {
        exception::PCIeInitialisationError lExc;
        log(lExc, "PCIe client URI ", Quote(uri()), ": 'events' attribute is specified multiple times");
        throw lExc;
      }

      mUseInterrupt = true;
      mDeviceFileFPGAEvent.setPath(lIt->second);
      log (Info() , "PCIe client with URI ", Quote (uri()), " is configured to use interrupts");
    }
    else if (lIt->first == "sleep") {
      mSleepDuration = boost::chrono::microseconds(boost::lexical_cast<size_t>(lIt->second));
      log (Notice() , "PCIe client with URI ", Quote (uri()), " : Inter-poll-/-interrupt sleep duration set to ", boost::lexical_cast<size_t>(lIt->second), " us by URI 'sleep' attribute");
    }
    else if (lIt->first == "max_in_flight") {
      mMaxInFlight = boost::lexical_cast<size_t>(lIt->second);
      log (Notice() , "PCIe client with URI ", Quote (uri()), " : 'Maximum number of packets in flight' set to ", boost::lexical_cast<size_t>(lIt->second), " by URI 'max_in_flight' attribute");
    }
    else if (lIt->first == "max_packet_size") {
      mMaxPacketSize = boost::lexical_cast<size_t>(lIt->second);
      log (Notice() , "PCIe client with URI ", Quote (uri()), " : 'Maximum packet size (in 32-bit words) set to ", boost::lexical_cast<size_t>(lIt->second), " by URI 'max_packet_size' attribute");
    }
    else if (lIt->first == "xdma_7series_workaround") {
      mXdma7seriesWorkaround = true;
      log (Notice() , "PCIe client with URI ", Quote (uri()), " : Adjusting size of PCIe reads to a few fixed sizes as workaround for 7-series xdma firmware bug");
    }
    else
      log (Warning() , "Unknown attribute ", Quote (lIt->first), " used in URI ", Quote(uri()));
  }
}


PCIe::~PCIe()
{
  disconnect();
}


void PCIe::implementDispatch ( boost::shared_ptr< Buffers > aBuffers )
{
  log(Debug(), "PCIe client (URI: ", Quote(uri()), ") : implementDispatch method called");

  if ( ! mConnected )
    connect();

  if ( mReplyQueue.size() == mMaxInFlight )
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
  log(Notice(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : closing device files since exception detected");

  ClientInterface::returnBufferToPool ( mReplyQueue );
  disconnect();

  InnerProtocol::dispatchExceptionHandler();
}


uint32_t PCIe::getMaxSendSize()
{
  if ( ! mConnected )
    connect();

  return mMaxPacketSize * 4;
}


uint32_t PCIe::getMaxReplySize()
{
  if ( ! mConnected )
    connect();

  return mMaxPacketSize * 4;
}


void PCIe::connect()
{
  log ( Debug() , "PCIe client is opening device file " , Quote ( mDeviceFileHostToFPGA.getPath() ) , " (client-to-device)" );
  mDeviceFileHostToFPGA.open();

  log ( Debug() , "PCIe client is opening device file " , Quote ( mDeviceFileFPGAToHost.getPath() ) , " (device-to-client)" );
  std::vector<uint32_t> lValues;
  mDeviceFileFPGAToHost.read(0x0, 4, lValues);
  log (Info(), "Read status info from addr 0 (", Integer(lValues.at(0)), ", ", Integer(lValues.at(1)), ", ", Integer(lValues.at(2)), ", ", Integer(lValues.at(3)), "): ", PacketFmt((const uint8_t*)lValues.data(), 4 * lValues.size()));

  mNumberOfPages = lValues.at(0);
  if ( (mMaxInFlight == 0) or (mMaxInFlight > mNumberOfPages) )
    mMaxInFlight = mNumberOfPages;
  mPageSize = std::min(uint32_t(4096), lValues.at(1));
  if ( (mMaxPacketSize == 0) or (mMaxPacketSize >= mPageSize) )
    mMaxPacketSize = mPageSize - 1;
  mIndexNextPage = lValues.at(2);
  mPublishedReplyPageCount = lValues.at(3);
  mReadReplyPageCount = mPublishedReplyPageCount;

  if (lValues.at(1) > 0xFFFF) {
    exception::PCIeInitialisationError lExc;
    log (lExc, "Invalid page size, ", Integer(lValues.at(1)), ", reported in device file ", Quote(mDeviceFileFPGAToHost.getPath()));
    throw lExc;
  }

  if (mIndexNextPage >= mNumberOfPages) {
    exception::PCIeInitialisationError lExc;
    log (lExc, "Next page index, ", Integer(mIndexNextPage), ", reported in device file ", Quote(mDeviceFileFPGAToHost.getPath()), " is inconsistent with number of pages, ", Integer(mNumberOfPages));
    throw lExc;
  }

  if (mUseInterrupt)
    mDeviceFileFPGAEvent.open();

  mConnected = true;
  log ( Info() , "PCIe client connected to device at ", Quote(mDeviceFileHostToFPGA.getPath()), ", ", Quote(mDeviceFileFPGAToHost.getPath()), "; FPGA has ", Integer(mNumberOfPages), " pages, each of size ", Integer(mPageSize), " words, index ", Integer(mIndexNextPage), " should be filled next" );
}


void PCIe::disconnect()
{
  mDeviceFileHostToFPGA.close();
  mDeviceFileFPGAToHost.close();
  mDeviceFileFPGAEvent.close();
  mConnected = false;
}


void PCIe::write(const boost::shared_ptr<Buffers>& aBuffers)
{
  log (Info(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : writing ", Integer(aBuffers->sendCounter() / 4), "-word packet to page ", Integer(mIndexNextPage), " in ", Quote(mDeviceFileHostToFPGA.getPath()));

  const uint32_t lHeaderWord = (0x10000 | (((aBuffers->sendCounter() / 4) - 1) & 0xFFFF));
  std::vector<std::pair<const uint8_t*, size_t> > lDataToWrite;
  lDataToWrite.push_back( std::make_pair(reinterpret_cast<const uint8_t*>(&lHeaderWord), sizeof lHeaderWord) );
  lDataToWrite.push_back( std::make_pair(aBuffers->getSendBuffer(), aBuffers->sendCounter()) );
  mDeviceFileHostToFPGA.write(mIndexNextPage * 4 * mPageSize, lDataToWrite);

  log (Debug(), "Wrote " , Integer((aBuffers->sendCounter() / 4) + 1), " 32-bit words at address " , Integer(mIndexNextPage * 4 * mPageSize), " ... ", PacketFmt(lDataToWrite));

  mIndexNextPage = (mIndexNextPage + 1) % mNumberOfPages;
  mReplyQueue.push_back(aBuffers);
}


void PCIe::read()
{
  const size_t lPageIndexToRead = (mIndexNextPage - mReplyQueue.size() + mNumberOfPages) % mNumberOfPages;
  SteadyClock_t::time_point lStartTime = SteadyClock_t::now();

  if (mReadReplyPageCount == mPublishedReplyPageCount)
  {
    if (mUseInterrupt)
    {
      std::vector<uint32_t> lRxEvent;
      // wait for interrupt; read events file node to see if user interrupt has come
      while (true) {
        mDeviceFileFPGAEvent.read(0, 1, lRxEvent);
        if (lRxEvent.at(0) == 1) {
          break;
        }
        lRxEvent.clear();

        if (SteadyClock_t::now() - lStartTime > boost::chrono::microseconds(getBoostTimeoutPeriod().total_microseconds())) {
          exception::PCIeTimeout lExc;
          log(lExc, "Next page (index ", Integer(lPageIndexToRead), " count ", Integer(mPublishedReplyPageCount+1), ") of PCIe device '" + mDeviceFileHostToFPGA.getPath() + "' is not ready after timeout period");
          throw lExc;
        }

        log(Debug(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Waiting for interrupt; sleeping for ", mSleepDuration.count(), "us");
        if (mSleepDuration > boost::chrono::microseconds(0))
          boost::this_thread::sleep_for( mSleepDuration );

      } // end of while (true)

      log(Info(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Reading page ", Integer(lPageIndexToRead), " (interrupt received)");
    }
    else
    {
      uint32_t lHwPublishedPageCount = 0x0;

      while ( true ) {
        std::vector<uint32_t> lValues;
        // FIXME : Improve by simply adding fileWrite method that takes uint32_t ref as argument (or returns uint32_t)
        mDeviceFileFPGAToHost.read(0, (mXdma7seriesWorkaround ? 8 : 4), lValues);
        lHwPublishedPageCount = lValues.at(3);
        log (Debug(), "Read status info from addr 0 (", Integer(lValues.at(0)), ", ", Integer(lValues.at(1)), ", ", Integer(lValues.at(2)), ", ", Integer(lValues.at(3)), "): ", PacketFmt((const uint8_t*)lValues.data(), 4 * lValues.size()));

        if (lHwPublishedPageCount != mPublishedReplyPageCount) {
          mPublishedReplyPageCount = lHwPublishedPageCount;
          break;
        }
        // FIXME: Throw if published page count is invalid number

        if (SteadyClock_t::now() - lStartTime > boost::chrono::microseconds(getBoostTimeoutPeriod().total_microseconds())) {
          exception::PCIeTimeout lExc;
          log(lExc, "Next page (index ", Integer(lPageIndexToRead), " count ", Integer(mPublishedReplyPageCount+1), ") of PCIe device '" + mDeviceFileHostToFPGA.getPath() + "' is not ready after timeout period");
          throw lExc;
        }

        log(Debug(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Trying to read page index ", Integer(lPageIndexToRead), " = count ", Integer(mReadReplyPageCount+1), "; published page count is ", Integer(lHwPublishedPageCount), "; sleeping for ", mSleepDuration.count(), "us");
        if (mSleepDuration > boost::chrono::microseconds(0))
          boost::this_thread::sleep_for( mSleepDuration );
      }

      log(Info(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Reading page ", Integer(lPageIndexToRead), " (published count ", Integer(lHwPublishedPageCount), ", surpasses required, ", Integer(mReadReplyPageCount + 1), ")");
    }
  }
  mReadReplyPageCount++;
  
  // PART 1 : Read the page
  boost::shared_ptr<Buffers> lBuffers = mReplyQueue.front();
  mReplyQueue.pop_front();

  uint32_t lNrWordsToRead(lBuffers->replyCounter() >> 2);
  if(mXdma7seriesWorkaround and (lNrWordsToRead % 32 == 0 || lNrWordsToRead % 32 == 28 || lNrWordsToRead < 4))
    lNrWordsToRead += 4;
  lNrWordsToRead += 1;
 
  std::vector<uint32_t> lPageContents;
  mDeviceFileFPGAToHost.read(4 + lPageIndexToRead * mPageSize, lNrWordsToRead , lPageContents);
  log (Debug(), "Read " , Integer(lNrWordsToRead), " 32-bit words from address " , Integer(4 + lPageIndexToRead * 4 * mPageSize), " ... ", PacketFmt((const uint8_t*)lPageContents.data(), 4 * lPageContents.size()));

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


} // end ns uhal
