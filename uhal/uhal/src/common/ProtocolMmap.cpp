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

#include "uhal/ProtocolMmap.hpp"


#include <algorithm>                                        // for min
#include <assert.h>
#include <chrono>
#include <cstdlib>
#include <fcntl.h>
#include <iomanip>                                          // for operator<<
#include <iostream>                                         // for operator<<
#include <sys/mman.h>
#include <sys/stat.h>
#include <stdlib.h>                                         // for size_t, free
#include <string.h>                                         // for memcpy
#include <thread>
#include <unistd.h>

#include <boost/lexical_cast.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>  // for time_dura...

#include "uhal/grammars/URI.hpp"                            // for URI
#include "uhal/log/LogLevels.hpp"                           // for BaseLogLevel
#include "uhal/log/log_inserters.integer.hpp"               // for Integer
#include "uhal/log/log_inserters.quote.hpp"                 // for Quote
#include "uhal/log/log.hpp"
#include "uhal/Buffers.hpp"
#include "uhal/ClientFactory.hpp"
#include "uhal/SigBusGuard.hpp"


namespace uhal {


Mmap::PacketFmt::PacketFmt(const uint8_t* const aPtr, const size_t aNrBytes) :
  mData(1, std::pair<const uint8_t*, size_t>(aPtr, aNrBytes))
{
}


Mmap::PacketFmt::PacketFmt(const std::vector< std::pair<const uint8_t*, size_t> >& aData) :
  mData(aData)
{}


Mmap::PacketFmt::~PacketFmt()
{}


std::ostream& operator<<(std::ostream& aStream, const Mmap::PacketFmt& aPacket)
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




Mmap::File::File(const std::string& aPath, int aFlags) :
  mPath(aPath),
  mFd(-1),
  mFlags(aFlags),
  mOffset(0),
  mMmapPtr(NULL),
  mMmapIOPtr(NULL)
{
}


Mmap::File::~File()
{
  close();
}


const std::string& Mmap::File::getPath() const
{
  return mPath;
}


void Mmap::File::setPath(const std::string& aPath)
{
  mPath = aPath;
}


void Mmap::File::setOffset(size_t aOffset)
{
  mOffset = aOffset;
}


// The default version of ipbus_transport_axi_if uses four IPBus
// transport buffers, each with a 2^11-bit address space. In
// addition, the first four 32-bit words in the IPBus transport
// address space contain status information. This means the
// corresponding memory memory map needs to allocate 2^15 + 16 bytes.
#define MAP_SIZE (32*1024UL + 16UL)
#define MAP_MASK (MAP_SIZE - 1)


void Mmap::File::open()
{
  if (mFd != -1)
    return;

  mFd = ::open(mPath.c_str(), mFlags);
  if ( mFd == -1 ) {
    exception::MmapInitialisationError lExc;
    log(lExc, "Failed to open device file ", Quote(mPath), "; errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
    throw lExc;
  }

  const off_t lPageSize = sysconf(_SC_PAGESIZE);
  const off_t lPageBaseAddr = (mOffset & ~(lPageSize-1));

  mMmapPtr = mmap(0, MAP_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, mFd, lPageBaseAddr);
  mMmapIOPtr = static_cast<uint8_t*>(mMmapPtr) + (mOffset - lPageBaseAddr);

  if (mMmapPtr == (void *)-1) {
    exception::MmapInitialisationError lExc;
    log(lExc, "Error occurred when mapping device file '" + mPath + "' to memory; errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
    throw lExc;
  }
}


void Mmap::File::close()
{
  if (mMmapPtr != NULL) {
    if (munmap(mMmapPtr, MAP_SIZE) == -1)
      log ( Error() , "mmap client for ", Quote(mPath), " encountered error when unmapping memory" );
    else {
      mMmapPtr = NULL;
      mMmapIOPtr = NULL;
    }
  }

  if (mFd != -1) {
    int rc = ::close(mFd);
    mFd = -1;
    if (rc == -1)
      log (Error(), "Failed to close file ", Quote(mPath), "; errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
  }
}


void Mmap::File::read(const uint32_t aAddr, const uint32_t aNrWords, std::vector<uint32_t>& aValues)
{
  if (mFd == -1)
    open();

  if (4 * aAddr + 4 * aNrWords > size_t(MAP_SIZE)) {
    exception::MmapInitialisationError lExc;
    log(lExc, "Attempted to read beyond the end of mapped memory for device file '" + mPath + "' (reading ", Integer(4 * aNrWords), " bytes from address ", Integer(4 * aAddr), ", i.e. ", Integer(uint32_t(4 * aAddr + 4 * aNrWords - MAP_SIZE)), " bytes beyond end of ", Integer(uint32_t(MAP_SIZE)), " mapped bytes.");
    throw lExc;
  }

  std::ostringstream lMessage;
  lMessage << "SIGBUS received during " << 4*aNrWords << "-byte read @ 0x" << std::hex << 4*aAddr << " in " << mPath;
  SigBusGuard lGuard;
  lGuard.protect([&]{
    uint8_t* lVirtAddr = static_cast<uint8_t*>(mMmapIOPtr) + off_t(4*aAddr);

    for (size_t i=0; i<aNrWords; i++) {
      aValues.push_back(*((uint32_t*) (lVirtAddr + 4 * i)));
    }
  }, lMessage.str());
}


void Mmap::File::write(const uint32_t aAddr, const std::vector<std::pair<const uint8_t*, size_t> >& aData)
{
  if (mFd == -1)
    open();

  size_t lNrBytes = 0;
  for (size_t i = 0; i < aData.size(); i++)
    lNrBytes += aData.at(i).second;

  assert((lNrBytes % 4) == 0);

  if (aAddr + lNrBytes > size_t(MAP_SIZE)) {
    exception::MmapInitialisationError lExc;
    log(lExc, "Attempted to write beyond the end of mapped memory for device file '" + mPath + "' (writing ", Integer(lNrBytes), " bytes at address ", Integer(aAddr), ", i.e. ", Integer(uint32_t(aAddr + lNrBytes - MAP_SIZE)), " bytes beyond end of ", Integer(uint32_t(MAP_SIZE)), " mapped bytes.");
    throw lExc;
  }

  char *allocated = NULL;
  posix_memalign((void **)&allocated, 4096/*alignment*/, lNrBytes + 4096);
  if (allocated == NULL) {
    exception::MmapCommunicationError lExc;
    log(lExc, "Failed to allocate ", Integer(lNrBytes + 4096), " bytes in File::write/2 function");
    throw lExc;
  }

  std::ostringstream lMessage;
  lMessage << "SIGBUS received during " << lNrBytes << "-byte write @ 0x" << std::hex << aAddr << " in " << mPath;
  SigBusGuard lGuard;
  lGuard.protect([&]{
    // data to write to register address
    char* buffer = allocated;
    size_t lNrBytesCopied = 0;
    for (size_t i = 0; i < aData.size(); i++) {
      memcpy(buffer + lNrBytesCopied, aData.at(i).first, aData.at(i).second);
      lNrBytesCopied += aData.at(i).second;
    }

    lNrBytesCopied = 0;
    while (lNrBytesCopied < lNrBytes) {
      char* lSrcPtr = buffer + lNrBytesCopied;
      char* lVirtAddr = static_cast<char*>(mMmapIOPtr) + aAddr + lNrBytesCopied;
      if ((lNrBytes - lNrBytesCopied) >= 8) {
        *((uint64_t*) lVirtAddr) = *(uint64_t*) lSrcPtr;
        lNrBytesCopied += 8;
      }
      else if ((lNrBytes - lNrBytesCopied) >= 4) {
        *((uint64_t*) lVirtAddr) = uint64_t(*(uint32_t*) lSrcPtr);
        lNrBytesCopied += 4;
      }
    }

    free(allocated);
  }, lMessage.str());
}




Mmap::Mmap ( const std::string& aId, const URI& aUri ) :
  IPbus< 2 , 0 > ( aId , aUri ),
  mConnected(false),
  mDeviceFile(aUri.mHostname, O_RDWR | O_SYNC),
  mNumberOfPages(0),
  mPageSize(0),
  mIndexNextPage(0),
  mPublishedReplyPageCount(0),
  mReadReplyPageCount(0),
  mAsynchronousException ( NULL )
{
  mSleepDuration = std::chrono::microseconds(50);

  for (const auto& lArg: aUri.mArguments) {
    if (lArg.first == "sleep") {
      mSleepDuration = std::chrono::microseconds(boost::lexical_cast<size_t>(lArg.second));
      log (Notice() , "mmap client with URI ", Quote (uri()), " : Inter-poll-/-interrupt sleep duration set to ", boost::lexical_cast<size_t>(lArg.second), " us by URI 'sleep' attribute");
    }
    else if (lArg.first == "offset") {
      const bool lIsHex = (lArg.second.find("0x") == 0) or (lArg.second.find("0X") == 0);
      const size_t lOffset = (lIsHex ? boost::lexical_cast<HexTo<size_t> >(lArg.second) : boost::lexical_cast<size_t>(lArg.second));
      mDeviceFile.setOffset(lOffset);
      log (Notice(), "mmap client with URI ", Quote (uri()), " : Address offset set to ", Integer(lOffset, IntFmt<hex>()));
    }
    else {
      log (Warning() , "Unknown attribute ", Quote (lArg.first), " used in URI ", Quote(uri()));
    }
  }
}


Mmap::~Mmap()
{
  disconnect();
}


void Mmap::implementDispatch ( std::shared_ptr< Buffers > aBuffers )
{
  log(Debug(), "mmap client (URI: ", Quote(uri()), ") : implementDispatch method called");

  if ( ! mConnected )
    connect();

  if ( mReplyQueue.size() == mNumberOfPages )
    read();
  write(aBuffers);
}


void Mmap::Flush( )
{
  log(Debug(), "mmap client (URI: ", Quote(uri()), ") : Flush method called");
  while ( !mReplyQueue.empty() )
    read();

}


void Mmap::dispatchExceptionHandler()
{
  // FIXME: Adapt to PCIe implementation
  log(Notice(), "mmap client ", Quote(id()), " (URI: ", Quote(uri()), ") : closing device files since exception detected");

  ClientInterface::returnBufferToPool ( mReplyQueue );
  disconnect();

  InnerProtocol::dispatchExceptionHandler();
}


uint32_t Mmap::getMaxSendSize()
{
  if ( ! mConnected )
    connect();

  return (mPageSize - 1) * 4;
}


uint32_t Mmap::getMaxReplySize()
{
  if ( ! mConnected )
    connect();

  return (mPageSize - 1) * 4;
}


void Mmap::connect()
{
  log ( Debug() , "mmap client is opening device file " , Quote ( mDeviceFile.getPath() ) );
  std::vector<uint32_t> lValues;
  mDeviceFile.read(0x0, 4, lValues);
  log (Info(), "Read status info from addr 0 (", Integer(lValues.at(0)), ", ", Integer(lValues.at(1)), ", ", Integer(lValues.at(2)), ", ", Integer(lValues.at(3)), "): ", PacketFmt((const uint8_t*)lValues.data(), 4 * lValues.size()));

  mNumberOfPages = lValues.at(0);
  mPageSize = std::min(uint32_t(4096), lValues.at(1));
  mIndexNextPage = lValues.at(2);
  mPublishedReplyPageCount = lValues.at(3);
  mReadReplyPageCount = mPublishedReplyPageCount;

  if (lValues.at(1) > 0xFFFF) {
    exception::MmapInitialisationError lExc;
    log (lExc, "Invalid page size, ", Integer(lValues.at(1)), ", reported in device file ", Quote(mDeviceFile.getPath()));
    throw lExc;
  }

  if (mIndexNextPage >= mNumberOfPages) {
    exception::MmapInitialisationError lExc;
    log (lExc, "Next page index, ", Integer(mIndexNextPage), ", reported in device file ", Quote(mDeviceFile.getPath()), " is inconsistent with number of pages, ", Integer(mNumberOfPages));
    throw lExc;
  }

  mConnected = true;
  log ( Info() , "mmap client connected to device at ", Quote(mDeviceFile.getPath()), "; FPGA has ", Integer(mNumberOfPages), " pages, each of size ", Integer(mPageSize), " words, index ", Integer(mIndexNextPage), " should be filled next" );
}


void Mmap::disconnect()
{
  mDeviceFile.close();
  mConnected = false;
}


void Mmap::write(const std::shared_ptr<Buffers>& aBuffers)
{
  log (Info(), "mmap client ", Quote(id()), " (URI: ", Quote(uri()), ") : writing ", Integer(aBuffers->sendCounter() / 4), "-word packet to page ", Integer(mIndexNextPage), " in ", Quote(mDeviceFile.getPath()));

  const uint32_t lHeaderWord = (0x10000 | (((aBuffers->sendCounter() / 4) - 1) & 0xFFFF));
  std::vector<std::pair<const uint8_t*, size_t> > lDataToWrite;
  lDataToWrite.push_back( std::make_pair(reinterpret_cast<const uint8_t*>(&lHeaderWord), sizeof lHeaderWord) );
  lDataToWrite.push_back( std::make_pair(aBuffers->getSendBuffer(), aBuffers->sendCounter()) );
  mDeviceFile.write(mIndexNextPage * 4 * mPageSize, lDataToWrite);

  log (Debug(), "Wrote " , Integer((aBuffers->sendCounter() / 4) + 1), " 32-bit words at address " , Integer(mIndexNextPage * 4 * mPageSize), " ... ", PacketFmt(lDataToWrite));

  mIndexNextPage = (mIndexNextPage + 1) % mNumberOfPages;
  mReplyQueue.push_back(aBuffers);
}


void Mmap::read()
{
  const size_t lPageIndexToRead = (mIndexNextPage - mReplyQueue.size() + mNumberOfPages) % mNumberOfPages;
  SteadyClock_t::time_point lStartTime = SteadyClock_t::now();

  if (mReadReplyPageCount == mPublishedReplyPageCount)
  {
    uint32_t lHwPublishedPageCount = 0x0;

    while ( true ) {
      std::vector<uint32_t> lValues;
      // FIXME : Improve by simply adding dmaWrite method that takes uint32_t ref as argument (or returns uint32_t)
      mDeviceFile.read(0, 4, lValues);
      lHwPublishedPageCount = lValues.at(3);
      log (Info(), "Read status info from addr 0 (", Integer(lValues.at(0)), ", ", Integer(lValues.at(1)), ", ", Integer(lValues.at(2)), ", ", Integer(lValues.at(3)), "): ", PacketFmt((const uint8_t*)lValues.data(), 4 * lValues.size()));

      if (lHwPublishedPageCount != mPublishedReplyPageCount) {
        mPublishedReplyPageCount = lHwPublishedPageCount;
        break;
      }
      // FIXME: Throw if published page count is invalid number

      if (SteadyClock_t::now() - lStartTime > std::chrono::microseconds(getBoostTimeoutPeriod().total_microseconds())) {
        exception::MmapTimeout lExc;
        log(lExc, "Next page (index ", Integer(lPageIndexToRead), " count ", Integer(mPublishedReplyPageCount+1), ") of mmap device '" + mDeviceFile.getPath() + "' is not ready after timeout period");
        throw lExc;
      }

      log(Debug(), "mmap client ", Quote(id()), " (URI: ", Quote(uri()), ") : Trying to read page index ", Integer(lPageIndexToRead), " = count ", Integer(mReadReplyPageCount+1), "; published page count is ", Integer(lHwPublishedPageCount), "; sleeping for ", mSleepDuration.count(), "us");
      if (mSleepDuration > std::chrono::microseconds(0))
        std::this_thread::sleep_for( mSleepDuration );
    }

    log(Info(), "mmap client ", Quote(id()), " (URI: ", Quote(uri()), ") : Reading page ", Integer(lPageIndexToRead), " (published count ", Integer(lHwPublishedPageCount), ", surpasses required, ", Integer(mReadReplyPageCount + 1), ")");
  }
  mReadReplyPageCount++;
  
  // PART 1 : Read the page
  std::shared_ptr<Buffers> lBuffers = mReplyQueue.front();
  mReplyQueue.pop_front();

  uint32_t lNrWordsToRead(lBuffers->replyCounter() >> 2);
  lNrWordsToRead += 1;
 
  std::vector<uint32_t> lPageContents;
  mDeviceFile.read(4 + lPageIndexToRead * mPageSize, lNrWordsToRead , lPageContents);
  log (Debug(), "Read " , Integer(lNrWordsToRead), " 32-bit words from address " , Integer(16 + lPageIndexToRead * 4 * mPageSize), " ... ", PacketFmt((const uint8_t*)lPageContents.data(), 4 * lPageContents.size()));

  // PART 2 : Transfer to reply buffer
  const std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( lBuffers->getReplyBuffer() );
  size_t lNrWordsInPacket = (lPageContents.at(0) >> 16) + (lPageContents.at(0) & 0xFFFF);
  if (lNrWordsInPacket != (lBuffers->replyCounter() >> 2))
    log (Warning(), "Expected reply packet to contain ", Integer(lBuffers->replyCounter() >> 2), " words, but it actually contains ", Integer(lNrWordsInPacket), " words");

  size_t lNrBytesCopied = 0;
  for (const auto& lBuffers: lReplyBuffers)
  {
    // Don't copy more of page than was written to, for cases when less data received than expected
    if ( lNrBytesCopied >= 4*lNrWordsInPacket)
      break;

    size_t lNrBytesToCopy = std::min( lBuffers.second , uint32_t(4*lNrWordsInPacket - lNrBytesCopied) );
    memcpy ( lBuffers.first, &lPageContents.at(1 + (lNrBytesCopied / 4)), lNrBytesToCopy );
    lNrBytesCopied += lNrBytesToCopy;
  }


  // PART 3 : Validate the packet contents
  mAsynchronousException = NULL;
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
    log ( *mAsynchronousException , "Exception caught during reply validation for mmap device with URI " , Quote ( this->uri() ) , "; what returned: " , Quote ( aExc.what() ) );
  }

  if ( mAsynchronousException )
  {
    mAsynchronousException->throwAsDerivedType();
  }
}


} // end ns uhal
