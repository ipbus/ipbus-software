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
#include <chrono>
#include <cstdlib>
#include <errno.h>
#include <fcntl.h>
#include <iomanip>                                          // for operator<<
#include <iostream>                                         // for operator<<
#include <sys/file.h>
#include <sys/stat.h>
#include <stdlib.h>                                         // for size_t, free
#include <stdio.h>
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
  mFlags(aFlags),
  mLocked(false),
  mBufferSize(0),
  mBuffer(NULL)
{
}


PCIe::File::~File()
{
  free(mBuffer);
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
    if (haveLock())
      unlock();
    int rc = ::close(mFd);
    mFd = -1;
    if (rc == -1)
      log (Error(), "Failed to close file ", Quote(mPath), "; errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
  }
}



void PCIe::File::createBuffer(const size_t aNrBytes)
{
  if (mBuffer != NULL) {
    if (mBufferSize >= aNrBytes)
      return;
    else {
      free(mBuffer);
      mBuffer = NULL;
      mBufferSize = 0;
    }
  }

  posix_memalign((void**)&mBuffer, 4096/*alignment*/, aNrBytes + 4096);
  if (mBuffer == NULL) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Failed to allocate ", Integer(aNrBytes + 4096), " bytes in PCIe::File::createBuffer");
    throw lExc;
  }

  mBufferSize=aNrBytes+4096;
}


void PCIe::File::read(const uint32_t aAddr, const uint32_t aNrWords, std::vector<uint32_t>& aValues)
{
  if (mFd == -1)
    open();

  createBuffer(4 * aNrWords);

  /* select AXI MM address */
  off_t off = lseek(mFd, 4*aAddr, SEEK_SET);
  if ( off != off_t(4 * aAddr)) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Offset returned by lseek, ", Integer(off), ", does not match that requested, ", Integer(4*aAddr), " (in preparation for read of ", Integer(aNrWords), " words)");
    throw lExc;
  }

  /* read data from AXI MM into buffer using SGDMA */
  int rc = ::read(mFd, mBuffer, 4*aNrWords);
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

  aValues.insert(aValues.end(), reinterpret_cast<uint32_t*>(mBuffer), reinterpret_cast<uint32_t*>(mBuffer)+ aNrWords);
}


void PCIe::File::write(const uint32_t aAddr, const std::vector<uint32_t>& aValues)
{
  write(4 * aAddr, reinterpret_cast<const uint8_t*>(aValues.data()), 4 * aValues.size());
}


void PCIe::File::write(const uint32_t aAddr, const uint8_t* const aPtr, const size_t aNrBytes)
{
  if (mFd == -1)
    open();

  assert((aNrBytes % 4) == 0);

  createBuffer(aNrBytes);

  // data to write to register address
  memcpy(mBuffer, aPtr, aNrBytes);

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
  int rc = ::write(mFd, mBuffer, aNrBytes);
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
}


void PCIe::File::write(const uint32_t aAddr, const std::vector<std::pair<const uint8_t*, size_t> >& aData)
{
  if (mFd == -1)
    open();

  size_t lNrBytes = 0;
  for (size_t i = 0; i < aData.size(); i++)
    lNrBytes += aData.at(i).second;

  assert((lNrBytes % 4) == 0);

  createBuffer(lNrBytes);

  // data to write to register address
  size_t lNrBytesCopied = 0;
  for (size_t i = 0; i < aData.size(); i++) {
    memcpy(mBuffer + lNrBytesCopied, aData.at(i).first, aData.at(i).second);
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
  int rc = ::write(mFd, mBuffer, lNrBytes);
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
}


bool PCIe::File::haveLock() const
{
  return mLocked;
}


void PCIe::File::lock()
{
  if ( flock(mFd, LOCK_EX) == -1 ) {
    exception::MutexError lExc;
    log(lExc, "Failed to lock device file ", Quote(mPath), "; errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
    throw lExc;
  }
  mLocked = true;
}


void PCIe::File::unlock()
{
  if ( flock(mFd, LOCK_UN) == -1 ) {
    log(Warning(), "Failed to unlock device file ", Quote(mPath), "; errno=", Integer(errno), ", meaning ", Quote (strerror(errno)));
  }
  else
    mLocked = false;
}




PCIe::RobustMutex::RobustMutex() :
  mCount(0),
  mSessionActive(false)
{
  pthread_mutexattr_t lAttr;

  int s = pthread_mutexattr_init(&lAttr);
  if (s != 0) {
    exception::MutexError lExc;
    log(lExc, "Error code ", Integer(s), " (", strerror(s), ") returned in mutex attr initialisation");
    throw lExc;
  }

  s = pthread_mutexattr_setpshared(&lAttr, PTHREAD_PROCESS_SHARED);
  if (s != 0) {
    exception::MutexError lExc;
    log(lExc, "Error code ", Integer(s), " (", strerror(s), ") returned by pthread_mutexattr_setpshared");
    throw lExc;
  }

  s = pthread_mutexattr_setrobust(&lAttr, PTHREAD_MUTEX_ROBUST);
  if (s != 0) {
    exception::MutexError lExc;
    log(lExc, "Error code ", Integer(s), " (", strerror(s), ") returned by pthread_mutexattr_setrobust");
    throw lExc;
  }

  s = pthread_mutex_init(&mMutex, &lAttr);
  if (s != 0) {
    exception::MutexError lExc;
    log(lExc, "Error code ", Integer(s), " (", strerror(s), ") returned in mutex initialisation");
    throw lExc;
  }
}


PCIe::RobustMutex::~RobustMutex()
{
}


void PCIe::RobustMutex::lock()
{
  int s = pthread_mutex_lock(&mMutex);
  bool lLastOwnerDied = (s == EOWNERDEAD);
  if (lLastOwnerDied)
    s = pthread_mutex_consistent(&mMutex);

  if (s != 0) {
    exception::MutexError lExc;
    log(lExc, "Error code ", Integer(s), " (", strerror(s), ") returned when ", lLastOwnerDied ? "making mutex state consistent" : "locking mutex");
    throw lExc;
  }
}


void PCIe::RobustMutex::unlock()
{
  int s = pthread_mutex_unlock(&mMutex);
  if (s != 0)
    log(Error(), "Error code ", Integer(s), " (", strerror(s), ") returned when unlocking mutex");
}


uint64_t PCIe::RobustMutex::getCounter() const
{
  return mCount;
}


bool PCIe::RobustMutex::isActive() const
{
  return mSessionActive;
}


void PCIe::RobustMutex::startSession()
{
  mCount++;
  mSessionActive = true;
}

void PCIe::RobustMutex::endSession()
{
  mSessionActive = false;
}




template <class T>
PCIe::SharedObject<T>::SharedObject(const std::string& aName) :
  mName(aName),
  mSharedMem(boost::interprocess::open_or_create, aName.c_str(), 1024, 0x0, boost::interprocess::permissions(0666)),
  mObj(mSharedMem.find_or_construct<T>(boost::interprocess::unique_instance)())
{
}

template <class T>
PCIe::SharedObject<T>::~SharedObject()
{
  // boost::interprocess::shared_memory_object::remove(mName.c_str());
}

template <class T>
T* PCIe::SharedObject<T>::operator->()
{
  return mObj;
}

template <class T>
T& PCIe::SharedObject<T>::operator*()
{
  return *mObj;
}




std::string PCIe::getSharedMemName(const std::string& aPath)
{
  std::string lSanitizedPath(aPath);
  std::replace(lSanitizedPath.begin(), lSanitizedPath.end(), '/', ':');

  return "/uhal::ipbuspcie-2.0::" + lSanitizedPath;
}


PCIe::PCIe ( const std::string& aId, const URI& aUri ) :
  IPbus< 2 , 0 > ( aId , aUri ),
  mConnected(false),
  mDeviceFileHostToFPGA(aUri.mHostname.substr(0, aUri.mHostname.find(",")), O_RDWR ),
  mDeviceFileFPGAToHost(aUri.mHostname.substr(aUri.mHostname.find(",")+1), O_RDWR | O_NONBLOCK  /* for read might need O_RDWR | O_NONBLOCK */),
  mDeviceFileFPGAEvent("", O_RDONLY),
  mIPCMutex(getSharedMemName(mDeviceFileHostToFPGA.getPath())),
  mXdma7seriesWorkaround(false),
  mUseInterrupt(false),
  mNumberOfPages(0),
  mMaxInFlight(0),
  mPageSize(0),
  mMaxPacketSize(0),
  mIndexNextPage(0),
  mPublishedReplyPageCount(0),
  mReadReplyPageCount(0)
{
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

  mSleepDuration = std::chrono::microseconds(mUseInterrupt ? 0 : 50);

  for (const auto& lArg: aUri.mArguments) {
    if (lArg.first == "events") {
      if (mUseInterrupt) {
        exception::PCIeInitialisationError lExc;
        log(lExc, "PCIe client URI ", Quote(uri()), ": 'events' attribute is specified multiple times");
        throw lExc;
      }

      mUseInterrupt = true;
      mDeviceFileFPGAEvent.setPath(lArg.second);
      log (Info() , "PCIe client with URI ", Quote (uri()), " is configured to use interrupts");
    }
    else if (lArg.first == "sleep") {
      mSleepDuration = std::chrono::microseconds(boost::lexical_cast<size_t>(lArg.second));
      log (Notice() , "PCIe client with URI ", Quote (uri()), " : Inter-poll-/-interrupt sleep duration set to ", boost::lexical_cast<size_t>(lArg.second), " us by URI 'sleep' attribute");
    }
    else if (lArg.first == "max_in_flight") {
      mMaxInFlight = boost::lexical_cast<size_t>(lArg.second);
      log (Notice() , "PCIe client with URI ", Quote (uri()), " : 'Maximum number of packets in flight' set to ", boost::lexical_cast<size_t>(lArg.second), " by URI 'max_in_flight' attribute");
    }
    else if (lArg.first == "max_packet_size") {
      mMaxPacketSize = boost::lexical_cast<size_t>(lArg.second);
      log (Notice() , "PCIe client with URI ", Quote (uri()), " : 'Maximum packet size (in 32-bit words) set to ", boost::lexical_cast<size_t>(lArg.second), " by URI 'max_packet_size' attribute");
    }
    else if (lArg.first == "xdma_7series_workaround") {
      mXdma7seriesWorkaround = true;
      log (Notice() , "PCIe client with URI ", Quote (uri()), " : Adjusting size of PCIe reads to a few fixed sizes as workaround for 7-series xdma firmware bug");
    }
    else
      log (Warning() , "Unknown attribute ", Quote (lArg.first), " used in URI ", Quote(uri()));
  }
}


PCIe::~PCIe()
{
  disconnect();
}


void PCIe::implementDispatch ( std::shared_ptr< Buffers > aBuffers )
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

  mDeviceFileHostToFPGA.unlock();

  IPCScopedLock_t lLockGuard(*mIPCMutex);
  mIPCMutex->endSession();
}


void PCIe::dispatchExceptionHandler()
{
  log(Notice(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : closing device files since exception detected");

  ClientInterface::returnBufferToPool ( mReplyQueue );

  mDeviceFileHostToFPGA.unlock();

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
  IPCScopedLock_t lLockGuard(*mIPCMutex);
  connect(lLockGuard);
}


void PCIe::connect(IPCScopedLock_t& aGuard)
{
  // Read current value of session counter when reading status info from FPGA
  // (So that can check whether this info is up-to-date later on, when sending next request packet)
  mIPCExternalSessionActive = mIPCMutex->isActive() and (not mDeviceFileHostToFPGA.haveLock());
  mIPCSessionCount = mIPCMutex->getCounter();

  log ( Debug() , "PCIe client is opening device file " , Quote ( mDeviceFileFPGAToHost.getPath() ) , " (device-to-client)" );
  mDeviceFileFPGAToHost.open();

  std::vector<uint32_t> lValues;
  mDeviceFileFPGAToHost.read(0x0, 4, lValues);
  aGuard.unlock();
  log ( Debug(), "Read status info (", Integer(lValues.at(0)), ", ", Integer(lValues.at(1)), ", ", Integer(lValues.at(2)), ", ", Integer(lValues.at(3)), "): ", PacketFmt((const uint8_t*)lValues.data(), 4 * lValues.size()));

  mNumberOfPages = lValues.at(0);
  if ( (mMaxInFlight == 0) or (mMaxInFlight > mNumberOfPages) )
    mMaxInFlight = mNumberOfPages;
  mPageSize = lValues.at(1);
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

  log ( Debug() , "PCIe client is opening device file " , Quote ( mDeviceFileHostToFPGA.getPath() ) , " (client-to-device)" );
  mDeviceFileHostToFPGA.open();

  mDeviceFileFPGAToHost.createBuffer(4 * mPageSize);
  mDeviceFileHostToFPGA.createBuffer(4 * mPageSize);

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


void PCIe::write(const std::shared_ptr<Buffers>& aBuffers)
{
  if (not mDeviceFileHostToFPGA.haveLock()) {
    mDeviceFileHostToFPGA.lock();

    IPCScopedLock_t lGuard(*mIPCMutex);
    mIPCMutex->startSession();
    mIPCSessionCount++;

    // If these two numbers don't match, another client/process has sent packets
    // more recently than this client has, so must re-read status info
    if (mIPCExternalSessionActive or (mIPCMutex->getCounter() != mIPCSessionCount)) {
      connect(lGuard);
    }
  }

  log (Info(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : writing ", Integer(aBuffers->sendCounter() / 4), "-word packet to page ", Integer(mIndexNextPage), " in ", Quote(mDeviceFileHostToFPGA.getPath()));

  const uint32_t lHeaderWord = (0x10000 | (((aBuffers->sendCounter() / 4) - 1) & 0xFFFF));
  std::vector<std::pair<const uint8_t*, size_t> > lDataToWrite;
  lDataToWrite.push_back( std::make_pair(reinterpret_cast<const uint8_t*>(&lHeaderWord), sizeof lHeaderWord) );
  lDataToWrite.push_back( std::make_pair(aBuffers->getSendBuffer(), aBuffers->sendCounter()) );

  IPCScopedLock_t lGuard(*mIPCMutex);
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

        if (SteadyClock_t::now() - lStartTime > std::chrono::microseconds(getBoostTimeoutPeriod().total_microseconds())) {
          exception::PCIeTimeout lExc;
          log(lExc, "Next page (index ", Integer(lPageIndexToRead), " count ", Integer(mPublishedReplyPageCount+1), ") of PCIe device '" + mDeviceFileHostToFPGA.getPath() + "' is not ready after timeout period");
          throw lExc;
        }

        log(Debug(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Waiting for interrupt; sleeping for ", mSleepDuration.count(), "us");
        if (mSleepDuration > std::chrono::microseconds(0))
          std::this_thread::sleep_for( mSleepDuration );

      } // end of while (true)

      log(Info(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Reading page ", Integer(lPageIndexToRead), " (interrupt received)");
    }
    else
    {
      uint32_t lHwPublishedPageCount = 0x0;

      std::vector<uint32_t> lValues;
      while ( true ) {
        // FIXME : Improve by simply adding fileWrite method that takes uint32_t ref as argument (or returns uint32_t)
        IPCScopedLock_t lGuard(*mIPCMutex);
        mDeviceFileFPGAToHost.read(0, (mXdma7seriesWorkaround ? 8 : 4), lValues);
        lHwPublishedPageCount = lValues.at(3);
        log (Debug(), "Read status info from addr 0 (", Integer(lValues.at(0)), ", ", Integer(lValues.at(1)), ", ", Integer(lValues.at(2)), ", ", Integer(lValues.at(3)), "): ", PacketFmt((const uint8_t*)lValues.data(), 4 * lValues.size()));

        if (lHwPublishedPageCount != mPublishedReplyPageCount) {
          mPublishedReplyPageCount = lHwPublishedPageCount;
          break;
        }
        // FIXME: Throw if published page count is invalid number

        if (SteadyClock_t::now() - lStartTime > std::chrono::microseconds(getBoostTimeoutPeriod().total_microseconds())) {
          exception::PCIeTimeout lExc;
          log(lExc, "Next page (index ", Integer(lPageIndexToRead), " count ", Integer(mPublishedReplyPageCount+1), ") of PCIe device '" + mDeviceFileHostToFPGA.getPath() + "' is not ready after timeout period");
          throw lExc;
        }

        log(Debug(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Trying to read page index ", Integer(lPageIndexToRead), " = count ", Integer(mReadReplyPageCount+1), "; published page count is ", Integer(lHwPublishedPageCount), "; sleeping for ", mSleepDuration.count(), "us");
        if (mSleepDuration > std::chrono::microseconds(0))
          std::this_thread::sleep_for( mSleepDuration );
        lValues.clear();
      }

      log(Info(), "PCIe client ", Quote(id()), " (URI: ", Quote(uri()), ") : Reading page ", Integer(lPageIndexToRead), " (published count ", Integer(lHwPublishedPageCount), ", surpasses required, ", Integer(mReadReplyPageCount + 1), ")");
    }
  }
  mReadReplyPageCount++;
  
  // PART 1 : Read the page
  std::shared_ptr<Buffers> lBuffers = mReplyQueue.front();
  mReplyQueue.pop_front();

  uint32_t lNrWordsToRead(lBuffers->replyCounter() >> 2);
  if(mXdma7seriesWorkaround and (lNrWordsToRead % 32 == 0 || lNrWordsToRead % 32 == 28 || lNrWordsToRead < 4))
    lNrWordsToRead += 4;
  lNrWordsToRead += 1;
 
  std::vector<uint32_t> lPageContents;
  IPCScopedLock_t lGuard(*mIPCMutex);
  mDeviceFileFPGAToHost.read(4 + lPageIndexToRead * mPageSize, lNrWordsToRead , lPageContents);
  lGuard.unlock();
  log (Debug(), "Read " , Integer(lNrWordsToRead), " 32-bit words from address " , Integer(16 + lPageIndexToRead * 4 * mPageSize), " ... ", PacketFmt((const uint8_t*)lPageContents.data(), 4 * lPageContents.size()));

  // PART 2 : Transfer to reply buffer
  const std::deque< std::pair< uint8_t* , uint32_t > >& lReplyBuffers ( lBuffers->getReplyBuffer() );
  size_t lNrWordsInPacket = (lPageContents.at(0) >> 16) + (lPageContents.at(0) & 0xFFFF);
  if (lNrWordsInPacket != (lBuffers->replyCounter() >> 2))
    log (Warning(), "Expected reply packet to contain ", Integer(lBuffers->replyCounter() >> 2), " words, but it actually contains ", Integer(lNrWordsInPacket), " words");

  size_t lNrBytesCopied = 0;
  for (const auto& lBuffer: lReplyBuffers)
  {
    // Don't copy more of page than was written to, for cases when less data received than expected
    if ( lNrBytesCopied >= 4*lNrWordsInPacket)
      break;

    size_t lNrBytesToCopy = std::min( lBuffer.second , uint32_t(4*lNrWordsInPacket - lNrBytesCopied) );
    memcpy ( lBuffer.first, &lPageContents.at(1 + (lNrBytesCopied / 4)), lNrBytesToCopy );
    lNrBytesCopied += lNrBytesToCopy;
  }


  // PART 3 : Validate the packet contents
  uhal::exception::exception* lExc = NULL;
  try
  {
    lExc = ClientInterface::validate ( lBuffers );
  }
  catch ( exception::exception& aExc )
  {
    exception::ValidationError lExc2;
    log ( lExc2 , "Exception caught during reply validation for PCIe device with URI " , Quote ( this->uri() ) , "; what returned: " , Quote ( aExc.what() ) );
    throw lExc2;
  }

  if (lExc != NULL)
    lExc->throwAsDerivedType();
}


} // end ns uhal
