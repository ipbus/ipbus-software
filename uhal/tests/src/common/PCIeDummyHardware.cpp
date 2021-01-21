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

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

---------------------------------------------------------------------------
*/

#include "uhal/tests/PCIeDummyHardware.hpp"


#include <chrono>
#include <errno.h>
#include <fcntl.h>
#include <thread>
#include <string>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "uhal/log/LogLevels.hpp"
#include "uhal/log/log_inserters.integer.hpp"
#include "uhal/log/log_inserters.quote.hpp"
#include "uhal/log/log.hpp"
#include "uhal/ProtocolPCIe.hpp"


namespace uhal {
namespace tests {

PCIeDummyHardware::PCIeDummyHardware(const std::string& aDevicePathHostToFPGA, const std::string& aDevicePathFPGAToHost, const uint32_t& aReplyDelay, const bool& aBigEndianHack) :
  DummyHardware<2, 0>(aReplyDelay, aBigEndianHack),
  mDevicePathHostToFPGA(aDevicePathHostToFPGA),
  mDevicePathFPGAToHost(aDevicePathFPGAToHost),
  mNumberOfPages(3),
  mWordsPerPage(360),
  mNextPageIndex(0),
  mPublishedPageCount(0),
  mStop(false),
  mDeviceFileHostToFPGA(-1),
  mDeviceFileFPGAToHost(-1)
{
  log(Debug(), "PCIe dummy hardware is creating client-to-device named PIPE ", Quote (mDevicePathHostToFPGA));
  int rc = mkfifo(mDevicePathHostToFPGA.c_str(), 0666);
  if ( rc != 0 ) {
    std::runtime_error lExc("Cannot create FIFO; mkfifo returned " + std::to_string(rc) + ", errno=" + std::to_string(errno));
    throw lExc;
  }

  mDeviceFileHostToFPGA = open(mDevicePathHostToFPGA.c_str(), O_RDONLY | O_NONBLOCK); /* O_NONBLOCK so that open does not hang */
  if ( mDeviceFileHostToFPGA < 0 ) {
    std::runtime_error lExc("Problem opening host-to-FPGA device file '" + mDevicePathHostToFPGA + "', errno=" + std::to_string(errno) + " (dummy hw)");
    throw lExc;
  }

  // Set the new flags with O_NONBLOCK masked out
  int lFileFlags = fcntl(mDeviceFileHostToFPGA, F_GETFL);
  fcntl(mDeviceFileHostToFPGA, F_SETFL, lFileFlags & ~O_NONBLOCK);

  log(Debug(), "PCIe dummy hardware is creating device-to-client file ", Quote (mDevicePathFPGAToHost));
  mDeviceFileFPGAToHost = open(mDevicePathFPGAToHost.c_str(), O_RDWR | O_CREAT | O_DIRECTORY, 0666 /* permission */);
  if ( mDeviceFileFPGAToHost < 0 ) {
    std::runtime_error lExc("Cannot open FPGA-to-host device file '" + mDevicePathFPGAToHost + "' (dummy hw)");
    throw lExc;
  }

  std::vector<uint32_t> lFPGAToHostData(4 + mNumberOfPages * mWordsPerPage, 0);
  lFPGAToHostData.at(0) = mNumberOfPages;
  lFPGAToHostData.at(1) = mWordsPerPage;
  lFPGAToHostData.at(2) = mNextPageIndex;
  lFPGAToHostData.at(3) = mPublishedPageCount;

  fileWrite(mDeviceFileFPGAToHost, 0, lFPGAToHostData);

  log(Notice(), "Starting IPbus 2.0 PCIe dummy hardware ", Quote(mDevicePathHostToFPGA), ", ", Quote(mDevicePathFPGAToHost), "; ", Integer(mNumberOfPages), " pages, ", Integer(mWordsPerPage), " words per page");
}


PCIeDummyHardware::~PCIeDummyHardware()
{
  log(Notice(), "Destroying IPbus 2.0 PCIe dummy hardware ", Quote(mDevicePathHostToFPGA), ", ", Quote(mDevicePathFPGAToHost));

  if (close(mDeviceFileHostToFPGA))
    log(Fatal(), "Problem occurred when closing ", Quote(mDevicePathHostToFPGA), " during dummy hardware destruction");
  if (remove(mDevicePathHostToFPGA.c_str()))
    log(Fatal(), "Problem occurred when removing ", Quote(mDevicePathHostToFPGA), " during dummy hardware destruction");

  if (close(mDeviceFileFPGAToHost))
    log(Fatal(), "Problem occurred when closing ", Quote(mDevicePathFPGAToHost), " during dummy hardware destruction");
  if (remove(mDevicePathFPGAToHost.c_str()))
    log(Fatal(), "Problem occurred when removing ", Quote(mDevicePathFPGAToHost), " during dummy hardware destruction");
}


void PCIeDummyHardware::run()
{
  log(Info(), "Entering run method for IPbus 2.0 PCIe dummy hardware ", Quote(mDevicePathHostToFPGA), ", ", Quote(mDevicePathFPGAToHost));

  while ( !mStop ) {
    int lNrBytes;
    int lRC = ioctl(mDeviceFileHostToFPGA, FIONREAD, &lNrBytes);
    log (Debug(), "PCIeDummyHardware::run  -  ioctl returns ", Integer(lRC), ", ", Integer(lNrBytes), " bytes available");
    assert (lRC == 0);

    if (lNrBytes == 0) {
      std::this_thread::sleep_for(std::chrono::microseconds(50));
      continue;
    }

    log(Debug(), "IPbus 2.0 PCIe dummy hardware ", Quote(mDevicePathHostToFPGA), ", ", Quote(mDevicePathFPGAToHost), " : data ready; reading 'packet length' header");
    // FIXME: Define template method that reads sizeof(T) bytes and returns T
    std::vector<uint32_t> lPageHeader;
    fifoRead(mDeviceFileHostToFPGA, 0x0, 1, lPageHeader);

    const uint32_t lNrWordsInRequestPacket = (lPageHeader.at(0) >> 16) + (lPageHeader.at(0) & 0xFFFF);

    if (lNrWordsInRequestPacket >= mWordsPerPage) {
      log(Fatal(), "PCIeDummyHardware::run  -  returning early since receiving ", Integer(lNrWordsInRequestPacket), "-word packet, but there's only ", Integer(mWordsPerPage), " words per page");
      return;
    }

    log(Info(), "IPbus 2.0 PCIe dummy hardware ", Quote(mDevicePathHostToFPGA), ", ", Quote(mDevicePathFPGAToHost), " : reading ", Integer(lNrWordsInRequestPacket), "-word packet");
    mReceive.clear();
    fifoRead(mDeviceFileHostToFPGA, 0x0, lNrWordsInRequestPacket, mReceive);

    // std::cout << "Received:" << std::endl;
    // for(size_t i=0; i<mReceive.size(); i++)
    //   std::cout  << " @" << i << "   0x" << std::hex << mReceive.at(i) << std::dec << std::endl;

    mReply.clear();
    AnalyzeReceivedAndCreateReply(4 * lNrWordsInRequestPacket);

    log(Info(), "IPbus 2.0 PCIe dummy hardware ", Quote(mDevicePathHostToFPGA), ", ", Quote(mDevicePathFPGAToHost), " : writing ", Integer(mReply.size()), "-word reply to page ", Integer(mNextPageIndex));
    lPageHeader.clear();
    lPageHeader.push_back(0x10000 | ((mReply.size() - 1) & 0xFFFF));

    fileWrite(mDeviceFileFPGAToHost, 4 + mNextPageIndex * mWordsPerPage, lPageHeader); // FIXME: Define template fileWrite method that takes const T& as final argument, so that don't have to define vector
    fileWrite(mDeviceFileFPGAToHost, 4 + mNextPageIndex * mWordsPerPage + 1, mReply);

    mNextPageIndex = (mNextPageIndex + 1) % mNumberOfPages;
    mPublishedPageCount++;

    std::vector<uint32_t> lStatusBlock(4, 0);
    lStatusBlock.at(0) = mNumberOfPages;
    lStatusBlock.at(1) = mWordsPerPage;
    lStatusBlock.at(2) = mNextPageIndex;
    lStatusBlock.at(3) = mPublishedPageCount;
    fileWrite(mDeviceFileFPGAToHost, 0, lStatusBlock);
  }

  log(Info(), "Exiting run method for IPbus 2.0 PCIe dummy hardware ", Quote(mDevicePathHostToFPGA), ", ", Quote(mDevicePathFPGAToHost));
}


void PCIeDummyHardware::stop()
{
  log(Info(), "Stopping IPbus 2.0 PCIe dummy hardware ", Quote(mDevicePathHostToFPGA), ", ", Quote(mDevicePathFPGAToHost));
  mStop = true;
}


void PCIeDummyHardware::fifoRead(int aFileDescriptor, const uint32_t, const uint32_t aNrWords, std::vector<uint32_t>& aValues)
{
  char *allocated = NULL;
  posix_memalign((void **)&allocated, 4096/*alignment*/, 4*aNrWords + 4096);
  assert(allocated);

  /* select AXI MM address */
  char* buffer = allocated;
  memset(buffer, 0, 4 * aNrWords);

  /* read data from AXI MM into buffer using SGDMA */
  size_t lNrBytesRead = 0;
  do {
    if (lNrBytesRead != 0)
      log (Fatal(), "fifo calling ::read multiple times to get all expected ", Integer(aNrWords * 4), " bytes (only ", Integer(lNrBytesRead), " so far)");
    int rc = ::read(aFileDescriptor, buffer + lNrBytesRead, 4*aNrWords - lNrBytesRead);
    if (rc <= 0)
      log (Fatal(), "fifo   -   read returned ", Integer(rc), ", bugger! errno = ", Integer(errno), ", meaning ", Quote (strerror(errno)));
    assert (rc > 0);
    lNrBytesRead += rc;
  } while (lNrBytesRead < (4 * aNrWords) );
  if (lNrBytesRead > (4 * aNrWords))
    log (Fatal(), "fifo   -   read ", Integer(lNrBytesRead), " but only wanted to read  ", Integer(aNrWords * 4), " bytes");

  aValues.insert(aValues.end(), reinterpret_cast<uint32_t*>(buffer), reinterpret_cast<uint32_t*>(buffer)+ aNrWords);

  free(allocated);
}



bool PCIeDummyHardware::fileWrite(int aFileDescriptor, const uint32_t aAddr, const std::vector<uint32_t>& aValues)
{
  return fileWrite(aFileDescriptor, aAddr, reinterpret_cast<const uint8_t*>(aValues.data()), 4 * aValues.size());
}


bool PCIeDummyHardware::fileWrite(int aFileDescriptor, const uint32_t aAddr, const uint8_t* const aPtr, const size_t aNrBytes)
{
  assert((aNrBytes % 4) == 0);

  char *allocated = NULL;
  posix_memalign((void **)&allocated, 4096/*alignment*/, aNrBytes + 4096);
  assert(allocated);

  // data to write to register address
  char* buffer = allocated;
  memcpy(buffer, aPtr, aNrBytes);

  /* select AXI MM address */
  off_t off = lseek(aFileDescriptor, 4*aAddr, SEEK_SET);
  if ( off != 4*aAddr) {
    exception::PCIeCommunicationError lExc;
    log(lExc, "Offset returned by lseek, ", Integer(off), ", does not match that requested, ", Integer(4*aAddr), " (in preparation for write of ", Integer(aNrBytes), " bytes)");
    throw lExc;
  }

  /* write buffer to AXI MM address using SGDMA */
  int rc = ::write(aFileDescriptor, buffer, aNrBytes);
  assert((rc >= 0) && (size_t(rc) == aNrBytes));

  free(allocated);

  return true;
}


} // end ns tests
} // end ns uhal
