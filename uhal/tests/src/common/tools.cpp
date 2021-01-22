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

#include "uhal/tests/tools.hpp"


#include <chrono>
#include <fcntl.h>
#include <vector>

#include "uhal/ProtocolPCIe.hpp"
#include "uhal/tests/DummyHardware.hpp"


namespace uhal {
namespace tests {


DummyHardwareRunner::DummyHardwareRunner(DummyHardwareInterface* aHw) :
  mHw(aHw),
  mHwThread([aHw] () {aHw->run();})
{
}

DummyHardwareRunner::~DummyHardwareRunner()
{
  mHw->stop();
  mHwThread.join();
}

void DummyHardwareRunner::setReplyDelay(const std::chrono::microseconds& aDelay)
{
  mHw->setReplyDelay(aDelay);
}


double measureReadLatency(ClientInterface& aClient, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, bool aVerbose)
{
  return measureReadLatency(std::vector<ClientInterface*>(1, &aClient), aBaseAddr, aDepth, aNrIterations, aDispatchEachIteration, aVerbose);
}


double measureReadLatency(const std::vector<ClientInterface*>& aClients, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, bool aVerbose)
{
  Timer myTimer;

  for ( unsigned i = 0; i < aNrIterations ; ++i )
  {
    if ( aVerbose )
    {
      std::cout << "Iteration " << i << std::endl;
    }

    for (const auto& c: aClients)
      c->readBlock ( aBaseAddr, aDepth, defs::NON_INCREMENTAL );

    if ( aDispatchEachIteration )
    {
      for (const auto& c: aClients)
        c->dispatch();
    }
  }

  if ( !aDispatchEachIteration )
  {
    for (const auto& c: aClients)
      c->dispatch();
  }

  return myTimer.elapsedSeconds();
}


double measureWriteLatency(ClientInterface& aClient, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, bool aVerbose)
{
  return measureWriteLatency(std::vector<ClientInterface*>(1, &aClient), aBaseAddr, aDepth, aNrIterations, aDispatchEachIteration, aVerbose);
}


double measureWriteLatency(const std::vector<ClientInterface*>& aClients, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, bool aVerbose)
{
  // Send buffer - lots of "cafebabe" (in little-endian)
  std::vector<uint32_t> sendBuffer ( aDepth, 0xbebafeca );
  Timer myTimer;

  for ( unsigned i = 0; i < aNrIterations ; ++i )
  {
    if ( aVerbose )
    {
      std::cout << "Iteration " << i << std::endl;
    }

    // Create the packet
    for (const auto& c: aClients)
      c->writeBlock ( aBaseAddr, sendBuffer, defs::NON_INCREMENTAL );

    if ( aDispatchEachIteration )
    {
      for (const auto& c: aClients)
        c->dispatch();
    }
  }

  if ( !aDispatchEachIteration )
  {
    for (const auto& c: aClients)
      c->dispatch();
  }

  return myTimer.elapsedSeconds();
}


double measureFileReadLatency(const std::string& aFilePath, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aVerbose)
{
  PCIe::File lFile(aFilePath, O_RDWR | O_NONBLOCK);
  lFile.open();

  std::vector<uint32_t> lRecvBuffer;
  lFile.read(aBaseAddr, aDepth, lRecvBuffer);
  lRecvBuffer.clear();
  
  Timer myTimer;
  for ( unsigned i = 0; i < aNrIterations ; ++i )
  {
    if ( aVerbose )
    {
      std::cout << "Iteration " << i << std::endl;
    }

    lFile.read(aBaseAddr, aDepth, lRecvBuffer);
    lRecvBuffer.clear();
  }

  return myTimer.elapsedSeconds();
}


double measureFileWriteLatency(const std::string& aFilePath, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aVerbose)
{
  PCIe::File lFile(aFilePath, O_RDWR);
  std::vector<uint32_t> lSendBuffer(aDepth, 0x0);
  lFile.write(aBaseAddr, lSendBuffer);

  Timer myTimer;
  for ( unsigned i = 0; i < aNrIterations ; ++i )
  {
    if ( aVerbose )
    {
      std::cout << "Iteration " << i << std::endl;
    }

    lFile.write(aBaseAddr, lSendBuffer);
  }

  return myTimer.elapsedSeconds();
}


} // end ns tests
} // end ns uhal
