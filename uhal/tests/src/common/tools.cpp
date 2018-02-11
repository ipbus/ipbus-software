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


#include "uhal/tests/DummyHardware.hpp"


namespace uhal {
namespace tests {


DummyHardwareRunner::DummyHardwareRunner(DummyHardwareInterface* aHw) :
  mHw(aHw),
  mHwThread(boost::bind(&DummyHardwareInterface::run, aHw))
{
}

DummyHardwareRunner::~DummyHardwareRunner()
{
  mHw->stop();
  mHwThread.join();
}

void DummyHardwareRunner::setReplyDelay(const boost::chrono::microseconds& aDelay)
{
  mHw->setReplyDelay(aDelay);
}


double measureReadLatency(ClientInterface& aClient, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, bool aVerbose)
{
  return measureReadLatency(std::vector<ClientInterface*>(1, &aClient), aBaseAddr, aDepth, aNrIterations, aDispatchEachIteration, aVerbose);
}


double measureReadLatency(const std::vector<ClientInterface*>& aClients, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, bool aVerbose)
{
  typedef std::vector<ClientInterface*>::const_iterator ClientIterator_t;
  Timer myTimer;

  for ( unsigned i = 0; i < aNrIterations ; ++i )
  {
    if ( aVerbose )
    {
      std::cout << "Iteration " << i << std::endl;
    }

    for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
    {
      (*lIt)->readBlock ( aBaseAddr, aDepth, defs::NON_INCREMENTAL );
    }

    if ( aDispatchEachIteration )
    {
      for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
      {
        (*lIt)->dispatch();
      }
    }
  }

  if ( !aDispatchEachIteration )
  {
    for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
    {
      (*lIt)->dispatch();
    }
  }

  return myTimer.elapsedSeconds();
}


double measureWriteLatency(ClientInterface& aClient, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, bool aVerbose)
{
  return measureWriteLatency(std::vector<ClientInterface*>(1, &aClient), aBaseAddr, aDepth, aNrIterations, aDispatchEachIteration, aVerbose);
}


double measureWriteLatency(const std::vector<ClientInterface*>& aClients, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, bool aVerbose)
{
  typedef std::vector<ClientInterface*>::const_iterator ClientIterator_t;

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
    for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
    {
      (*lIt)->writeBlock ( aBaseAddr, sendBuffer, defs::NON_INCREMENTAL );
    }

    if ( aDispatchEachIteration )
    {
      for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
      {
        (*lIt)->dispatch();
      }
    }
  }

  if ( !aDispatchEachIteration )
  {
    for (ClientIterator_t lIt = aClients.begin(); lIt != aClients.end(); lIt++)
    {
      (*lIt)->dispatch();
    }
  }

  return myTimer.elapsedSeconds();
}


} // end ns tests
} // end ns uhal
