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

#ifndef _uhal_tests_tools_hpp_
#define _uhal_tests_tools_hpp_


#include <stdint.h>
#include <iostream>
#include <map>
#include <string>
#include <exception>
#include <sys/time.h>

#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>

#include "uhal/ConnectionManager.hpp"
#include "uhal/HwInterface.hpp"
#include "uhal/tests/PCIeDummyHardware.hpp"
#include "uhal/tests/TCPDummyHardware.hpp"
#include "uhal/tests/UDPDummyHardware.hpp"


#define UHAL_TESTS_DEFINE_CLIENT_TEST_CASES( test_suite_name , test_case_name , test_fixture, test_case_contents ) \
  \
  BOOST_AUTO_TEST_SUITE( ipbusudp_1_3 ) \
  \
  BOOST_AUTO_TEST_SUITE( test_suite_name ) \
  \
  BOOST_FIXTURE_TEST_CASE( test_case_name , test_fixture<IPBUS_1_3_UDP> ) \
  {\
    test_case_contents \
  }\
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  \
  BOOST_AUTO_TEST_SUITE( ipbustcp_1_3 ) \
  \
  BOOST_AUTO_TEST_SUITE( test_suite_name ) \
  \
  BOOST_FIXTURE_TEST_CASE( test_case_name , test_fixture<IPBUS_1_3_TCP> ) \
  {\
    test_case_contents \
  }\
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  \
  BOOST_AUTO_TEST_SUITE( chtcp_1_3 ) \
  \
  BOOST_AUTO_TEST_SUITE( test_suite_name ) \
  \
  BOOST_FIXTURE_TEST_CASE( test_case_name , test_fixture<IPBUS_1_3_CONTROLHUB> ) \
  {\
    test_case_contents \
  }\
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  \
  BOOST_AUTO_TEST_SUITE( ipbusudp_2_0 ) \
  \
  BOOST_AUTO_TEST_SUITE( test_suite_name ) \
  \
  BOOST_FIXTURE_TEST_CASE( test_case_name , test_fixture<IPBUS_2_0_UDP> ) \
  {\
    test_case_contents \
  }\
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  \
  BOOST_AUTO_TEST_SUITE( ipbustcp_2_0 ) \
  \
  BOOST_AUTO_TEST_SUITE( test_suite_name ) \
  \
  BOOST_FIXTURE_TEST_CASE( test_case_name , test_fixture<IPBUS_2_0_TCP> ) \
  {\
    test_case_contents \
  }\
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  \
  BOOST_AUTO_TEST_SUITE( chtcp_2_0 ) \
  \
  BOOST_AUTO_TEST_SUITE( test_suite_name ) \
  \
  BOOST_FIXTURE_TEST_CASE( test_case_name , test_fixture<IPBUS_2_0_CONTROLHUB> ) \
  {\
    test_case_contents \
  }\
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  \
  BOOST_AUTO_TEST_SUITE( ipbuspcie_2_0 ) \
  \
  BOOST_AUTO_TEST_SUITE( test_suite_name ) \
  \
  BOOST_FIXTURE_TEST_CASE( test_case_name , test_fixture<IPBUS_2_0_PCIE> ) \
  {\
    test_case_contents \
  }\
  \
  BOOST_AUTO_TEST_SUITE_END() \
  \
  BOOST_AUTO_TEST_SUITE_END()



namespace uhal {
class HwInterface;

namespace tests {

class PCIeDummyHardware;

enum DeviceType {
  IPBUS_1_3_UDP,
  IPBUS_1_3_TCP,
  IPBUS_1_3_CONTROLHUB,
  IPBUS_2_0_UDP, 
  IPBUS_2_0_TCP,
  IPBUS_2_0_CONTROLHUB,
  IPBUS_2_0_PCIE
};


class DummyHardwareRunnerInterface {
protected:
  DummyHardwareRunnerInterface(){}

public:
  virtual ~DummyHardwareRunnerInterface(){}

  virtual void setReplyDelay (const boost::chrono::microseconds& aDelay) = 0;
};


template <class DummyHardwareType>
class DummyHardwareRunner : public DummyHardwareRunnerInterface {
public:
  DummyHardwareRunner(const uint16_t aPort , const uint32_t aReplyDelay, const bool  aBigEndianHack) :
    mHw(aPort, aReplyDelay, aBigEndianHack),
    mHwThread( boost::bind(&DummyHardwareType::run, &mHw))
  {
  }

  ~DummyHardwareRunner()
  {
    mHw.stop();
    mHwThread.join();
  }

  void setReplyDelay(const boost::chrono::microseconds& aDelay)
  {
    mHw.setReplyDelay(aDelay);
  }

private:
  DummyHardwareType mHw;
  boost::thread mHwThread;
};


template <>
class DummyHardwareRunner<PCIeDummyHardware> : public DummyHardwareRunnerInterface {
public:
  DummyHardwareRunner(const std::string& aClientToHwFile, const std::string& aHwToClientFile, const uint32_t aReplyDelay, const bool aBigEndianHack) : 
    mHw(aClientToHwFile, aHwToClientFile, aReplyDelay, aBigEndianHack),
    mHwThread(boost::bind(&PCIeDummyHardware::run, &mHw))
  {
  }

  ~DummyHardwareRunner()
  {
    mHw.stop();
    mHwThread.join();
  }

  void setReplyDelay(const boost::chrono::microseconds& aDelay)
  {
    mHw.setReplyDelay(aDelay);
  }

private:
  PCIeDummyHardware mHw;
  boost::thread mHwThread; 
};



struct AbstractFixture {
protected:
  AbstractFixture() {}
  virtual ~AbstractFixture() {}

public:
  static std::string sConnectionFile;
};

template <DeviceType type> struct MinimalFixture;


template <DeviceType type>
struct MinimalFixture : public AbstractFixture {
  MinimalFixture();
  virtual ~MinimalFixture();

  uhal::HwInterface getHwInterface() const;

  static const DeviceType deviceType;

protected:
  uint16_t devicePort;
  std::string deviceId;
};


template <>
struct MinimalFixture<IPBUS_2_0_PCIE> : public AbstractFixture {
  MinimalFixture(); 
  ~MinimalFixture();

  uhal::HwInterface getHwInterface() const;

  static const DeviceType deviceType;
  std::string hardwareToClientFile;
  std::string clientToHardwareFile;
  std::string deviceId;
};

template <DeviceType type>
const DeviceType MinimalFixture<type>::deviceType = type;

template <>
MinimalFixture<IPBUS_1_3_UDP>::MinimalFixture();

template <>
MinimalFixture<IPBUS_1_3_TCP>::MinimalFixture();

template <>
MinimalFixture<IPBUS_1_3_CONTROLHUB>::MinimalFixture();

template <>
MinimalFixture<IPBUS_2_0_UDP>::MinimalFixture();

template <>
MinimalFixture<IPBUS_2_0_TCP>::MinimalFixture();

template <>
MinimalFixture<IPBUS_2_0_CONTROLHUB>::MinimalFixture();


template <DeviceType type>
MinimalFixture<type>::~MinimalFixture()
{
}

template <DeviceType type>
HwInterface MinimalFixture<type>::getHwInterface() const
{
  ConnectionManager manager(sConnectionFile);
  return manager.getDevice(deviceId);
}




template <DeviceType type>
struct DummyHardwareFixture : public MinimalFixture<type> {
  DummyHardwareFixture();
  ~DummyHardwareFixture();

  boost::shared_ptr<DummyHardwareRunnerInterface> hwRunner;

private:
  static DummyHardwareRunnerInterface* createRunner (const uint16_t aPort);
};


template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_1_3_UDP>::createRunner (const uint16_t aPort);

template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_1_3_CONTROLHUB>::createRunner (const uint16_t aPort);

template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_1_3_TCP>::createRunner (const uint16_t aPort);

template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_2_0_UDP>::createRunner (const uint16_t aPort);

template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_2_0_CONTROLHUB>::createRunner (const uint16_t aPort);

template <>
DummyHardwareRunnerInterface* DummyHardwareFixture<IPBUS_2_0_TCP>::createRunner (const uint16_t aPort);


template <DeviceType type>
DummyHardwareFixture<type>::DummyHardwareFixture() :
  MinimalFixture<type>(),
  hwRunner(createRunner(this->devicePort))
{
}


template <>
DummyHardwareFixture<IPBUS_2_0_CONTROLHUB>::DummyHardwareFixture();


template <DeviceType type>
DummyHardwareFixture<type>::~DummyHardwareFixture()
{
}


template <>
struct DummyHardwareFixture<IPBUS_2_0_PCIE> : public MinimalFixture<IPBUS_2_0_PCIE> {
  DummyHardwareFixture();
  ~DummyHardwareFixture();

  boost::shared_ptr<DummyHardwareRunnerInterface> hwRunner;
};


/// A very simple timer
class Timer
{
public:

  Timer() :m_start()
  {
    gettimeofday ( &m_start, NULL );
  }

  /// Returns number of elapsed seconds since the timer was instantiated.
  double elapsedSeconds()
  {
    timeval now;
    gettimeofday ( &now, NULL );
    time_t sec = now.tv_sec - m_start.tv_sec;
    suseconds_t usec = now.tv_usec - m_start.tv_usec;
    return static_cast<double> ( sec + usec/1000000. );
  }

private:
  timeval m_start;

}; /* End of class Timer */


double measureRxPerformance(const std::vector<ClientInterface*>& aClients, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, std::ostream* aOutStream);

double measureTxPerformance(const std::vector<ClientInterface*>& aClients, uint32_t aBaseAddr, uint32_t aDepth, size_t aNrIterations, bool aDispatchEachIteration, std::ostream* aOutStream);


} // end ns tests
} // end ns uhal

#endif
