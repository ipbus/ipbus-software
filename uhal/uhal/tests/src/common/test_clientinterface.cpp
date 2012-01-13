#define BOOST_TEST_DYN_LINK

#define BOOST_TEST_MODULE IPBus UDP Client Test Cases

#include <boost/test/unit_test.hpp>

#include "ipbus/ipbus.hpp"

#include "ipbus/test_tools.hpp"

#include <string>
#include <cstdlib>


BOOST_AUTO_TEST_CASE(connection_manager) {
  ipbus::ConnectionManager manager("addr/*_connection_config.*");
  ipbus::ClientInterface& client(manager.getDevice("hcal.crate1.slot1"));
  
  BOOST_CHECK(client.ping());

  std::vector<std::string> ids = manager.getDevices("^hcal.*");
  for(std::vector<std::string>::const_iterator i(ids.begin()); i!=ids.end(); ++i) {
    BOOST_CHECK(i->find("hcal") != std::string::npos);
    ipbus::ClientInterface& client(manager.getDevice(*i));
    
    BOOST_CHECK_MESSAGE(client.ping(), "Device '" + *i + "' not available at '" + client.getURL() + "'");
  }
    
}


BOOST_AUTO_TEST_CASE(validated_memory) {
  ipbus::ValMem v1(0x33);
  
  //instantiation without a value is an invalid memory
  BOOST_CHECK(!v1.valid());
  BOOST_CHECK_THROW(static_cast<uint32_t>(v1),std::exception);

  v1 = 23;
  BOOST_CHECK_NO_THROW(v1.value());

  v1.valid(false);
  BOOST_CHECK_THROW(static_cast<uint32_t>(v1),std::exception);

  ipbus::ValMem v2(32);
  BOOST_CHECK(!v2.valid());
  BOOST_CHECK_THROW(static_cast<uint32_t>(v2),std::exception);
  
  v2.valid(true);
  BOOST_CHECK_NO_THROW(static_cast<uint32_t>(v2));

  v1 = v2;
  BOOST_CHECK(v1.valid() == v2.valid());
  BOOST_CHECK(v1 == v2);

  

}

BOOST_AUTO_TEST_CASE(read_write) {

  std::string host = ipbus::test::getParam<std::string>("host","unknow.cern.ch");
  int port = ipbus::test::getParam<int>("port",1234);
  uint32_t addr = ipbus::test::getParam<uint32_t>("addr",0x1234);

  ipbus::Client<ipbus::UDP> c = ipbus::Client<ipbus::UDP>(host,port);
  ipbus::ClientInterface& client(c);
  
  BOOST_CHECK(c.ping());

  ipbus::ValMem pre = rand();

  client.write(addr,pre);
  client.dispatch();
  
  ipbus::ValMem post = client.read(addr);   //alternative read(addr,&post) is unsafe 
  
  BOOST_CHECK(!post.valid());

  client.dispatch();

  BOOST_CHECK(post.valid());
  BOOST_CHECK_MESSAGE(pre == post,"double step write-write");  //automatic cast

  pre = rand();
  client.write(addr,pre);
  post = client.read(addr);   
  
  BOOST_CHECK(!post.valid());
  
  client.dispatch();
  
  BOOST_CHECK(post.valid());
  BOOST_CHECK_MESSAGE(pre == post,"single step write-read");  

}


BOOST_AUTO_TEST_CASE(read_modify_write) {
  std::string host = ipbus::test::getParam<std::string>("host","unknow.cern.ch");
  int port = ipbus::test::getParam<int>("port",1234);
  uint32_t addr = ipbus::test::getParam<uint32_t>("addr",0x1234);

  ipbus::Client<ipbus::UDP> c = ipbus::Client<ipbus::UDP>(host,port);
  ipbus::ClientInterface& client = c;

  BOOST_CHECK(c.ping());

  ipbus::ValMem pre = rand();

  ipbus::ValMem post = client.read_modify_write(addr,pre);
  BOOST_CHECK(!post.valid());

  client.dispatch();
  
  BOOST_CHECK(post.valid());
  BOOST_CHECK(pre == post);  

}

BOOST_AUTO_TEST_CASE(block_read_and_write) {
  std::string host = ipbus::test::getParam<std::string>("host","unknow.cern.ch");
  int port = ipbus::test::getParam<int>("port",1234);
  uint32_t addr = ipbus::test::getParam<uint32_t>("addr",0x1234);

  ipbus::Client<ipbus::UDP> c = ipbus::Client<ipbus::UDP>(host,port);
  ipbus::ClientInterface& client = c;
  
  BOOST_CHECK(c.ping());

  size_t SIZE=1024;
  std::vector<uint32_t> pre;
  for(size_t i=0;i!=SIZE;++i)
    pre.push_back(rand());

  client.write(addr,pre);
  client.dispatch();

  std::vector<ipbus::ValMem>  post = client.read(addr,SIZE);
  BOOST_CHECK(!post.begin()->valid());
  BOOST_CHECK(!post.rbegin()->valid());

  client.dispatch();

  BOOST_CHECK(post.begin()->valid());
  BOOST_CHECK(post.rbegin()->valid());
  BOOST_CHECK(post.size() == SIZE);
  BOOST_CHECK(*pre.begin() == *post.begin()); // inhomogenous collection can not be tested with BOOST macros
  BOOST_CHECK(*pre.rbegin() == *post.rbegin());

  pre.clear();
  for(size_t i=0;i!=SIZE;++i)
    pre.push_back(rand());

  client.write(addr,pre);
  post = client.read(addr,SIZE);
  client.dispatch();
  
  BOOST_CHECK(post.size() == SIZE);
  BOOST_CHECK(*pre.begin() == *post.begin()); 
  BOOST_CHECK(*pre.rbegin() == *post.rbegin());

  pre.clear();
  for(size_t i=0;i!=SIZE;++i)
    pre.push_back(rand());

  post = client.read_modify_write(addr,pre);
  BOOST_CHECK(!post.begin()->valid());
  BOOST_CHECK(!post.rbegin()->valid());
  client.dispatch();
  
  BOOST_CHECK(post.begin()->valid());
  BOOST_CHECK(post.rbegin()->valid());
  BOOST_CHECK(pre.size() == post.size() );
  BOOST_CHECK(*pre.begin() == *post.begin());
  BOOST_CHECK(*pre.rbegin() == *post.rbegin());



}


