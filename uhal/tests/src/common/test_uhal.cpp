#define BOOST_TEST_DYN_LINK

#define BOOST_TEST_MODULE uHAL Test Cases

#include <boost/test/unit_test.hpp>

#include "uhal/uhal.hpp"

#include "uhal/test_tools.hpp"

#include <vector>
#include <string>
#include <cstdlib>

BOOST_AUTO_TEST_CASE(HwInterface_creation) {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw=manager.getDevice("hcal.crate1.slot1");
  BOOST_CHECK(hw.getClient().ping());

  std::vector<std::string> ids = manager.getDevices("hcal.crate1");
  for(std::vector<std::string>::const_iterator i(ids.begin()); i != ids.end(); ++i)
    BOOST_CHECK(manager.getDevice(*i).getClient().ping());
  
}

BOOST_AUTO_TEST_CASE(navigation_and_traversal_test) {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  
  uhal::Node node1(hw.getNode("SYSTEM.REGISTER"));
  uhal::Node node2(hw.getNode("SYSTEM").getNode("REGISTER"));
  BOOST_CHECK(node1 == node2);
  
  uhal::NodePermission a = node1.getPermission();
  uint32_t addr = node1.getAddress();
  uint32_t mask = node1.getMask();
  std::string id = node1.getId();
  BOOST_CHECK(id="SYSTEM.REGISTER");

  uhal::Node branch(hw.getNode("SYSTEM"));
  BOOST_CHECK_THROW(branch.getPermission(),std::exception);
  BOOST_CHECK_THROW(branch.getAddress(),std::exception);
  BOOST_CHECK_THROW(branch.getMask(),std::exception);
  BOOST_CHECK_THROW(branch.read(),std::exception);
  BOOST_CHECK_THROW(branch.write(rand()),std::exception);

  std::vector<std::string> children = branch.getNodes();
}

BOOST_AUTO_TEST_CASE(read_test) {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");

  //read register
  uhal::ValMem mem1 = hw.getNode("SYSTEM").getNode("TTC").getNode("ADDRESS").read();
  uhal::ValMem mem2 = hw.getNode("SYSTEM.TTC.ADDRESS").read();
  hw.dispatch();
  
  BOOST_CHECK(mem1.getValid() && mem2.getValid());
  BOOST_CHECK(mem1.getValue() == mem2.getValue());
  BOOST_CHECK(mem1.getAddress() == mem2.getAddress());
  BOOST_CHECK(mem1.getAddress() == hw.getNode("SYSTEM.TTC.ADDRESS").getAddress());

  //read memory
  uint32_t SIZE=1024;
  std::vector<uhal::ValMem> block1 = hw.getNode("SYSTEM").getNode("MEMORY").readBlock(SIZE);
  std::vector<uhal::ValMem> block2 = hw.getNode("SYSTEM.MEMORY").readBlock(SIZE);
  hw.dispatch();
  
  BOOST_CHECK(block1.size() == SIZE);
  BOOST_CHECK(block2.size() == SIZE);
  BOOST_CHECK(block1.begin()->getValid() && block2.begin()->getValid());
  BOOST_CHECK(block1.begin()->getValue() == block2.begin()->getValue());
  BOOST_CHECK(block1.begin()->getAddress() == block2.begin()->getAddress());
  BOOST_CHECK(block1.rbegin()->getValid() && block2.rbegin()->getValid());
  BOOST_CHECK(block1.rbegin()->getValue() == block2.rbegin()->getValue());
  BOOST_CHECK(block1.rbegin()->getAddress() == block2.rbegin()->getAddress());

  //read FIFO
  block1.clear();
  block1 = hw.getNode("SYSTEM").getNode("MEMORY").readBlock(SIZE,uhal::NON_INCREMENTAL);
  block2.clear();
  block2 = hw.getNode("SYSTEM.MEMORY").readBlock(SIZE,uhal::NON_INCREMENTAL);
  hw.dispatch();
  
  BOOST_CHECK(block1.size() == SIZE);
  BOOST_CHECK(block2.size() == SIZE);
  BOOST_CHECK(block1.begin()->getValid() && block2.begin()->getValid());
  BOOST_CHECK(block1.begin()->getValue() == block2.begin()->getValue());
  BOOST_CHECK(block1.begin()->getAddress() == block2.begin()->getAddress());
  BOOST_CHECK(block1.rbegin()->getValid() && block2.rbegin()->getValid());
  BOOST_CHECK(block1.rbegin()->getValue() == block2.rbegin()->getValue());
  BOOST_CHECK(block1.rbegin()->getAddress() == block2.rbegin()->getAddress());
  
}

BOOST_AUTO_TEST_CASE(write_test) {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  
  //write register
  uint32_t val = static_cast<uint32_t>(rand());
  hw.getNode("SYSTEM").getNode("TTC").getNode("ADDRESS").write(val);
  uhal::ValMem mem = hw.getNode("SYSTEM").getNode("TTC").getNode("ADDRESS").read();

  hw.dispatch();
  
  BOOST_CHECK(mem.getValid());
  BOOST_CHECK(mem.getValue() == val);

  //write memory
  uint32_t SIZE=1024;
  std::vector<uint32_t> vals;
  for(i=0;i!=SIZE;i++)
    vals.push_back(static_cast<uint32_t>(rand()));

  hw.getNode("SYSTEM.MEMORY").writeBlock(vals);
  std::vector<uhal::ValMem> block = hw.getNode("SYSTEM").getNode("MEMORY").readBlock(SIZE);
  
  hw.dispatch();
  
  BOOST_CHECK(block.size() == SIZE);
  BOOST_CHECK(block.begin()->getAddress() == hw.getNode("SYSTEM.MEMORY").getAddress());
  BOOST_CHECK(block.rbegin()->getAddress() == hw.getNode("SYSTEM.MEMORY").getAddress()+SIZE-1);
  BOOST_CHECK(block.begin()->getValid() && block.rbegin()->getValid());
  BOOST_CHECK(block.begin()->getValue() == *vals.begin());
  BOOST_CHECK(block.rbegin()->getValue() == *vals.rbegin());

  //write FIFO

  
}

//mask performs also the shift right/left on the read/write operations
BOOST_AUTO_TEST_CASE(read_write_mask) {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  
  uhal::ValMem mem = hw.getNode("REGISTER_MASK_0xF0").read();
  BOOST_CHECK(mem.getValue() >=0 && mem.getValue() <=0xF);

  uint32_t val = 0x3;
  hw.getNode("REGISTER_MASK_0xF0").write(val);
  mem = hw.getNode("REGISTER_MASK_0xF0").read();
  hw.dispatch();
  BOOST_CHECK(mem.getValue == val);
}

BOOST_AUTO_TEST_CASE(read_write_permissions) {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");

  //read write register 
  uint32_t val = static_Cast<uint32_t>(rand());
  hw.getNode("READ_WRITE_REGISTER").write(val);
  uhal::ValMem mem = hw.getNode("READ_WRITE_REGISTER").read();

  hw.dispatch();
  BOOST_CHECK(mem.getValue() == val);

  //read only register
  uhal::ValMem mem = hw.getNode("READ_ONLY_REGISTER").read();
  hw.dispatch();

  BOOST_CHECK_THROW(hw.get("READ_ONLY_REGISTER").write(rand()),std::exception);

  //write only register
  val = static_Cast<uint32_t>(rand());
  hw.getNode("WRITE_ONLY_REGISTER").write(val);
  hw.dispatch();
  
  BOOST_CHECK_THROW(hw.get("WRITE_ONLY_REGISTER").read(),std::exception);
  
  //read write memory
  uint32_t SIZE = 1024;
  std::vector<uint32_t> vals;
  for(i=0;i!=SIZE;i++)
    vals.push_back(static_cast<uint32_t>(rand()));
  
  hw.getNode("READ_WRITE_MEMORY").writeBlock(vals);
  std::vector<uhal::ValMem> block = hw.getNode("READ_WRITE_MEMORY").readBlock(SIZE);
  hw.dispatch();
  
  BOOST_CHECK(block.size() == vals.size());
  BOOST_CHECK(block.begin()->getValue() == vals.begin()->getValue());
  BOOST_CHECK(block.rbegin()->getValue() == vals.rbegin()->getValue());
  
  //read only memory
  BOOST_CHECK_THROW(hw.getNode("READ_ONLY_MEMORY").write(vals),std::exception);

  std::vector<uhal::ValMem> block = hw.getNode("READ_ONLY_MEMORY").readBlock(SIZE);
  hw.dispatch();
  BOOST_CHECK(block.size() == vals.size());
  BOOST_CHECK(block.begin()->getValue() == vals.begin()->getValue());
  BOOST_CHECK(block.rbegin()->getValue() == vals.rbegin()->getValue());
  
  //write only memory
  BOOST_CHECK_THROW(hw.getNode("READ_WRITE_MEMORY").readBlock(SIZE),std::exception);
  hw.getNode("WRITE_ONLY_MEMORY").write(vals);
  hw.dispatch();
  

 }

BOOST_AUTO_TEST_CASE(critical_section) {
}

BOOST_AUTO_TEST_CASE(new_protocol) {
}
