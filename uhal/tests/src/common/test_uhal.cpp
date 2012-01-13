//#define BOOST_TEST_DYN_LINK

//#define BOOST_TEST_MODULE uHAL Test Cases

//#include <boost/test/unit_test.hpp>

#include "uhal/uhal.hpp"

#include <vector>
#include <string>
#include <cstdlib>

void hwInterface_creation() {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw=manager.getDevice("hcal.crate1.slot1");
  //BOOST_CHECK(manager.ping());

  std::vector<std::string> ids = manager.getDevices("hcal.crate1");
  for(std::vector<std::string>::const_iterator i(ids.begin()); i != ids.end(); ++i)
    //BOOST_CHECK(manager.getDevice(*i).getClient().ping();
    ;
}

void navigation_and_traversal_test() {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  
  uhal::Node node1(hw.getNode("SYSTEM.REGISTER"));
  uhal::Node node2(hw.getNode("SYSTEM").getNode("REGISTER"));

  
  uhal::NodePermission a = node1.getPermission();
  uint32_t mask = node1.getMask();
  std::string id = node1.getId();
  //BOOST_CHECK(id=="SYSTEM.REGISTER");

  uhal::Node branch(hw.getNode("SYSTEM"));
  //BOOST_CHECK_THROW(branch.getPermission(),std::exception);
  //BOOST_CHECK_THROW(branch.getMask(),std::exception);
  //BOOST_CHECK_THROW(branch.read(),std::exception);
  //BOOST_CHECK_THROW(branch.write(rand()),std::exception);

  std::vector<std::string> children = branch.getNodes();
}

void read_test() {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");

  //read register
  uhal::ValMem mem1 = hw.getNode("SYSTEM").getNode("TTC").getNode("ADDRESS").read();
  uhal::ValMem mem2 = hw.getNode("SYSTEM.TTC.ADDRESS").read();
  hw.dispatch();
  
  //BOOST_CHECK(mem1.getValid() && mem2.getValid());
  //BOOST_CHECK(mem1.getValue() == mem2.getValue());

  //read memory
  uint32_t SIZE=1024;
  std::vector<uhal::ValMem> block1 = hw.getNode("SYSTEM").getNode("MEMORY").readBlock(SIZE);
  std::vector<uhal::ValMem> block2 = hw.getNode("SYSTEM.MEMORY").readBlock(SIZE);
  hw.dispatch();
  
  //BOOST_CHECK(block1.size() == SIZE);
  //BOOST_CHECK(block2.size() == SIZE);
  //BOOST_CHECK(block1.begin()->getValid() && block2.begin()->getValid());
  //BOOST_CHECK(block1.begin()->getValue() == block2.begin()->getValue());
  //BOOST_CHECK(block1.rbegin()->getValid() && block2.rbegin()->getValid());
  //BOOST_CHECK(block1.rbegin()->getValue() == block2.rbegin()->getValue());

  //read FIFO
  block1.clear();
  block1 = hw.getNode("SYSTEM").getNode("MEMORY").readBlock(SIZE,uhal::NON_INCREMENTAL);
  block2.clear();
  block2 = hw.getNode("SYSTEM.MEMORY").readBlock(SIZE,uhal::NON_INCREMENTAL);
  hw.dispatch();
  
  //BOOST_CHECK(block1.size() == SIZE);
  //BOOST_CHECK(block2.size() == SIZE);
  //BOOST_CHECK(block1.begin()->getValid() && block2.begin()->getValid());
  //BOOST_CHECK(block1.begin()->getValue() == block2.begin()->getValue());
  //BOOST_CHECK(block1.rbegin()->getValid() && block2.rbegin()->getValid());
  //BOOST_CHECK(block1.rbegin()->getValue() == block2.rbegin()->getValue());
  
}

void write_test() {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  
  //write register
  uint32_t val = static_cast<uint32_t>(rand());
  hw.getNode("SYSTEM").getNode("TTC").getNode("ADDRESS").write(val);
  uhal::ValMem mem = hw.getNode("SYSTEM").getNode("TTC").getNode("ADDRESS").read();

  hw.dispatch();
  
  //BOOST_CHECK(mem.getValid());
  //BOOST_CHECK(mem.getValue() == val);

  //write memory
  uint32_t SIZE=1024;
  std::vector<uint32_t> vals;
  for(uint32_t i=0;i!=SIZE;i++)
    vals.push_back(static_cast<uint32_t>(rand()));

  hw.getNode("SYSTEM.MEMORY").writeBlock(vals);
  std::vector<uhal::ValMem> block = hw.getNode("SYSTEM").getNode("MEMORY").readBlock(SIZE);
  
  hw.dispatch();
  
  //BOOST_CHECK(block.size() == SIZE);
  //BOOST_CHECK(block.begin()->getValid() && block.rbegin()->getValid());
  //BOOST_CHECK(block.begin()->getValue() == *vals.begin());
  //BOOST_CHECK(block.rbegin()->getValue() == *vals.rbegin());

  //write FIFO
  vals.clear();
  for(uint32_t i=0;i!=SIZE;i++)
    vals.push_back(static_cast<uint32_t>(rand()));

  hw.getNode("SYSTEM").getNode("MEMORY").writeBlock(vals,uhal::NON_INCREMENTAL);
  block = hw.getNode("SYSTEM").getNode("MEMORY").readBlock(SIZE,uhal::NON_INCREMENTAL);
  hw.dispatch();
  
}

void read_write_mask() {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  
  uhal::ValMem mem = hw.getNode("REGISTER_MASK_0xF0").read();
  //BOOST_CHECK(mem.getValue() >=0 && mem.getValue() <=0xF);

  uint32_t val = 0x3;
  hw.getNode("REGISTER_MASK_0xF0").write(val);
  mem = hw.getNode("REGISTER_MASK_0xF0").read();
  hw.dispatch();
  //BOOST_CHECK(mem.getValue() == val);
}

void read_write_permissions() {
  uhal::ConnectionManager manager("addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");

  //read write register 
  uint32_t val = static_cast<uint32_t>(rand());
  hw.getNode("READ_WRITE_REGISTER").write(val);
  uhal::ValMem mem = hw.getNode("READ_WRITE_REGISTER").read();

  hw.dispatch();
  //BOOST_CHECK(mem.getValue() == val);

  //read only register
  mem = hw.getNode("READ_ONLY_REGISTER").read();
  hw.dispatch();

  //BOOST_CHECK_THROW(hw.getNode("READ_ONLY_REGISTER").write(rand()),std::exception);

  //write only register
  val = static_cast<uint32_t>(rand());
  hw.getNode("WRITE_ONLY_REGISTER").write(val);
  hw.dispatch();
  
  //BOOST_CHECK_THROW(hw.getNode("WRITE_ONLY_REGISTER").read(),std::exception);
  
  //read write memory
  uint32_t SIZE = 1024;
  std::vector<uint32_t> vals;
  for(uint32_t i=0;i!=SIZE;i++)
    vals.push_back(static_cast<uint32_t>(rand()));
  
  hw.getNode("READ_WRITE_MEMORY").writeBlock(vals);
  std::vector<uhal::ValMem> block = hw.getNode("READ_WRITE_MEMORY").readBlock(SIZE);
  hw.dispatch();
  
  //BOOST_CHECK(block.size() == vals.size());
  //BOOST_CHECK(block.begin()->getValue() == *vals.begin());
  //BOOST_CHECK(block.rbegin()->getValue() == *vals.rbegin());
  
  //read only memory
  //BOOST_CHECK_THROW(hw.getNode("READ_ONLY_MEMORY").writeBlock(vals),std::exception);

  block = hw.getNode("READ_ONLY_MEMORY").readBlock(SIZE);
  hw.dispatch();
  //BOOST_CHECK(block.size() == vals.size());
  //BOOST_CHECK(block.begin()->getValue() == *vals.begin());
  //BOOST_CHECK(block.rbegin()->getValue() == *vals.rbegin());
  
  //write only memory
  //BOOST_CHECK_THROW(hw.getNode("WRITE_ONLY_MEMORY").readBlock(SIZE),std::exception);
  hw.getNode("WRITE_ONLY_MEMORY").writeBlock(vals);
  hw.dispatch();
  

 }

int main(int argc,char* argv[]) {
  hwInterface_creation();
  navigation_and_traversal_test();
  read_test();
  write_test();
  read_write_mask();
  read_write_permissions();
}
