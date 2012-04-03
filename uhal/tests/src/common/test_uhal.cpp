//#define BOOST_TEST_DYN_LINK

//#define BOOST_TEST_MODULE uHAL Test Cases

//#include <boost/test/unit_test.hpp>

#include "uhal/uhal.hpp"

#include <vector>
#include <string>
#include <cstdlib>

void hwInterface_creation() {

  // uhal::ConnectionManager manager("     file://tests/addr*/*connections.xml   ; file://~/connection*.xml  ;  ;;; ; ;  ");// http://svnweb.cern.ch/world/wsvn/cactus/trunk/uhal/tests/addr/connections.xml?op=dl&rev=head      ");
  
  // uhal::HwInterface hw=manager.getDevice("hcal.crate1.slot1");
  // //BOOST_CHECK(manager.ping());
  
  // // manager.getDevice("hcal.crate1.slot2");
  
  // hw.dispatch();
  
  // std::vector<std::string> ids = manager.getDevices("hcal.crate1.*");
  // for(std::vector<std::string>::const_iterator i(ids.begin()); i != ids.end(); ++i)
    // //BOOST_CHECK(manager.getDevice(*i).getClient().ping();
    // ;
}

void rawClientAccess(){
 uhal::ConnectionManager manager("file://tests/addr/connections.xml");
  uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  
  //write register
  uint32_t val = static_cast<uint32_t>(rand());
  
   hw.getClient()->write( 0xBA5EADD4 , val );
  uhal::ValWord< uint32_t > mem = hw.getClient()->read( 0xBA5EADD4 );
   
  hw.dispatch();
  
  if( mem==val ){
	pantheios::log_INFORMATIONAL(  "SINGLE WORD WRITE/READ : ALL GOOD" ) ;
  }else{
	pantheios::log_ERROR( "MISMATCH : Source " , pantheios::integer( val , pantheios::fmt::fullHex | 10 ) , " vs. Found " , pantheios::integer( mem.value()  ,  pantheios::fmt::fullHex | 10 ) );
	pantheios::log_LOCATION;
	throw 0;
  }
  
  // //write memory
  uint32_t SIZE=1024;
  std::vector<uint32_t> vals;
  for(uint32_t i=0;i!=SIZE;i++)
    vals.push_back(static_cast<uint32_t>(rand()));
    // vals.push_back(static_cast<uint32_t>(i));

  hw.getClient()->writeBlock( 0xBA5EADD4 , vals );
  uhal::ValVector< uint32_t > block = hw.getClient()->readBlock( 0xBA5EADD4 , SIZE );
  
  hw.dispatch();
  
  
  uhal::ValVector< uint32_t >::const_iterator lReadIt = block.begin();
  std::vector< uint32_t >::const_iterator lSourceIt = vals.begin();
  int count = 0;
  
  for( ; lReadIt != block.end() && lSourceIt != vals.end() ; ++lReadIt , ++lSourceIt , ++count ){
	if( *lReadIt != *lSourceIt ){
		pantheios::log_ERROR( "MISMATCH AT " , pantheios::integer(count) , " : Source " , pantheios::integer( *lSourceIt , pantheios::fmt::fullHex | 10 ) , " vs. Found " , pantheios::integer( *lReadIt  ,  pantheios::fmt::fullHex | 10 ) );
		pantheios::log_LOCATION;
		throw 0;
	}
  }
  
  pantheios::log_INFORMATIONAL(  "INCREMENTING BLOCK WRITE/READ : ALL GOOD" ) ;
  
  // //BOOST_CHECK(block.size() == SIZE);
  // //BOOST_CHECK(block.begin()->valid() && block.rbegin()->valid());
  // //BOOST_CHECK(*block.begin() == *vals.begin());
  // //BOOST_CHECK*(block.rbegin() == *vals.rbegin());

  // //write FIFO
  vals.clear();
  for(uint32_t i=0;i!=SIZE;i++)
    vals.push_back(static_cast<uint32_t>(rand()));
    // vals.push_back(static_cast<uint32_t>(i));
	  
  hw.getClient()->writeBlock( 0xBA5EADD4 , vals , uhal::defs::NON_INCREMENTAL );
  /*uhal::ValVector< uint32_t >*/ block = hw.getClient()->readBlock( 0xBA5EADD4 , SIZE , uhal::defs::NON_INCREMENTAL );

  hw.dispatch();
  
  /*uhal::ValVector< uint32_t >::const_iterator*/ lReadIt = block.begin();
  /*std::vector< uint32_t >::const_iterator*/ lSourceIt = vals.end();
  lSourceIt--;
  
  /*int*/ count = 0;
  
  for( ; lReadIt != block.end() ; ++lReadIt , ++count ){
	if( *lReadIt != *lSourceIt ){
		pantheios::log_ERROR( "MISMATCH AT " , pantheios::integer(count) , " : Source " , pantheios::integer( *lSourceIt , pantheios::fmt::fullHex | 10 ) , " vs. Found " , pantheios::integer( *lReadIt  ,  pantheios::fmt::fullHex | 10 ) );
		pantheios::log_LOCATION;
		throw 0;
	}
  }
  
  pantheios::log_INFORMATIONAL(  "NON-INCREMENTING BLOCK WRITE/READ : ALL GOOD" ) ;

  
  
  uint32_t expected( (*lSourceIt&0x00C0FFEE)|0xF00DF00D );

  /*uhal::ValWord< uint32_t >*/ mem = hw.getClient()->rmw_bits( 0xBA5EADD4 , 0x00C0FFEE , 0xF00DF00D );
  hw.dispatch();
  
  if( mem == expected ){
	pantheios::log_INFORMATIONAL(  "RMW-BITS : ALL GOOD" ) ;
  }else{
	pantheios::log_ERROR( "MISMATCH : Source " , pantheios::integer( expected , pantheios::fmt::fullHex | 10 ) , " vs. Found " , pantheios::integer( mem.value()  ,  pantheios::fmt::fullHex | 10 ) );
	pantheios::log_LOCATION;
	throw 0;
  }  
  
  int32_t expected2( expected + 0x0BADBABE );

  uhal::ValWord< int32_t > mem2 = hw.getClient()->rmw_sum( 0xBA5EADD4 , 0x0BADBABE );
  hw.dispatch();
  
  if( mem2 == expected2 ){
	pantheios::log_INFORMATIONAL(  "RMW-SUM : ALL GOOD" ) ;
  }else{
	pantheios::log_ERROR( "MISMATCH : Source " , pantheios::integer( expected2 , pantheios::fmt::fullHex | 10 ) , " vs. Found " , pantheios::integer( mem2.value()  ,  pantheios::fmt::fullHex | 10 ) );
	pantheios::log_LOCATION;
	throw 0;
  }  
  
}


void navigation_and_traversal_test() {
  // uhal::ConnectionManager manager("file://tests/addr/connections.xml");
  // uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  // std::vector<std::string> lNodes = hw.getNodes();
  // for( std::vector<std::string>::iterator lIt = lNodes.begin() ; lIt != lNodes.end() ; ++lIt ) std::cout << "Get nodes: " << *lIt << std::endl;
 
  // lNodes = hw.getNodes( ".*ENABLE.*" );
  // for( std::vector<std::string>::iterator lIt = lNodes.begin() ; lIt != lNodes.end() ; ++lIt ) std::cout << "Get nodes Regex: " << *lIt << std::endl;
  
  
  // uhal::Node node1(hw.getNode("RECEIVER.CONFIG"));
  // uhal::Node node2(hw.getNode("RECEIVER").getNode("CONFIG"));

  
  // uhal::defs::NodePermission a = node1.getPermission();
  // uint32_t mask = node1.getMask();
  // std::string id = node1.getId();
  // //BOOST_CHECK(id=="RECEIVER.CONFIG");

  // uhal::Node branch(hw.getNode("RECEIVER"));
  // //BOOST_CHECK_THROW(branch.getPermission(),std::exception);
  // //BOOST_CHECK_THROW(branch.getMask(),std::exception);
  // //BOOST_CHECK_THROW(branch.read(),std::exception);
  // //BOOST_CHECK_THROW(branch.write(rand()),std::exception);

  // std::vector<std::string> children = branch.getNodes();
}

void read_test() {
  // uhal::ConnectionManager manager( "file://tests/addr/connections.xml" );
  // uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");

  // //read register
  // uhal::ValWord< uint32_t > mem1 = hw.getNode("RECEIVER").getNode("TTC").getNode("ADDRESS").read();
  // uhal::ValWord< uint32_t > mem2 = hw.getNode("RECEIVER.TTC.ADDRESS").read();
  // hw.dispatch();
  
  // //BOOST_CHECK(mem1.valid() && mem2.valid());
  // //BOOST_CHECK(mem1.value() == mem2.value());

  // //read memory
  // uint32_t SIZE=1024;
  // uhal::ValVector< uint32_t > block1 = hw.getNode("RECEIVER").getNode("MEMORY").readBlock(SIZE);
  // uhal::ValVector< uint32_t > block2 = hw.getNode("RECEIVER.MEMORY").readBlock(SIZE);
  // hw.dispatch();
  
  // //BOOST_CHECK(block1.size() == SIZE);
  // //BOOST_CHECK(block2.size() == SIZE);
  // //BOOST_CHECK(block1.begin()->valid() && block2.begin()->valid());
  // //BOOST_CHECK(*block1.begin() == *block2.begin());
  // //BOOST_CHECK(block1.rbegin()->valid() && block2.rbegin()->valid());
  // //BOOST_CHECK(*block1.rbegin() == *block2.rbegin());

  // //read FIFO
  // block1 = hw.getNode("RECEIVER").getNode("MEMORY").readBlock(SIZE,uhal::defs::NON_INCREMENTAL);
  // block2 = hw.getNode("RECEIVER.MEMORY").readBlock(SIZE,uhal::defs::NON_INCREMENTAL);
  // hw.dispatch();
  
  // //BOOST_CHECK(block1.size() == SIZE);
  // //BOOST_CHECK(block2.size() == SIZE);
  // //BOOST_CHECK(block1.begin()->valid() && block2.begin()->valid());
  // //BOOST_CHECK(*block1.begin() == *block2.begin());
  // //BOOST_CHECK(block1.rbegin()->valid() && block2.rbegin()->valid());
  // //BOOST_CHECK(*block1.rbegin() == *block2.rbegin());
  
}

void write_test() {
  // uhal::ConnectionManager manager("file://tests/addr/connections.xml");
  // uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  
  // //write register
  // uint32_t val = static_cast<uint32_t>(rand());
  
  // hw.getNode("RECEIVER").getNode("TTC").getNode("ADDRESS").write(val);
  // uhal::ValWord< uint32_t > mem = hw.getNode("RECEIVER").getNode("TTC").getNode("ADDRESS").read();
    
  // hw.dispatch();
  
  // if(mem==val){
	// pantheios::log_INFORMATIONAL(  "ALL GOOD" ) ;
  // }else{
	// pantheios::log_ERROR( "MISMATCH : Source " , pantheios::integer( val , pantheios::fmt::fullHex | 10 ) , " vs. Found " , pantheios::integer( mem.value()  ,  pantheios::fmt::fullHex | 10 ) );
	// throw 0;
  // }
  
  // // //BOOST_CHECK(mem.valid());
  // // //BOOST_CHECK(mem == val);

  // // //write memory
  // uint32_t SIZE=1024;
  // std::vector<uint32_t> vals;
  // for(uint32_t i=0;i!=SIZE;i++)
    // vals.push_back(static_cast<uint32_t>(rand()));
    // // vals.push_back(static_cast<uint32_t>(i));

  // hw.getNode("RECEIVER.MEMORY").writeBlock(vals);
  // uhal::ValVector< uint32_t > block = hw.getNode("RECEIVER").getNode("MEMORY").readBlock(SIZE);
   
  // hw.dispatch();
  
  
  // uhal::ValVector< uint32_t >::const_iterator lReadIt = block.begin();
  // std::vector< uint32_t >::const_iterator lSourceIt = vals.begin();
  // int count = 0;
  
  // for( ; lReadIt != block.end() && lSourceIt != vals.end() ; ++lReadIt , ++lSourceIt , ++count ){
	// if( *lReadIt != *lSourceIt ){
		// pantheios::log_ERROR( "MISMATCH AT " , pantheios::integer(count) , " : Source " , pantheios::integer( *lSourceIt , pantheios::fmt::fullHex | 10 ) , " vs. Found " , pantheios::integer( *lReadIt  ,  pantheios::fmt::fullHex | 10 ) );
		// throw 0;
	// }
  // }
  
  // pantheios::log_INFORMATIONAL(  "ALL GOOD" ) ;
  
  // // //BOOST_CHECK(block.size() == SIZE);
  // // //BOOST_CHECK(block.begin()->valid() && block.rbegin()->valid());
  // // //BOOST_CHECK(*block.begin() == *vals.begin());
  // // //BOOST_CHECK*(block.rbegin() == *vals.rbegin());

  // // //write FIFO
  // vals.clear();
  // for(uint32_t i=0;i!=SIZE;i++)
    // vals.push_back(static_cast<uint32_t>(rand()));
    // // vals.push_back(static_cast<uint32_t>(i));
	
  // hw.getNode("RECEIVER").getNode("MEMORY").writeBlock(vals,uhal::defs::NON_INCREMENTAL);
  // block = hw.getNode("RECEIVER").getNode("MEMORY").readBlock(SIZE,uhal::defs::NON_INCREMENTAL);
  
 
  // hw.dispatch();
  
  // /*uhal::ValVector< uint32_t >::const_iterator*/ lReadIt = block.begin();
  // /*std::vector< uint32_t >::const_iterator*/ lSourceIt = vals.end();
  // lSourceIt--;
  
  // /*int*/ count = 0;
  
  // for( ; lReadIt != block.end() ; ++lReadIt , ++count ){
	// if( *lReadIt != *lSourceIt ){
		// pantheios::log_ERROR( "MISMATCH AT " , pantheios::integer(count) , " : Source " , pantheios::integer( *lSourceIt , pantheios::fmt::fullHex | 10 ) , " vs. Found " , pantheios::integer( *lReadIt  ,  pantheios::fmt::fullHex | 10 ) );
		// throw 0;
	// }
  // }
  
  // pantheios::log_INFORMATIONAL(  "ALL GOOD" ) ;
  
}

// void read_write_mask() {
  // uhal::ConnectionManager manager("addr/connections.xml");
  // uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  
  // uhal::ValWord< uint32_t > mem = hw.getNode("REGISTER_MASK_0xF0").read();
  // //BOOST_CHECK(mem >=0 && mem <=0xF);

  // uint32_t val = 0x3;
  // hw.getNode("REGISTER_MASK_0xF0").write(val);
  // mem = hw.getNode("REGISTER_MASK_0xF0").read();
  // hw.dispatch();
  // //BOOST_CHECK(mem == val);
// }

// void read_write_permissions() {
  // uhal::ConnectionManager manager("addr/connections.xml");
  // uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");

  // //read write register 
  // uint32_t val = static_cast<uint32_t>(rand());
  // hw.getNode("READ_WRITE_REGISTER").write(val);
  // uhal::ValWord< uint32_t > mem = hw.getNode("READ_WRITE_REGISTER").read();

  // hw.dispatch();
  // //BOOST_CHECK(mem == val);

  // //read only register
  // mem = hw.getNode("READ_ONLY_REGISTER").read();
  // hw.dispatch();

  // //BOOST_CHECK_THROW(hw.getNode("READ_ONLY_REGISTER").write(rand()),std::exception);

  // //write only register
  // val = static_cast<uint32_t>(rand());
  // hw.getNode("WRITE_ONLY_REGISTER").write(val);
  // hw.dispatch();
  
  // //BOOST_CHECK_THROW(hw.getNode("WRITE_ONLY_REGISTER").read(),std::exception);
  
  // //read write memory
  // uint32_t SIZE = 1024;
  // std::vector<uint32_t> vals;
  // for(uint32_t i=0;i!=SIZE;i++)
    // vals.push_back(static_cast<uint32_t>(rand()));
  
  // hw.getNode("READ_WRITE_MEMORY").writeBlock(vals);
  // uhal::ValVector< uint32_t > block = hw.getNode("READ_WRITE_MEMORY").readBlock(SIZE);
  // hw.dispatch();
  
  // //BOOST_CHECK(block.size() == vals.size());
  // //BOOST_CHECK(*block.begin() == *vals.begin());
  // //BOOST_CHECK*(block.rbegin() == *vals.rbegin());
  
  // //read only memory
  // //BOOST_CHECK_THROW(hw.getNode("READ_ONLY_MEMORY").writeBlock(vals),std::exception);

  // block = hw.getNode("READ_ONLY_MEMORY").readBlock(SIZE);
  // hw.dispatch();
  // //BOOST_CHECK(block.size() == vals.size());
  // //BOOST_CHECK(*block.begin() == *vals.begin());
  // //BOOST_CHECK(*block.rbegin() == *vals.rbegin());
  
  // //write only memory
  // //BOOST_CHECK_THROW(hw.getNode("WRITE_ONLY_MEMORY").readBlock(SIZE),std::exception);
  // hw.getNode("WRITE_ONLY_MEMORY").writeBlock(vals);
  // hw.dispatch();
  

 // }

// void synchronization_primitive() {
  // uhal::ConnectionManager manager("addr/connections.xml");
  // uhal::HwInterface hw = manager.getDevice("hcal.crate1.slot1");
  
  // uint32_t SIZE = 10;
  // for(uint32_t i=0;i!=SIZE;i++)
    // uhal::ValWord< uint32_t > tmp = hw.getNode("REG1").read();

  // hw.dispatch(uhal::defs::ATOMIC);

  // SIZE = 1024*1024;
  // for(uint32_t i=0;i!=SIZE;i++)
    // uhal::ValWord< uint32_t > tmp = hw.getNode("REG1").read();
  
  // //BOOST_CHECK_THROW(hw.dispatch(uhal::defs::ATOMIC),std::exception);
  // try {
    // hw.dispatch(uhal::defs::ATOMIC);
  // } catch(std::exception&) {}
      
// }

int main(int argc,char* argv[]) {
  hwInterface_creation();
  rawClientAccess();
  // navigation_and_traversal_test();
  // read_test();
  // write_test();
  // read_write_mask();
  // read_write_permissions();
  // synchronization_primitive();
}
