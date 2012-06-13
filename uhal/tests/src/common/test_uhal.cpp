//#define BOOST_TEST_DYN_LINK

//#define BOOST_TEST_MODULE uHAL Test Cases

//#include <boost/test/unit_test.hpp>

#include "uhal/uhal.hpp"

#include <vector>
#include <string>
#include <cstdlib>
#include <boost/math/special_functions/factorials.hpp>

#include "log/log.hpp"

using namespace uhal;


void hwInterface_creation()
{
	try
	{
		ConnectionManager manager ( "     file://tests/addr*/*connections.xml   ; file://~/connection*.xml  ;  ;;; ; ;  " ); // http://svnweb.cern.ch/world/wsvn/cactus/trunk/uhal/tests/addr/connections.xml?op=dl&rev=head      ");
		HwInterface hw=manager.getDevice ( "hcal.crate1.slot1" );
		//BOOST_CHECK(manager.ping());
		// manager.getDevice("hcal.crate1.slot2");
		// hw.dispatch();
		std::vector<std::string> ids = manager.getDevices ( "hcal.crate1.*" );

		for ( std::vector<std::string>::const_iterator i ( ids.begin() ); i != ids.end(); ++i )
			//BOOST_CHECK(manager.getDevice(*i).getClient().ping();
			;
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw exception ( aExc );
	}
}


void rawClientAccess()
{
	try
	{
		ConnectionManager manager ( "file://tests/addr/connections.xml" );
		HwInterface hw = manager.getDevice ( "hcal.crate1.slot1" );
		//write register
		uint32_t val = static_cast<uint32_t> ( rand() );
		hw.getClient()->write ( 0xBA5EADD4 , val );
		ValWord< uint32_t > mem = hw.getClient()->read ( 0xBA5EADD4 );
		hw.dispatch();

		if ( mem==val )
		{
			log ( Info() , "SINGLE WORD WRITE/READ : ALL GOOD" ) ;
		}
		else
		{
			log ( Error() , "MISMATCH : Source " , Integer< hex , fixed> ( val ) , " vs. Found " , Integer< hex , fixed> ( mem.value() ) );
			throw 0;
		}

		// 		// // //write memory
		uint32_t SIZE=129;
		std::vector<uint32_t> vals;

		for ( uint32_t i=0; i!=SIZE; i++ )
		{
			vals.push_back ( static_cast<uint32_t> ( rand() ) );
		}

		// vals.push_back(static_cast<uint32_t>(i));
		hw.getClient()->writeBlock ( 0xBA5EADD4 , vals );
		ValVector< uint32_t > block = hw.getClient()->readBlock ( 0xBA5EADD4 , SIZE );
		hw.dispatch();

		if ( block.size() != SIZE )
		{
			log ( Error() , "SEND AND REPLY SIZES MISMATCH" );
			throw 0;
		}

		ValVector< uint32_t >::const_iterator lReadIt = block.begin();
		std::vector< uint32_t >::const_iterator lSourceIt = vals.begin();
		int count = 0;

		for ( ; lReadIt != block.end() && lSourceIt != vals.end() ; ++lReadIt , ++lSourceIt , ++count )
		{
			if ( *lReadIt != *lSourceIt )
			{
				log ( Error() , "MISMATCH AT " , Integer ( count ) , " : Source " , Integer< hex , fixed> ( *lSourceIt ) , " vs. Found " , Integer< hex , fixed> ( *lReadIt ) );
				throw 0;
			}
		}

		log ( Info() , "INCREMENTING BLOCK WRITE/READ : ALL GOOD" ) ;
		// //BOOST_CHECK(block.size() == SIZE);
		// //BOOST_CHECK(block.begin()->valid() && block.rbegin()->valid());
		// //BOOST_CHECK(*block.begin() == *vals.begin());
		// //BOOST_CHECK*(block.rbegin() == *vals.rbegin());
		// //write FIFO
		vals.clear();

		for ( uint32_t i=0; i!=SIZE; i++ )
		{
			vals.push_back ( static_cast<uint32_t> ( rand() ) );
		}

		// vals.push_back(static_cast<uint32_t>(i));
		hw.getClient()->writeBlock ( 0xBA5EADD4 , vals , defs::NON_INCREMENTAL );
		/*ValVector< uint32_t >*/
		block = hw.getClient()->readBlock ( 0xBA5EADD4 , SIZE , defs::NON_INCREMENTAL );
		hw.dispatch();

		if ( block.size() != SIZE )
		{
			log ( Error() , "SEND AND REPLY SIZES MISMATCH" );
			throw 0;
		}

		/*ValVector< uint32_t >::const_iterator*/ lReadIt = block.begin();
		/*std::vector< uint32_t >::const_iterator*/
		lSourceIt = vals.end();
		lSourceIt--;
		/*int*/
		count = 0;

		for ( ; lReadIt != block.end() ; ++lReadIt , ++count )
		{
			if ( *lReadIt != *lSourceIt )
			{
				log ( Error() , "MISMATCH AT " , Integer ( count ) , " : Source " , Integer< hex , fixed> ( *lSourceIt ) , " vs. Found " , Integer< hex , fixed> ( *lReadIt ) );
				throw 0;
			}
		}

		log ( Info() , "NON-INCREMENTING BLOCK WRITE/READ : ALL GOOD" ) ;
		uint32_t expected ( ( *lSourceIt&0x00C0FFEE ) |0xF00DF00D );
		/*ValWord< uint32_t >*/
		mem = hw.getClient()->rmw_bits ( 0xBA5EADD4 , 0x00C0FFEE , 0xF00DF00D );
		hw.dispatch();

		if ( mem == expected )
		{
			log ( Info() , "RMW-BITS : ALL GOOD" ) ;
		}
		else
		{
			log ( Error() , "MISMATCH : Source " , Integer< hex , fixed> ( expected ) , " vs. Found " , Integer< hex , fixed> ( mem.value() ) );
			throw 0;
		}

		int32_t expected2 ( expected + 0x0BADBABE );
		ValWord< int32_t > mem2 = hw.getClient()->rmw_sum ( 0xBA5EADD4 , 0x0BADBABE );
		hw.dispatch();

		if ( mem2 == expected2 )
		{
			log ( Info() , "RMW-SUM : ALL GOOD" ) ;
		}
		else
		{
			log ( Error() , "MISMATCH : Source " , Integer< hex , fixed> ( expected2 ) , " vs. Found " , Integer< hex , fixed> ( mem2.value() ) );
			throw 0;
		}
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw exception ( aExc );
	}
}


void navigation_and_traversal_test()
{
	try
	{
		ConnectionManager manager ( "file://tests/addr/connections.xml" );
		HwInterface hw = manager.getDevice ( "hcal.crate1.slot1" );
		std::vector<std::string> lNodes = hw.getNodes();

		for ( std::vector<std::string>::iterator lIt = lNodes.begin() ; lIt != lNodes.end() ; ++lIt )
		{
			log ( Info() , "Get nodes: " , *lIt );
		}

		lNodes = hw.getNodes ( ".*ENABLE.*" );

		for ( std::vector<std::string>::iterator lIt = lNodes.begin() ; lIt != lNodes.end() ; ++lIt )
		{
			log ( Info() , "Get nodes Regex: " , *lIt );
		}

		Node node1 ( hw.getNode ( "RECEIVER.CONFIG" ) );
		Node node2 ( hw.getNode ( "RECEIVER" ).getNode ( "CONFIG" ) );
		defs::NodePermission a = node1.getPermission();
		uint32_t mask = node1.getMask();
		std::string id = node1.getId();
		//BOOST_CHECK(id=="RECEIVER.CONFIG");
		Node branch ( hw.getNode ( "RECEIVER" ) );
		//BOOST_CHECK_THROW(branch.getPermission(),std::exception);
		//BOOST_CHECK_THROW(branch.getMask(),std::exception);
		//BOOST_CHECK_THROW(branch.read(),std::exception);
		//BOOST_CHECK_THROW(branch.write(rand()),std::exception);
		std::vector<std::string> children = branch.getNodes();
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw exception ( aExc );
	}
}


void read_test()
{
	try
	{
		ConnectionManager manager ( "file://tests/addr/connections.xml" );
		HwInterface hw = manager.getDevice ( "hcal.crate1.slot1" );
		//read register
		ValWord< uint32_t > mem1 = hw.getNode ( "SYSTEM1" ).getNode ( "SYSTEM" ).getNode ( "TTC" ).getNode ( "ADDRESS" ).read();
		ValWord< uint32_t > mem2 = hw.getNode ( "SYSTEM1.SYSTEM.TTC.ADDRESS" ).read();
		hw.dispatch();
		//BOOST_CHECK(mem1.valid() && mem2.valid());
		//BOOST_CHECK(mem1.value() == mem2.value());
		//read memory
		uint32_t SIZE=1024;
		ValVector< uint32_t > block1 = hw.getNode ( "TRANSMITTER" ).getNode ( "BRAM_DATA" ).readBlock ( SIZE );
		ValVector< uint32_t > block2 = hw.getNode ( "TRANSMITTER.BRAM_DATA" ).readBlock ( SIZE );
		hw.dispatch();
		//BOOST_CHECK(block1.size() == SIZE);
		//BOOST_CHECK(block2.size() == SIZE);
		//BOOST_CHECK(block1.begin()->valid() && block2.begin()->valid());
		//BOOST_CHECK(*block1.begin() == *block2.begin());
		//BOOST_CHECK(block1.rbegin()->valid() && block2.rbegin()->valid());
		//BOOST_CHECK(*block1.rbegin() == *block2.rbegin());
		//read FIFO
		block1 = hw.getNode ( "TRANSMITTER" ).getNode ( "BRAM_DATA" ).readBlock ( SIZE,defs::NON_INCREMENTAL );
		block2 = hw.getNode ( "TRANSMITTER.BRAM_DATA" ).readBlock ( SIZE,defs::NON_INCREMENTAL );
		hw.dispatch();
		//BOOST_CHECK(block1.size() == SIZE);
		//BOOST_CHECK(block2.size() == SIZE);
		//BOOST_CHECK(block1.begin()->valid() && block2.begin()->valid());
		//BOOST_CHECK(*block1.begin() == *block2.begin());
		//BOOST_CHECK(block1.rbegin()->valid() && block2.rbegin()->valid());
		//BOOST_CHECK(*block1.rbegin() == *block2.rbegin());
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw exception ( aExc );
	}
}

void write_test()
{
	try
	{
		ConnectionManager manager ( "file://tests/addr/connections.xml" );
		HwInterface hw = manager.getDevice ( "hcal.crate1.slot1" );
		//write register
		uint32_t val = static_cast<uint32_t> ( rand() );
		hw.getNode ( "SYSTEM1" ).getNode ( "SYSTEM" ).getNode ( "TTC" ).getNode ( "ADDRESS" ).write ( val );
		ValWord< uint32_t > mem = hw.getNode ( "SYSTEM1" ).getNode ( "SYSTEM" ).getNode ( "TTC" ).getNode ( "ADDRESS" ).read();
		hw.dispatch();

		if ( mem==val )
		{
			log ( Info() , "ALL GOOD" ) ;
		}
		else
		{
			log ( Error() , "MISMATCH : Source " , Integer< hex , fixed> ( val ) , " vs. Found " , Integer< hex , fixed> ( mem.value() ) );
			throw 0;
		}

		// //BOOST_CHECK(mem.valid());
		// //BOOST_CHECK(mem == val);
		// //write memory
		uint32_t SIZE=1024;
		std::vector<uint32_t> vals;

		for ( uint32_t i=0; i!=SIZE; i++ )
		{
			vals.push_back ( static_cast<uint32_t> ( rand() ) );
		}

		// vals.push_back(static_cast<uint32_t>(i));
		hw.getNode ( "TRANSMITTER.BRAM_DATA" ).writeBlock ( vals );
		ValVector< uint32_t > block = hw.getNode ( "TRANSMITTER" ).getNode ( "BRAM_DATA" ).readBlock ( SIZE );
		hw.dispatch();

		if ( block.size() != SIZE )
		{
			log ( Error() , "SEND AND REPLY SIZES MISMATCH" );
			throw 0;
		}

		ValVector< uint32_t >::const_iterator lReadIt = block.begin();
		std::vector< uint32_t >::const_iterator lSourceIt = vals.begin();
		int count = 0;

		for ( ; lReadIt != block.end() && lSourceIt != vals.end() ; ++lReadIt , ++lSourceIt , ++count )
		{
			if ( *lReadIt != *lSourceIt )
			{
				log ( Error() , "MISMATCH AT " , Integer ( count ) , " : Source " , Integer< hex , fixed> ( *lSourceIt ) , " vs. Found " , Integer< hex , fixed> ( *lReadIt ) );
				throw 0;
			}
		}

		log ( Info() , "ALL GOOD" ) ;
		// //BOOST_CHECK(block.size() == SIZE);
		// //BOOST_CHECK(block.begin()->valid() && block.rbegin()->valid());
		// //BOOST_CHECK(*block.begin() == *vals.begin());
		// //BOOST_CHECK*(block.rbegin() == *vals.rbegin());
		// //write FIFO
		vals.clear();

		for ( uint32_t i=0; i!=SIZE; i++ )
		{
			vals.push_back ( static_cast<uint32_t> ( rand() ) );
		}

		// vals.push_back(static_cast<uint32_t>(i));
		hw.getNode ( "TRANSMITTER" ).getNode ( "BRAM_DATA" ).writeBlock ( vals,defs::NON_INCREMENTAL );
		block = hw.getNode ( "TRANSMITTER" ).getNode ( "BRAM_DATA" ).readBlock ( SIZE,defs::NON_INCREMENTAL );
		hw.dispatch();
		/*ValVector< uint32_t >::const_iterator*/
		lReadIt = block.begin();
		/*std::vector< uint32_t >::const_iterator*/
		lSourceIt = vals.end();
		lSourceIt--;
		/*int*/
		count = 0;

		for ( ; lReadIt != block.end() ; ++lReadIt , ++count )
		{
			if ( *lReadIt != *lSourceIt )
			{
				log ( Error() , "MISMATCH AT " , Integer ( count ) , " : Source " , Integer< hex , fixed> ( *lSourceIt ) , " vs. Found " , Integer< hex , fixed> ( *lReadIt ) );
				throw 0;
			}
		}

		log ( Info() , "ALL GOOD" ) ;
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw exception ( aExc );
	}
}

void read_write_mask()
{
	ConnectionManager manager ( "file://tests/addr/connections.xml" );
	HwInterface hw = manager.getDevice ( "hcal.crate1.slot1" );
	hw.getNode ( "JTAG_BASE_ADDR.a" ).write ( 0x1 );
	hw.getNode ( "JTAG_BASE_ADDR.b" ).write ( 0x1 );
	hw.getNode ( "JTAG_BASE_ADDR.c" ).write ( 0x1 );
	hw.getNode ( "JTAG_BASE_ADDR.d" ).write ( 0x1 );
	hw.dispatch();
	HwInterface hw2 = manager.getDevice ( "hcal.crate1.slot2" );
	hw2.getNode ( "JTAG_BASE_ADDR.a" ).write ( 0x1 );
	hw2.getNode ( "JTAG_BASE_ADDR.b" ).write ( 0x1 );
	hw2.getNode ( "JTAG_BASE_ADDR.c" ).write ( 0x1 );
	hw2.getNode ( "JTAG_BASE_ADDR.d" ).write ( 0x1 );
	hw2.dispatch();
	// ValWord< uint32_t > mem = hw.getNode("REGISTER_MASK_0xF0").read();
	// //BOOST_CHECK(mem >=0 && mem <=0xF);
	// uint32_t val = 0x3;
	// hw.getNode("REGISTER_MASK_0xF0").write(val);
	// mem = hw.getNode("REGISTER_MASK_0xF0").read();
	// hw.dispatch();
	// //BOOST_CHECK(mem == val);
}

// void read_write_permissions() {
// ConnectionManager manager("addr/connections.xml");
// HwInterface hw = manager.getDevice("hcal.crate1.slot1");

// //read write register
// uint32_t val = static_cast<uint32_t>(rand());
// hw.getNode("READ_WRITE_REGISTER").write(val);
// ValWord< uint32_t > mem = hw.getNode("READ_WRITE_REGISTER").read();

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
// ValVector< uint32_t > block = hw.getNode("READ_WRITE_MEMORY").readBlock(SIZE);
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
// ConnectionManager manager("addr/connections.xml");
// HwInterface hw = manager.getDevice("hcal.crate1.slot1");

// uint32_t SIZE = 10;
// for(uint32_t i=0;i!=SIZE;i++)
// ValWord< uint32_t > tmp = hw.getNode("REG1").read();

// hw.dispatch(defs::ATOMIC);

// SIZE = 1024*1024;
// for(uint32_t i=0;i!=SIZE;i++)
// ValWord< uint32_t > tmp = hw.getNode("REG1").read();

// //BOOST_CHECK_THROW(hw.dispatch(defs::ATOMIC),std::exception);
// try {
// hw.dispatch(defs::ATOMIC);
// } catch(std::exception&) {}

// }


void addOperationToQueue ( HwInterface& hw , const std::vector<int>& aOperationList , const uint32_t& val, const std::vector<uint32_t>& vals )
{
	try
	{
		for ( std::vector<int>::const_iterator lIt = aOperationList.begin() ; lIt != aOperationList.end() ; ++lIt )
		{
			switch ( *lIt )
			{
				case 0:
					hw.getClient()->write ( 0xBA5EADD4 , val );
					break;
				case 1:
					hw.getClient()->read ( 0xBA5EADD4 );
					break;
				case 2:
					hw.getClient()->writeBlock ( 0xBA5EADD4 , vals );
					break;
				case 3:
					hw.getClient()->readBlock ( 0xBA5EADD4 , vals.size() );
					break;
				case 4:
					hw.getClient()->writeBlock ( 0xBA5EADD4 , vals , defs::NON_INCREMENTAL );
					break;
				case 5:
					hw.getClient()->readBlock ( 0xBA5EADD4 , vals.size() , defs::NON_INCREMENTAL );
					break;
				case 6:
					hw.getClient()->rmw_bits ( 0xBA5EADD4 , 0x00C0FFEE , 0xF00DF00D );
					break;
				case 7:
					hw.getClient()->rmw_sum ( 0xBA5EADD4 , 0x0BADBABE );
					break;
				case 8:
					hw.getClient()->write ( 0xADD4BA5E , val );
					break;
				case 9:
					hw.getClient()->read ( 0xADD4BA5E );
					break;
				case 10:
					hw.getClient()->writeBlock ( 0xADD4BA5E , vals );
					break;
				case 11:
					hw.getClient()->readBlock ( 0xADD4BA5E , vals.size() );
					break;
				case 12:
					hw.getClient()->writeBlock ( 0xADD4BA5E , vals , defs::NON_INCREMENTAL );
					break;
				case 13:
					hw.getClient()->readBlock ( 0xADD4BA5E , vals.size() , defs::NON_INCREMENTAL );
					break;
				case 14:
					hw.getClient()->rmw_bits ( 0xADD4BA5E , 0xDEADFACE , 0xFEEDFACE );
					break;
				case 15:
					hw.getClient()->rmw_sum ( 0xADD4BA5E , 0xDEADBEEF );
					break;
				default:
					throw 0;
			}
		}
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw exception ( aExc );
	}
}




void allInstructionPermutations()
{
	log_configuration::setLogLevelTo ( Info() );

	try
	{
		ConnectionManager manager ( "file://tests/addr/connections.xml" );
		std::vector< HwInterface > hw;
		hw.push_back ( manager.getDevice ( "hcal.crate1.slot1" ) );
		hw.push_back ( manager.getDevice ( "hcal.crate1.slot2" ) );
		uint32_t BlockSize ( 2 );
		uint32_t val = static_cast<uint32_t> ( rand() );
		std::vector<uint32_t> vals;

		for ( uint32_t i=0; i!=BlockSize; i++ )
		{
			vals.push_back ( static_cast<uint32_t> ( rand() ) );
		}

		std::vector<int> lOperationSequence;
		int count;
		double total;

		// ----------------------------------------------------------------------------------------------------------------------------------------
		try
		{
			// ----------------------------------------------------------------------------------------------------------------------------------------
			if ( true )
			{
				int lTemp[] = { 0,2,4,8,10,12 };
				lOperationSequence = std::vector<int> ( lTemp , lTemp+6 );
				count = 0;
				total = boost::math::factorial<double> ( lOperationSequence.size() );

				do
				{
					log ( Info() , "all writes, permutation: " , Integer ( ++count ) , "/" , Real ( total ) );
					addOperationToQueue ( hw.at ( rand() %hw.size() ) , lOperationSequence , val , vals );
					hw.at ( 0 ).dispatch();
				}
				while ( std::next_permutation ( lOperationSequence.begin() , lOperationSequence.end() ) );
			}

			// ----------------------------------------------------------------------------------------------------------------------------------------
			// ----------------------------------------------------------------------------------------------------------------------------------------
			if ( true )
			{
				int lTemp[] = { 1,3,5,9,11,13 };
				lOperationSequence = std::vector<int> ( lTemp , lTemp+6 );
				count = 0;
				total = boost::math::factorial<double> ( lOperationSequence.size() );

				do
				{
					log ( Info() , "all reads, permutation: " , Integer ( ++count ) , "/" , Real ( total ) );
					addOperationToQueue ( hw.at ( rand() %hw.size() ) , lOperationSequence , val , vals );
					hw.at ( 0 ).dispatch();
				}
				while ( std::next_permutation ( lOperationSequence.begin() , lOperationSequence.end() ) );
			}

			// ----------------------------------------------------------------------------------------------------------------------------------------
			// ----------------------------------------------------------------------------------------------------------------------------------------
			if ( true )
			{
				int lTemp[] = { 6,7,14,15 };
				lOperationSequence = std::vector<int> ( lTemp , lTemp+4 );
				count = 0;
				total = boost::math::factorial<double> ( lOperationSequence.size() );

				do
				{
					log ( Info() , "read-modify-write, permutation: " , Integer ( ++count ) , "/" , Real ( total ) );
					addOperationToQueue ( hw.at ( rand() %hw.size() ) , lOperationSequence , val , vals );
					hw.at ( 0 ).dispatch();
				}
				while ( std::next_permutation ( lOperationSequence.begin() , lOperationSequence.end() ) );
			}

			// ----------------------------------------------------------------------------------------------------------------------------------------
			// ----------------------------------------------------------------------------------------------------------------------------------------
			if ( true )
			{
				int lTemp[] = { 0,1,2,3,4,5,6,7 };
				lOperationSequence = std::vector<int> ( lTemp , lTemp+8 );
				count = 0;
				total = boost::math::factorial<double> ( lOperationSequence.size() );

				do
				{
					log ( Info() , "all types, same addr, permutation: " , Integer ( ++count ) , "/" , Real ( total ) );
					addOperationToQueue ( hw.at ( rand() %hw.size() ) , lOperationSequence , val , vals );
					hw.at ( 0 ).dispatch();
				}
				while ( std::next_permutation ( lOperationSequence.begin() , lOperationSequence.end() ) );
			}

			// ----------------------------------------------------------------------------------------------------------------------------------------
		}
		catch ( const std::exception& aExc )
		{
			std::stringstream lStr;
			std::string messages[] = { "write addr1" , "read addr1" , "writeBlock addr1" , "readBlock addr1" , "writeBlock non-incrementing addr1" , "readBlock non-incrementing addr1" , "rmw_bits addr1" , "rmw_sum addr1" ,
									   "write addr2" , "read addr2" , "writeBlock addr2" , "readBlock addr2" , "writeBlock non-incrementing addr2" , "readBlock non-incrementing addr2" , "rmw_bits addr2" , "rmw_sum addr2"
									 };

			for ( std::vector<int>::iterator lIt = lOperationSequence.begin() ; lIt != lOperationSequence.end() ; ++lIt )
			{
				lStr << messages[ *lIt ] << ", ";
			}

			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			log ( Error() , "TEST SEQUENCE WAS : " , lStr.str() );
			throw exception ( aExc );
		}
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw exception ( aExc );
	}
}


int main ( int argc,char* argv[] )
{
	try
	{
		log_configuration::setLogLevelTo ( Debug() );
		hwInterface_creation();
		rawClientAccess();
		navigation_and_traversal_test();
		read_test();
		write_test();
		read_write_mask();
		// read_write_permissions();
		// synchronization_primitive();
		allInstructionPermutations();
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw exception ( aExc );
	}
}
