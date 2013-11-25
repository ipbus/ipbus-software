#include "uhal/tests/DummyDerivedNode.hpp"
#include "uhal/uhal.hpp"
#include "uhal/log/log.hpp"
#include <typeinfo>

using namespace uhal;

int main ( int argc,char* argv[] )
{
   HwInterface hw=ConnectionManager::getDevice ( "test_device_id","ipbusudp-2.0://192.168.0.128:50001","file://tests/etc/uhal/tests/dummy_derived.xml" );

   log( Notice(), "Derived 1");
   hw.getNode<DummyDerivedNode>("derived").print();
   
   log( Notice(), "Derived 2");
   hw.getNode<DummyDerivedNode>("derived2").print();
   
   log( Notice(), "Derived 2-b");
   const Node& lNode = hw.getNode("derived2");
   DummyDerivedNode lDerived( lNode );
   lDerived.print();
   
   log( Notice(), "DoubleDerived 3");
   hw.getNode<DoubleDerivedNode>("derived3").print();
      
   log( Notice(), "DoubleDerived 3-b");
   const Node& lDNode = hw.getNode("derived3");
   DoubleDerivedNode lDoubleDerived( lDNode );
   lDoubleDerived.print();
   
   boost::unordered_map<std::string,std::string> lPars;
   lPars = lDoubleDerived.getParameters();
   
   std::cout << typeid(lPars).name() << std::endl;
   
   
 
}
