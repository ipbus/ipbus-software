#include "uhal/Node.hpp"

#include "uhal/HwInterface.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/AddressTable.hpp"
#include "uhal/AddressTableBuilder.hpp"
//#include "uhal/Utilities.hpp"

namespace uhal
{
    /*Node::Node( HwInterface* aHwInterface, const std::string& aFullId )
        :mHw( aHwInterface ),
         mFullId( aFullId ),
         mAddr( aHwInterface->getAddressTable().getAddress( aFullId ) ),
         mMask( aHwInterface->getAddressTable().getMask( aFullId ) ),
         mPermission( aHwInterface->getAddressTable().getPermission( aFullId ) )
    {}*/

    Node Node::getNode( const std::string& aId )
    {
      //  return Node( mHw , mUid + "." + aId );
    }

    std::vector<std::string> Node::getNodes()
    {
        //return mHw->getAddressTable().getChildren( mUid );
    }

	
	
	
	Node::Node( const pugi::xml_node& aXmlNode  ) :
		mHw( NULL ),
        mAddr( 0x00000000 ),
        mMask( 0xFFFFFFFF ),
        mPermission( defs::READWRITE ),
		mChildren( new std::vector< Node > )
	{
/*	
		if ( ! uhal::utilities::GetXMLattribute<true>( aXmlNode , "id" , mUid ) ){
			throw NodeMustHaveUID();
		}
		
		std::string lModule;
		if ( uhal::utilities::GetXMLattribute<false>( aXmlNode , "module" , lModule ) ){
			mChildren->push_back( AddressTableBuilder::getInstance().getAddressTable( lModule ) );
		}else{
			uhal::utilities::GetXMLattribute<false>( aXmlNode , "address" , mAddr );
			uhal::utilities::GetXMLattribute<false>( aXmlNode , "mask" , mMask );
			std::string lPermission;
			if ( uhal::utilities::GetXMLattribute<false>( aXmlNode , "permission" , lPermission ) ){
				boost::spirit::qi::phrase_parse(
					lPermission.begin(),                          
					lPermission.end(),                           
					mPermissionsLut,   
					boost::spirit::ascii::space,
					mPermission
				);

				
				
			}
	
			pugi::xpath_node_set lNodes = aXmlNode.select_nodes("/node" );
			mChildren->reserve( lNodes.size() );
			for (pugi::xpath_node_set::const_iterator lNodeIt = lNodes.begin(); lNodeIt != lNodes.end(); ++lNodeIt )
			{
				mChildren->push_back( lNodeIt->node() );						
			}

		}
*/
	}
	
	
	
	
	
}
