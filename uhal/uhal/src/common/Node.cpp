#include "uhal/Node.hpp"

#include "uhal/HwInterface.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/AddressTable.hpp"
#include "uhal/AddressTableBuilder.hpp"
#include "uhal/Utilities.hpp"

#include "uhal/log.hpp"

namespace uhal
{

	Node::permissions_lut::permissions_lut()
	{
		add
			("r"			, defs::READ)
			("w"			, defs::WRITE)
			("read"			, defs::READ)
			("write"		, defs::WRITE)
			("rw"			, defs::READWRITE)
			("wr"			, defs::READWRITE)
			("readwrite"	, defs::READWRITE)
			("writeread"	, defs::READWRITE)
		;
	}

	const Node::permissions_lut Node::mPermissionsLut;
	

	Node::~Node(){}

	bool Node::operator == ( const Node& aNode )
	{
		return this->getAddress() == aNode.getAddress() &&
			   this->getMask() == aNode.getMask() &&
			   this->getPermission() == aNode.getPermission() &&
			   this->getId() == aNode.getId();
	}


	std::string Node::getId() const
	{
		return mUid;
	}

	uint32_t Node::getAddress() const
	{
		return mAddr;
	}

	uint32_t Node::getMask() const
	{
		return mMask;
	}

	defs::NodePermission Node::getPermission() const
	{
		return mPermission;
	}












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

		if ( ! uhal::utilities::GetXMLattribute<true>( aXmlNode , "id" , mUid ) ){
			throw NodeMustHaveUID();
		}
				
		std::string lModule;
		if ( uhal::utilities::GetXMLattribute<false>( aXmlNode , "module" , lModule ) ){
			try{
				pantheios::log_LOCATION;
				Node lNode = AddressTableBuilder::getInstance().getAddressTable( lModule );
				pantheios::log_LOCATION;
				mChildren->push_back( lNode );
				pantheios::log_LOCATION;
			}catch( std::exception& aExc ){
				pantheios::log_LOCATION;
				pantheios::log_ALERT ( "EXCEPTION: " , aExc.what() );				
			}
 		}else{
			uhal::utilities::GetXMLattribute<false>( aXmlNode , "address" , mAddr );
			uhal::utilities::GetXMLattribute<false>( aXmlNode , "mask" , mMask );
			std::string lPermission;
			if ( uhal::utilities::GetXMLattribute<false>( aXmlNode , "permission" , lPermission ) ){
				try{
					boost::spirit::qi::phrase_parse(
						lPermission.begin(),                          
						lPermission.end(),                           
						Node::mPermissionsLut,   
						boost::spirit::ascii::space,
						mPermission
					);
				}catch( std::exception& aExc ){
					pantheios::log_LOCATION;
					pantheios::log_ALERT ( "EXCEPTION: " , aExc.what() );				
				}
			}
 
			for ( pugi::xml_node lNode = aXmlNode.child("node"); lNode; lNode = lNode.next_sibling("node") )
			{
				mChildren->push_back( Node(lNode) );						
			}

		}

	}
	
	
	
	
	
}
