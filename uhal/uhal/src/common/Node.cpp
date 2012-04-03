#include "uhal/Node.hpp"

#include "uhal/HwInterface.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/AddressTable.hpp"
#include "uhal/AddressTableBuilder.hpp"
#include "uhal/Utilities.hpp"

#include "uhal/log.hpp"

#include <iomanip>

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


	void Node::stream( std::ostream& aStream , std::size_t indent ) const{
		aStream << std::hex << std::setfill('0') << std::uppercase;
		aStream << '\n' << std::string( indent , ' ') << "+ ";
		aStream << "Node \"" << mUid << "\", ";
		aStream << "Address 0x" << std::setw(8) << mAddr << ", ";
		aStream << "Mask 0x" << std::setw(8) << mMask << ", ";
		aStream << "Permissions " << (mPermission&defs::READ?'r':'-') << (mPermission&defs::WRITE?'w':'-');
			
		if( mChildren ){
			for ( std::vector < Node >::iterator lIt = mChildren->begin(); lIt != mChildren->end(); ++lIt ){
				lIt->stream( aStream , indent+2 );
			}
		}		
	}


    Node& Node::getNode( const std::string& aId )
    {
		std::hash_map< std::string , Node* >::iterator lIterator = mChildrenMap->find( aId );	
				
		if ( lIterator==mChildrenMap->end() ){
			pantheios::log_ALERT( "No branch found with ID-path \"" , aId , "\". Tree structure is:" , lazy_inserter(*this) );	
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw NoBranchFoundWithGivenUID();
		}
		
		return *(lIterator->second);
    }

	
    std::vector<std::string> Node::getNodes()
    {
		std::vector<std::string> lNodes;
		lNodes.reserve( mChildren->size() ); //prevent reallocations
		
		for ( std::vector < Node >::iterator lIt = mChildren->begin(); lIt != mChildren->end(); ++lIt ){
			lNodes.push_back( lIt->mUid );
		}
		
        return lNodes;
    }

	//Given a regex return the ids that match the
	std::vector<std::string> Node::getNodes ( const boost::regex& aRegex )
	{
		std::vector<std::string> lNodes;
		lNodes.reserve( mChildrenMap->size() ); //prevent reallocations

		for ( std::hash_map< std::string , Node* >::iterator lIt = mChildrenMap->begin(); lIt != mChildrenMap->end(); ++lIt ){
			boost::cmatch lMatch;
			if( boost::regex_match( lIt->first.c_str() , lMatch , aRegex ) ) //to allow partial match, add  boost::match_default|boost::match_partial  as fourth argument
			{
				lNodes.push_back( lIt->first );
			}
		}

		//bit dirty but since the hash map sorts them by the hash, not the value, they are completely scrambled here making it very hard to use.
		std::sort(lNodes.begin(), lNodes.end()); 

       return lNodes;
	}

	std::vector<std::string> Node::getNodes ( const char* aRegex )
	{
		return getNodes ( boost::regex( aRegex ) );
	}

	std::vector<std::string> Node::getNodes ( const std::string& aRegex )
	{
		return getNodes ( boost::regex( aRegex ) );
	}	
	
	
	
	Node::Node( const pugi::xml_node& aXmlNode  ) :
		mHw( NULL ),
        mAddr( 0x00000000 ),
        mMask( 0xFFFFFFFF ),
        mPermission( defs::READWRITE ),
		mChildren( new std::vector < Node > ),
		mChildrenMap( new std::hash_map< std::string , Node* > )
	{

		if ( ! uhal::utilities::GetXMLattribute<true>( aXmlNode , "id" , mUid ) ){
			//error description is given in the function
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw NodeMustHaveUID();
		}
				
		std::string lModule;
		if ( uhal::utilities::GetXMLattribute<false>( aXmlNode , "module" , lModule ) ){
			try{
				Node lNode = AddressTableBuilder::getInstance().getAddressTable( lModule );
				mChildren->push_back( lNode );	
			}catch( std::exception& aExc ){
				pantheios::log_ALERT( "Caught EXCEPTION \"" , aExc.what() , "\" in " , ThisLocation() );	
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
					pantheios::log_ALERT( "Caught EXCEPTION \"" , aExc.what() , "\" in " , ThisLocation() );
				}
			}
 
			for ( pugi::xml_node lXmlNode = aXmlNode.child("node"); lXmlNode; lXmlNode = lXmlNode.next_sibling("node") )
			{
				mChildren->push_back( Node(lXmlNode) );	
			}

		}
		

		//iterate over the immediate children
		for ( std::vector < Node >::iterator lIt = mChildren->begin(); lIt != mChildren->end(); ++lIt ){
			//add the immediate child to the lookup map
			mChildrenMap->insert( std::make_pair( lIt->mUid , &(*lIt) ) );
			
			//add all the entries in the child's look-up map to the paren't look-up map, prepended with the child's name followed by a '.'
			for( std::hash_map< std::string , Node* >::iterator lSubMapIt = lIt->mChildrenMap->begin() ; lSubMapIt != lIt->mChildrenMap->end() ; ++lSubMapIt ){
				mChildrenMap->insert( std::make_pair( (lIt->mUid)+'.'+(lSubMapIt->first) , lSubMapIt->second ) );
			}
		
		}

	}
	
	
	
	
	
}
