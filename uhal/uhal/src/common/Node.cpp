#include "uhal/Node.hpp"

#include "uhal/HwInterface.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/AddressTableBuilder.hpp"
#include "uhal/Utilities.hpp"

#include "uhal/log.hpp"

#include <iomanip>


std::ostream& operator<< ( std::ostream& aStream , const uhal::Node& aNode )
{
	try
	{
		aNode.stream ( aStream );
		return aStream;
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}
}


namespace uhal
{

	Node::permissions_lut::permissions_lut()
	{
		add
		( "r"			, defs::READ )
		( "w"			, defs::WRITE )
		( "read"			, defs::READ )
		( "write"		, defs::WRITE )
		( "rw"			, defs::READWRITE )
		( "wr"			, defs::READWRITE )
		( "readwrite"	, defs::READWRITE )
		( "writeread"	, defs::READWRITE )
		;
	}

	const Node::permissions_lut Node::mPermissionsLut;


	Node::~Node() {}

	bool Node::operator == ( const Node& aNode )
	{
		try
		{
			return this->getAddress() == aNode.getAddress() &&
				   this->getMask() == aNode.getMask() &&
				   this->getPermission() == aNode.getPermission() &&
				   this->getId() == aNode.getId();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	std::string Node::getId() const
	{
		try
		{
			return mUid;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	uint32_t Node::getAddress() const
	{
		try
		{
			return mAddr;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	uint32_t Node::getMask() const
	{
		try
		{
			return mMask;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	defs::NodePermission Node::getPermission() const
	{
		try
		{
			return mPermission;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	void Node::stream ( std::ostream& aStream , std::size_t aIndent ) const
	{
		try
		{
			aStream << std::hex << std::setfill ( '0' ) << std::uppercase;
			aStream << '\n' << std::string ( aIndent , ' ' ) << "+ ";
			aStream << "Node \"" << mUid << "\", ";
			aStream << "Address 0x" << std::setw ( 8 ) << mAddr << ", ";
			aStream << "Mask 0x" << std::setw ( 8 ) << mMask << ", ";
			aStream << "Permissions " << ( mPermission&defs::READ?'r':'-' ) << ( mPermission&defs::WRITE?'w':'-' );

			if ( mChildren )
			{
				for ( std::vector < Node >::iterator lIt = mChildren->begin(); lIt != mChildren->end(); ++lIt )
				{
					lIt->stream ( aStream , aIndent+2 );
				}
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	Node& Node::getNode ( const std::string& aId )
	{
		try
		{
			std::hash_map< std::string , Node* >::iterator lIterator = mChildrenMap->find ( aId );

			if ( lIterator==mChildrenMap->end() )
			{
				pantheios::log_ERROR ( "No branch found with ID-path \"" , aId , "\". Tree structure is:" , lazy_stream_inserter ( *this ) );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw NoBranchFoundWithGivenUID();
			}

			return * ( lIterator->second );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	std::vector<std::string> Node::getNodes()
	{
		try
		{
			std::vector<std::string> lNodes;
			lNodes.reserve ( mChildren->size() ); //prevent reallocations

			for ( std::vector < Node >::iterator lIt = mChildren->begin(); lIt != mChildren->end(); ++lIt )
			{
				lNodes.push_back ( lIt->mUid );
			}

			return lNodes;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	//Given a regex return the ids that match the
	std::vector<std::string> Node::getNodes ( const boost::regex& aRegex )
	{
		try
		{
			std::vector<std::string> lNodes;
			lNodes.reserve ( mChildrenMap->size() ); //prevent reallocations

			for ( std::hash_map< std::string , Node* >::iterator lIt = mChildrenMap->begin(); lIt != mChildrenMap->end(); ++lIt )
			{
				boost::cmatch lMatch;

				if ( boost::regex_match ( lIt->first.c_str() , lMatch , aRegex ) ) //to allow partial match, add  boost::match_default|boost::match_partial  as fourth argument
				{
					lNodes.push_back ( lIt->first );
				}
			}

			//bit dirty but since the hash map sorts them by the hash, not the value, they are completely scrambled here making it very hard to use.
			std::sort ( lNodes.begin(), lNodes.end() );
			return lNodes;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	std::vector<std::string> Node::getNodes ( const char* aRegex )
	{
		try
		{
			return getNodes ( boost::regex ( aRegex ) );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	std::vector<std::string> Node::getNodes ( const std::string& aRegex )
	{
		try
		{
			return getNodes ( boost::regex ( aRegex ) );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}



Node::Node ( const pugi::xml_node& aXmlNode ) try :
		mHw ( NULL ),
			mAddr ( 0x00000000 ),
			mMask ( 0xFFFFFFFF ),
			mPermission ( defs::READWRITE ),
			mChildren ( new std::vector < Node > ),
			mChildrenMap ( new std::hash_map< std::string , Node* > )
	{
		if ( ! uhal::utilities::GetXMLattribute<true> ( aXmlNode , "id" , mUid ) )
		{
			//error description is given in the function itself so no more elaboration required
			pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
			throw NodeMustHaveUID();
		}

		std::string lModule;

		if ( uhal::utilities::GetXMLattribute<false> ( aXmlNode , "module" , lModule ) )
		{
			try
			{
				Node lNode = AddressTableBuilder::getInstance().getAddressTable ( lModule );
				mChildren->push_back ( lNode );
			}
			catch ( const std::exception& aExc )
			{
				pantheios::log_EXCEPTION ( aExc );
				throw uhal::exception ( aExc );
			}
		}
		else
		{
			uhal::utilities::GetXMLattribute<false> ( aXmlNode , "address" , mAddr );
			uhal::utilities::GetXMLattribute<false> ( aXmlNode , "mask" , mMask );
			std::string lPermission;

			if ( uhal::utilities::GetXMLattribute<false> ( aXmlNode , "permission" , lPermission ) )
			{
				try
				{
					boost::spirit::qi::phrase_parse (
						lPermission.begin(),
						lPermission.end(),
						Node::mPermissionsLut,
						boost::spirit::ascii::space,
						mPermission
					);
				}
				catch ( const std::exception& aExc )
				{
					pantheios::log_EXCEPTION ( aExc );
					throw uhal::exception ( aExc );
				}
			}

			for ( pugi::xml_node lXmlNode = aXmlNode.child ( "node" ); lXmlNode; lXmlNode = lXmlNode.next_sibling ( "node" ) )
			{
				mChildren->push_back ( Node ( lXmlNode ) );
			}
		}

		//iterate over the immediate children
		for ( std::vector < Node >::iterator lIt = mChildren->begin(); lIt != mChildren->end(); ++lIt )
		{
			//add the immediate child to the lookup map
			mChildrenMap->insert ( std::make_pair ( lIt->mUid , & ( *lIt ) ) );

			//add all the entries in the child's look-up map to the paren't look-up map, prepended with the child's name followed by a '.'
			for ( std::hash_map< std::string , Node* >::iterator lSubMapIt = lIt->mChildrenMap->begin() ; lSubMapIt != lIt->mChildrenMap->end() ; ++lSubMapIt )
			{
				mChildrenMap->insert ( std::make_pair ( ( lIt->mUid ) +'.'+ ( lSubMapIt->first ) , lSubMapIt->second ) );
			}
		}
	}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}


	void Node::write ( const uint32_t& aValue )
	{
		try
		{
			if ( mPermission & defs::WRITE )
			{
				if ( mMask == defs::NOMASK )
				{
					mHw->getClient()->write ( mAddr , aValue );
				}
				else
				{
					mHw->getClient()->write ( mAddr , aValue , mMask );
				}
			}
			else
			{
				pantheios::log_ERROR ( "Node permissions denied write access" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw WriteAccessDenied();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	void Node::writeBlock ( const std::vector< uint32_t >& aValues , const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			if ( mPermission & defs::WRITE )
			{
				mHw->getClient()->writeBlock ( mAddr , aValues , aMode );
			}
			else
			{
				pantheios::log_ERROR ( "Node permissions denied write access" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw WriteAccessDenied();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	ValWord< uint32_t > Node::read()
	{
		try
		{
			if ( mPermission & defs::READ )
			{
				if ( mMask == defs::NOMASK )
				{
					return mHw->getClient()->read ( mAddr );
				}
				else
				{
					return mHw->getClient()->read ( mAddr , mMask );
				}
			}
			else
			{
				pantheios::log_ERROR ( "Node permissions denied read access" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw ReadAccessDenied();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	ValVector< uint32_t > Node::readBlock ( const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			if ( mPermission & defs::READ )
			{
				return mHw->getClient()->readBlock ( mAddr , aSize , aMode );
			}
			else
			{
				pantheios::log_ERROR ( "Node permissions denied read access" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw ReadAccessDenied();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	ValWord< int32_t > Node::readSigned()
	{
		try
		{
			if ( mPermission & defs::READ )
			{
				if ( mMask == defs::NOMASK )
				{
					return mHw->getClient()->readSigned ( mAddr );
				}
				else
				{
					return mHw->getClient()->readSigned ( mAddr , mMask );
				}
			}
			else
			{
				pantheios::log_ERROR ( "Node permissions denied read access" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw ReadAccessDenied();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	ValVector< int32_t > Node::readBlockSigned ( const uint32_t& aSize, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			if ( mPermission & defs::READ )
			{
				return mHw->getClient()->readBlockSigned ( mAddr , aSize , aMode );
			}
			else
			{
				pantheios::log_ERROR ( "Node permissions denied read access" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw ReadAccessDenied();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

}
