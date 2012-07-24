#include "uhal/Node.hpp"

#include "uhal/HwInterface.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/NodeTreeBuilder.hpp"
#include "uhal/Utilities.hpp"

#include "uhal/log/log.hpp"

#include <iomanip>

namespace uhal
{
	/**
		The log_inserter function to add the node object to a log entry
		@param aNode a node to format and print to log
	*/
	template < >
	void log_inserter< uhal::Node > ( const uhal::Node& aNode )
	{
		std::stringstream lStream;
		aNode.stream ( lStream );
		std::istreambuf_iterator<char> lEnd;
		std::istreambuf_iterator<char> lIt ( lStream.rdbuf() );

		while ( lIt!=lEnd )
		{
			put ( *lIt++ );
		}
	}
}







namespace uhal
{

	Node::permissions_lut::permissions_lut()
	{
		add
		( "r"			, defs::READ )
		( "w"			, defs::WRITE )
		( "read"		, defs::READ )
		( "write"		, defs::WRITE )
		( "rw"			, defs::READWRITE )
		( "wr"			, defs::READWRITE )
		( "readwrite"	, defs::READWRITE )
		( "writeread"	, defs::READWRITE )
		;
	}

	const Node::permissions_lut Node::mPermissionsLut;


	Node::mode_lut::mode_lut()
	{
		add
		( "single"			, defs::SINGLE )
		( "block"			, defs::INCREMENTAL )
		( "port"			, defs::NON_INCREMENTAL )
		( "incremental"		, defs::INCREMENTAL )
		( "non-incremental"	, defs::NON_INCREMENTAL )
		( "inc"				, defs::INCREMENTAL )
		( "non-inc"			, defs::NON_INCREMENTAL )
		;
	}

	const Node::mode_lut Node::mModeLut;








Node::Node ( const pugi::xml_node& aXmlNode , const boost::filesystem::path& aPath ) try :
		mHw ( NULL ),
			mUid ( "" ),
			mAddr ( 0x00000000 ),
			mAddrValid ( false ),
			mMask ( defs::NOMASK ),
			mPermission ( defs::READWRITE ),
			mMode ( defs::SINGLE ),
			mSize ( 0x00000000 ),
			mTags ( "" ),
			mChildren ( new std::deque< Node >() ),
			mChildrenMap ( new std::hash_map< std::string , Node* >() )
	{
		//ID is a compulsory attribute for identifying a node
		if ( ! uhal::utilities::GetXMLattribute<true> ( aXmlNode , "id" , mUid ) )
		{
			//error description is given in the function itself so no more elaboration required
			NodeMustHaveUID().throwFrom ( ThisLocation() );
		}

		//Address is an optional attribute for hierarchical addressing
		uhal::utilities::GetXMLattribute<false> ( aXmlNode , "address" , mAddr );
		//Module is an optional attribute for pointing to other xml files. When specified, module implies that certain other attributes be ignored
		std::string lModule;

		if ( uhal::utilities::GetXMLattribute<false> ( aXmlNode , "module" , lModule ) )
		{
			try
			{
				log ( Debug() , mUid , " : " , Integer ( mAddr , IntFmt<hex,fixed>() ) );
				//mChildren->push_back ( NodeTreeBuilder::getInstance().getNodeTree ( lModule , aPath , false )->clone() );
				boost::shared_ptr< Node > lNode ( NodeTreeBuilder::getInstance().getNodeTree ( lModule , aPath , false ) );

				for ( std::deque< Node >::iterator lIt = lNode->mChildren->begin(); lIt != lNode->mChildren->end(); ++lIt )
				{
					mChildren->push_back ( lIt->clone() );
				}
			}
			catch ( uhal::exception& aExc )
			{
				aExc.rethrowFrom ( ThisLocation() );
			}
			catch ( const std::exception& aExc )
			{
				StdException ( aExc ).throwFrom ( ThisLocation() );
			}
		}
		else
		{
			//Mask as an optional attribute to identify subfields of a register
			uhal::utilities::GetXMLattribute<false> ( aXmlNode , "mask" , mMask );
			//Permissions is an optional attribute for specifying read/write permissions
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
				catch ( uhal::exception& aExc )
				{
					aExc.rethrowFrom ( ThisLocation() );
				}
				catch ( const std::exception& aExc )
				{
					StdException ( aExc ).throwFrom ( ThisLocation() );
				}
			}

			//Mode is an optional attribute for specifying whether a block is incremental, non-incremental or a single register
			std::string lMode;

			if ( uhal::utilities::GetXMLattribute<false> ( aXmlNode , "mode" , lMode ) )
			{
				try
				{
					boost::spirit::qi::phrase_parse (
						lMode.begin(),
						lMode.end(),
						Node::mModeLut,
						boost::spirit::ascii::space,
						mMode
					);
				}
				catch ( uhal::exception& aExc )
				{
					aExc.rethrowFrom ( ThisLocation() );
				}
				catch ( const std::exception& aExc )
				{
					StdException ( aExc ).throwFrom ( ThisLocation() );
				}

				if ( mMode == defs::INCREMENTAL )
				{
					//If a block is incremental it requires a size attribute
					if ( ! uhal::utilities::GetXMLattribute<false> ( aXmlNode , "size" , mSize ) )
					{
						log ( Error() , "Nodes " , Quote ( mUid ) , " has type " , Quote ( "INCREMENTAL" ) , " require a " , Quote ( "size" ) , " attribute" );
						IncrementalNodeRequiresSizeAttribute().throwFrom ( ThisLocation() );
					}
				}
				else if ( mMode == defs::NON_INCREMENTAL )
				{
					//If a block is non-incremental, then a size attribute is recommended
					if ( ! uhal::utilities::GetXMLattribute<false> ( aXmlNode , "size" , mSize ) )
					{
						log ( Notice() , "Node " , Quote ( mUid ) , " has type " , Quote ( "NON_INCREMENTAL" ) , " does not have a " , Quote ( "size" ) , " attribute. This is not necessarily a problem, but if there is a limit to the size of the read/write operation from this port, then please consider adding this attribute for the sake of safety." );
					}
				}
			}

			for ( pugi::xml_node lXmlNode = aXmlNode.child ( "node" ); lXmlNode; lXmlNode = lXmlNode.next_sibling ( "node" ) )
			{
				mChildren->push_back ( NodeTreeBuilder::getInstance().create ( lXmlNode , aPath )->clone() );
			}
		}

		for ( std::deque< Node >::iterator lIt = mChildren->begin(); lIt != mChildren->end(); ++lIt )
		{
			mChildrenMap->insert ( std::make_pair ( lIt->mUid , & ( *lIt ) ) );

			for ( std::hash_map< std::string , Node* >::iterator lSubMapIt = lIt->mChildrenMap->begin() ; lSubMapIt != lIt->mChildrenMap->end() ; ++lSubMapIt )
			{
				mChildrenMap->insert ( std::make_pair ( ( lIt->mUid ) +'.'+ ( lSubMapIt->first ) , lSubMapIt->second ) );
			}
		}

		//Tags is an optional attribute to allow the user to add a description to a node
		uhal::utilities::GetXMLattribute<false> ( aXmlNode , "tags" , mTags );
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}


Node::Node ( ) try :
		mHw ( NULL ),
			mUid ( "" ),
			mAddr ( 0x00000000 ),
			mAddrValid ( false ),
			mMask ( defs::NOMASK ),
			mPermission ( defs::READWRITE ),
			mMode ( defs::SINGLE ),
			mSize ( 0x00000000 ),
			mTags ( "" ),
			mChildren ( new std::deque< Node >() ),
			mChildrenMap ( new std::hash_map< std::string , Node* >() )
	{
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}


Node::Node ( const Node& aNode ) try :
		mHw ( aNode.mHw ),
			mUid ( aNode.mUid ),
			mAddr ( aNode.mAddr ),
			mAddrValid ( aNode.mAddrValid ),
			mMask ( aNode.mMask ),
			mPermission ( aNode.mPermission ),
			mMode ( aNode.mMode ),
			mSize ( aNode.mSize ),
			mTags ( aNode.mTags ),
			mChildren ( aNode.mChildren ),
			mChildrenMap ( aNode.mChildrenMap )
	{
	}
	catch ( uhal::exception& aExc )
	{
		aExc.rethrowFrom ( ThisLocation() );
	}
	catch ( const std::exception& aExc )
	{
		StdException ( aExc ).throwFrom ( ThisLocation() );
	}



	Node Node::clone() const
	{
		Node lNode;
		lNode.mHw = mHw;
		lNode.mUid = mUid ;
		lNode.mAddr = mAddr;
		lNode.mAddrValid = mAddrValid;
		lNode.mMask = mMask;
		lNode.mPermission = mPermission;
		lNode.mMode = mMode;
		lNode.mSize = mSize;
		lNode.mTags = mTags;

		for ( std::deque< Node >::const_iterator lIt = mChildren->begin(); lIt != mChildren->end(); ++lIt )
		{
			lNode.mChildren->push_back ( lIt->clone() );
		}

		for ( std::deque< Node >::iterator lIt = lNode.mChildren->begin(); lIt != lNode.mChildren->end(); ++lIt )
		{
			lNode.mChildrenMap->insert ( std::make_pair ( lIt->mUid , & ( *lIt ) ) );

			for ( std::hash_map< std::string , Node* >::iterator lSubMapIt = lIt->mChildrenMap->begin() ; lSubMapIt != lIt->mChildrenMap->end() ; ++lSubMapIt )
			{
				lNode.mChildrenMap->insert ( std::make_pair ( ( lIt->mUid ) +'.'+ ( lSubMapIt->first ) , lSubMapIt->second ) );
			}
		}

		//std::cout << "Cloning " << mUid << " Children: " << mChildren->size() << "->" << lNode.mChildren->size() << " ChildrenMap: " << mChildrenMap->size() << "->" << lNode.mChildrenMap->size() << std::endl;
		return lNode;
	}


	Node::~Node()
	{}


	bool Node::operator == ( const Node& aNode )
	{
		try
		{
			return this->getAddress() == aNode.getAddress() &&
				   this->getMask() == aNode.getMask() &&
				   this->getPermission() == aNode.getPermission() &&
				   this->getId() == aNode.getId();
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}


	const std::string& Node::getId() const
	{
		try
		{
			return mUid;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	const uint32_t& Node::getAddress() const
	{
		try
		{
			return mAddr;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	const uint32_t& Node::getMask() const
	{
		try
		{
			return mMask;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	const defs::BlockReadWriteMode& Node::getMode() const
	{
		try
		{
			return mMode;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	const uint32_t& Node::getSize() const
	{
		try
		{
			return mSize;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	const defs::NodePermission& Node::getPermission() const
	{
		try
		{
			return mPermission;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}


	const std::string& Node::getTags() const
	{
		try
		{
			return mTags;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}




	void Node::stream ( std::ostream& aStream , std::size_t aIndent ) const
	{
		try
		{
			aStream << std::setfill ( '0' ) << std::uppercase;
			aStream << '\n' << std::string ( aIndent , ' ' ) << "+ ";
			aStream << "Node \"" << mUid << "\", ";

			switch ( mMode )
			{
				case defs::SINGLE:
					aStream << "SINGLE register, "
					<< std::hex << "Address 0x" << std::setw ( 8 ) << mAddr << ", "
					<< std::hex << "Mask 0x" << std::setw ( 8 ) << mMask << ", ";
					break;
				case defs::INCREMENTAL:
					aStream << "INCREMENTAL block, "
					<< std::dec << "Size " << mSize << ", "
					<< std::hex << "Addresses [0x" << std::setw ( 8 ) << mAddr << "-" << std::setw ( 8 ) << ( mAddr+mSize-1 ) << "], ";
					break;
				case defs::NON_INCREMENTAL:
					aStream << "NON-INCREMENTAL block, ";

					if ( mSize )
					{
						aStream << std::dec << "Size " << mSize << ", ";
					}

					aStream << std::hex << "Address 0x"  << std::setw ( 8 ) << mAddr << ", ";
					break;
			}

			aStream << "Permissions " << ( mPermission&defs::READ?'r':'-' ) << ( mPermission&defs::WRITE?'w':'-' ) ;
			// for ( std::hash_map< std::string , Node* >::const_iterator lIt = mChildrenMap->begin(); lIt != mChildrenMap->end(); ++lIt )
			// {
			// aStream << '\n' << std::string ( aIndent+2 , ' ' ) << "- Map entry " << (lIt->first);
			// }

			for ( std::deque< Node >::const_iterator lIt = mChildren->begin(); lIt != mChildren->end(); ++lIt )
			{
				lIt->stream ( aStream , aIndent+2 );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}


	Node& Node::getNode ( const std::string& aId )
	{
		try
		{
			std::hash_map< std::string , Node* >::iterator lIt = mChildrenMap->find ( aId );

			if ( lIt==mChildrenMap->end() )
			{
				log ( Error() , "No branch found with ID-path " ,  Quote ( aId ) );
				std::size_t lPos ( std::string::npos );
				bool lPartialMatch ( false );

				while ( true )
				{
					lPos = aId.rfind ( '.' , lPos );

					if ( lPos == std::string::npos )
					{
						break;
					}

					std::hash_map< std::string , Node* >::iterator lIt = mChildrenMap->find ( aId.substr ( 0 , lPos ) );

					if ( lIt!=mChildrenMap->end() )
					{
						log ( Error() , "Partial match " ,  Quote ( aId.substr ( 0 , lPos ) ) , " found for ID-path " ,  Quote ( aId ) );
						log ( Error() , "Tree structure of partial match is:" , * ( lIt->second ) );
						lPartialMatch = true;
						break;
					}

					lPos--;
				}

				if ( !lPartialMatch )
				{
					log ( Error() , "Not even a partial match found for ID-path " ,  Quote ( aId ) , ". If this address looks correct, please check for leading, trailing and stray whitespace.\nTree structure is:" , *this );
				}

				NoBranchFoundWithGivenUID().throwFrom ( ThisLocation() );
			}

			return * ( lIt->second );
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}


	std::vector<std::string> Node::getNodes()
	{
		try
		{
			std::vector<std::string> lNodes;
			lNodes.reserve ( mChildrenMap->size() ); //prevent reallocations

			for ( std::hash_map< std::string , Node* >::iterator lIt = mChildrenMap->begin(); lIt != mChildrenMap->end(); ++lIt )
			{
				lNodes.push_back ( lIt->first );
			}

			return lNodes;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	std::vector<std::string> Node::getNodes ( const std::string& aRegex )
	{
		try
		{
			std::vector<std::string> lNodes;
			lNodes.reserve ( mChildrenMap->size() ); //prevent reallocations
			log ( Info() , "Regular Expression : " , aRegex );

			for ( std::hash_map< std::string , Node* >::iterator lIt = mChildrenMap->begin(); lIt != mChildrenMap->end(); ++lIt )
			{
				boost::cmatch lMatch;

				if ( boost::regex_match ( lIt->first.c_str() , lMatch , boost::regex ( aRegex ) ) ) //to allow partial match, add  boost::match_default|boost::match_partial  as fourth argument
				{
					log ( Info() , lIt->first , " matches" );
					lNodes.push_back ( lIt->first );
				}
			}

			//bit dirty but since the hash map sorts them by the hash, not the value, they are completely scrambled here making it very hard to use.
			std::sort ( lNodes.begin(), lNodes.end() );
			return lNodes;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	ValHeader  Node::write ( const uint32_t& aValue )
	{
		try
		{
			if ( mPermission & defs::WRITE )
			{
				if ( mMask == defs::NOMASK )
				{
					return mHw->getClient()->write ( mAddr , aValue );
				}
				else
				{
					return mHw->getClient()->write ( mAddr , aValue , mMask );
				}
			}
			else
			{
				log ( Error() , "Node permissions denied write access" );
				WriteAccessDenied().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}


	ValHeader  Node::writeBlock ( const std::vector< uint32_t >& aValues ) // , const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			if ( ( mMode == defs::SINGLE ) && ( aValues.size() != 1 ) ) //We allow the user to call a bulk access of size=1 to a single register
			{
				log ( Error() , "Bulk Transfer requested on single register node" );
				log ( Error() , "If you were expecting an incremental write, please modify your address file to add the 'mode=",  Quote ( "incremental" ) , "' flags there" );
				BulkTransferOnSingleRegister().throwFrom ( ThisLocation() );
			}
			else
			{
				if ( ( mSize != 1 ) && ( aValues.size() >mSize ) )
				{
					log ( Error() , "Requested bulk write of greater size than the specified endpoint size" );
					BulkTransferRequestedTooLarge().throwFrom ( ThisLocation() );
				}

				if ( mPermission & defs::WRITE )
				{
					return mHw->getClient()->writeBlock ( mAddr , aValues , mMode ); //aMode );
				}
				else
				{
					log ( Error() , "Node permissions denied write access" );
					WriteAccessDenied().throwFrom ( ThisLocation() );
				}
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
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
				log ( Error() , "Node permissions denied read access" );
				ReadAccessDenied().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}


	ValVector< uint32_t > Node::readBlock ( const uint32_t& aSize ) //, const defs::BlockReadWriteMode& aMode )
	{
		try
		{
			if ( ( mMode == defs::SINGLE ) && ( aSize != 1 ) ) //We allow the user to call a bulk access of size=1 to a single register
			{
				log ( Error() , "Bulk Transfer requested on single register node" );
				log ( Error() , "If you were expecting an incremental read, please modify your address file to add the 'mode=",  Quote ( "incremental" ) , "' flags there" );
				BulkTransferOnSingleRegister().throwFrom ( ThisLocation() );
			}
			else
			{
				if ( ( mSize != 1 ) && ( aSize>mSize ) )
				{
					log ( Error() , "Requested bulk read of greater size than the specified endpoint size" );
					BulkTransferRequestedTooLarge().throwFrom ( ThisLocation() );
				}

				if ( mPermission & defs::READ )
				{
					return mHw->getClient()->readBlock ( mAddr , aSize , mMode ); //aMode );
				}
				else
				{
					log ( Error() , "Node permissions denied read access" );
					ReadAccessDenied().throwFrom ( ThisLocation() );
				}
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

	// ValWord< int32_t > Node::readSigned()
	// {
	// try
	// {
	// if ( mPermission & defs::READ )
	// {
	// if ( mMask == defs::NOMASK )
	// {
	// return mHw->getClient()->readSigned ( mAddr );
	// }
	// else
	// {
	// return mHw->getClient()->readSigned ( mAddr , mMask );
	// }
	// }
	// else
	// {
	// log ( Error() , "Node permissions denied read access" );
	// ReadAccessDenied().throwFrom ( ThisLocation() );
	// }
	// }
	// catch ( uhal::exception& aExc )
	// {
	// aExc.rethrowFrom ( ThisLocation() );
	// }
	// catch ( const std::exception& aExc )
	// {
	// StdException ( aExc ).throwFrom ( ThisLocation() );
	// }
	// }


	// ValVector< int32_t > Node::readBlockSigned ( const uint32_t& aSize ) //, const defs::BlockReadWriteMode& aMode )
	// {
	// try
	// {
	// if ( ( mMode == defs::SINGLE ) && ( aSize != 1 ) ) //We allow the user to call a bulk access of size=1 to a single register
	// {
	// log ( Error() , "Bulk Transfer requested on single register node" );
	// log ( Error() , "If you were expecting an incremental read, please modify your address file to add the 'mode=",  Quote ( "incremental" ) , "' flags there" );
	// BulkTransferOnSingleRegister().throwFrom ( ThisLocation() );
	// }
	// else
	// {
	// if ( ( mSize != 1 ) && ( aSize>mSize ) )
	// {
	// log ( Error() , "Requested bulk read of greater size than the specified endpoint size" );
	// BulkTransferRequestedTooLarge().throwFrom ( ThisLocation() );
	// }

	// if ( mPermission & defs::READ )
	// {
	// return mHw->getClient()->readBlockSigned ( mAddr , aSize , mMode ); //aMode );
	// }
	// else
	// {
	// log ( Error() , "Node permissions denied read access" );
	// ReadAccessDenied().throwFrom ( ThisLocation() );
	// }
	// }
	// }
	// catch ( uhal::exception& aExc )
	// {
	// aExc.rethrowFrom ( ThisLocation() );
	// }
	// catch ( const std::exception& aExc )
	// {
	// StdException ( aExc ).throwFrom ( ThisLocation() );
	// }
	// }




	ValWord< uint32_t > Node::rmw_bits ( const uint32_t& aANDterm , const uint32_t& aORterm )
	{
		try
		{
			if ( mPermission == defs::READWRITE )
			{
				return mHw->getClient()->rmw_bits ( mAddr , aANDterm , aORterm );
			}
			else
			{
				log ( Error() , "Node permissions denied read/write access" );
				ReadAccessDenied().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}



	ValWord< int32_t > Node::rmw_sum ( const int32_t& aAddend )
	{
		try
		{
			if ( mPermission == defs::READWRITE )
			{
				return mHw->getClient()->rmw_sum ( mAddr , aAddend );
			}
			else
			{
				log ( Error() , "Node permissions denied read/write access" );
				ReadAccessDenied().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}



	void Node::calculateHierarchicalAddresses ( const uint32_t& aAddr , const Node& aTopLevelNode )
	{
		try
		{
			if ( mAddr )
			{
				if ( mMode == defs::INCREMENTAL )
				{
					uint64_t lCurrentTop ( ( uint64_t ) ( mAddr ) + ( uint64_t ) ( mSize-1 ) );

					//Test for overlap with parent
					if ( ( uint32_t ) ( lCurrentTop ) & aAddr ) //should set the most significant bit of the child address and then AND this with the parent address
					{
						log ( Warning() , "The partial address of the top register in the current branch, " , Quote ( mUid ) , " , (" , Integer ( ( uint32_t ) ( lCurrentTop ) , IntFmt<hex,fixed>() ) , ") overlaps with the partial address of the parent branch (" , Integer ( aAddr , IntFmt<hex,fixed>() ) , "). This is in violation of the hierarchical design principal. For now this is a warning, but in the future this may be upgraded to throw an exception." );
					}

					//Update the addresses with parent address
					mAddr |= aAddr;
					lCurrentTop = ( uint64_t ) ( mAddr ) + ( uint64_t ) ( mSize-1 );
					// log ( Error() , Quote ( mUid ) ,
					// " : Size " , Integer ( mSize , IntFmt<hex,fixed>() ) ,
					// " : Base Address " , Integer ( mAddr , IntFmt<hex,fixed>() ) ,
					// " : lCurrentTop " ,  Integer ( lCurrentTop , IntFmt<hex,fixed>() )
					// );

					//Check that the requested block size does not extend outside register space
					if ( lCurrentTop >> 32 )
					{
						log ( Error() , "A block size of " , Integer ( mSize ) , " and a base address of " , Integer ( mAddr , IntFmt<hex,fixed>() ) , " exceeds bounds of address space" );
						ArraySizeExceedsRegisterBound().throwFrom ( ThisLocation() );
					}

					//Compare against all other branches
					for ( std::hash_map< std::string , Node* >::const_iterator lIt = aTopLevelNode.mChildrenMap->begin() ; lIt != aTopLevelNode.mChildrenMap->end() ; ++lIt )
					{
						const Node& lComparison ( *lIt->second );

						if ( lComparison.mAddrValid )
						{
							if ( lComparison.mMode == defs::INCREMENTAL )
							{
								// Current and comparison are both incremental
								uint32_t lComparisonTop ( lComparison.mAddr + ( lComparison.mSize-1 ) ); //Since the comparison is already marked as valid, we know that the top must be within the register space, or an exception would have been thrown

								if ( ( ( lComparisonTop >= mAddr ) && ( lComparisonTop <= lCurrentTop ) ) || ( ( lCurrentTop >= lComparison.mAddr ) && ( lCurrentTop <= lComparisonTop ) ) )
								{
									log ( Error() , "Branch " , Quote ( mUid ) ,
										  " has address range [" , Integer ( mAddr , IntFmt<hex,fixed>() ) , " - " , Integer ( lCurrentTop , IntFmt<hex,fixed>() ) ,
										  "] which overlaps with branch " , Quote ( lComparison.mUid ) ,
										  " which has address range [" , Integer ( lComparison.mAddr , IntFmt<hex,fixed>() ) , " - " , Integer ( lComparisonTop , IntFmt<hex,fixed>() ) ,
										  "]."
										);
									AddressSpaceOverlap().throwFrom ( ThisLocation() );
								}
							}
							else
							{
								// Current is incremental, comparison is static
								if ( ( lComparison.mAddr >= mAddr ) && ( lComparison.mAddr <= lCurrentTop ) )
								{
									log ( Error() , "Branch " , Quote ( mUid ) ,
										  " has address range [" , Integer ( mAddr , IntFmt<hex,fixed>() ) , " - " , Integer ( lCurrentTop , IntFmt<hex,fixed>() ) ,
										  "] which overlaps with branch " , Quote ( lComparison.mUid ) ,
										  " which has address " , Integer ( lComparison.mAddr , IntFmt<hex,fixed>() ) , "]."
										);
									AddressSpaceOverlap().throwFrom ( ThisLocation() );
								}
							}
						}
					}
				}
				else
				{
					//Test for overlap with parent
					if ( mAddr & aAddr )
					{
						log ( Warning() , "The partial address of the current branch, " , Quote ( mUid ) , " , (" , Integer ( mAddr , IntFmt<hex,fixed>() ) , ") overlaps with the partial address of the parent branch (" , Integer ( aAddr , IntFmt<hex,fixed>() ) , "). This is in violation of the hierarchical design principal. For now this is a warning, but in the future this may be upgraded to throw an exception." );
					}

					//Update the addresses with parent address
					mAddr |= aAddr;

					for ( std::hash_map< std::string , Node* >::const_iterator lIt = aTopLevelNode.mChildrenMap->begin() ; lIt != aTopLevelNode.mChildrenMap->end() ; ++lIt )
					{
						const Node& lComparison ( *lIt->second );

						if ( lComparison.mAddrValid )
						{
							if ( lComparison.mMode == defs::INCREMENTAL )
							{
								// Current is static, comparison is incremental
								uint32_t lComparisonTop ( lComparison.mAddr + ( lComparison.mSize-1 ) ); //Since the comparison is already marked as valid, we know that the top must be within the register space, or an exception would have been thrown

								if ( ( mAddr >= lComparison.mAddr ) && ( mAddr <= lComparisonTop ) )
								{
									log ( Error() , "Branch " , Quote ( mUid ) ,
										  " has address " , Integer ( mAddr , IntFmt<hex,fixed>() ) ,
										  " which overlaps with branch " , Quote ( lComparison.mUid ) ,
										  " which has address range [" , Integer ( lComparison.mAddr , IntFmt<hex,fixed>() ) , " - " , Integer ( lComparisonTop , IntFmt<hex,fixed>() ) ,
										  "]."
										);
									AddressSpaceOverlap().throwFrom ( ThisLocation() );
								}
							}
							else
							{
								// Current and comparison are both static
								if ( mAddr == lComparison.mAddr )
								{
									if ( mMask & lComparison.mMask )
									{
										log ( Error() , "Branch " , Quote ( mUid ) ,
											  " has address " , Integer ( mAddr , IntFmt<hex,fixed>() ) ,
											  " and mask " , Integer ( mMask , IntFmt<hex,fixed>() ) ,
											  " which overlaps with branch " , Quote ( lComparison.mUid ) ,
											  " which has address " , Integer ( lComparison.mAddr , IntFmt<hex,fixed>() ) ,
											  " and mask " , Integer ( lComparison.mMask , IntFmt<hex,fixed>() )
											);
										AddressSpaceOverlap().throwFrom ( ThisLocation() );
									}
								}
							}
						}
					}
				}
			}
			else
			{
				mAddr = aAddr;
			}

			for ( std::deque< Node >::iterator lIt = mChildren->begin(); lIt != mChildren->end(); ++lIt )
			{
				lIt->calculateHierarchicalAddresses ( mAddr , aTopLevelNode );
			}

			mAddrValid = true;
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}


	boost::shared_ptr<ClientInterface> Node::getClient()
	{
		try
		{
			return mHw->getClient();
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}

}
