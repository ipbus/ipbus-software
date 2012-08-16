#include "uhal/ClientImplementation.hpp"

#include "uhal/NodeTreeBuilder.hpp"
#include "uhal/Utilities.hpp"
#include "uhal/log/log.hpp"

#include <boost/algorithm/string.hpp>

namespace uhal
{

	const char* NodeTreeBuilder::mIdAttribute = "id";
	const char* NodeTreeBuilder::mAddressAttribute = "address";
	const char* NodeTreeBuilder::mTagsAttribute = "tags";
	const char* NodeTreeBuilder::mPermissionsAttribute = "permission";
	const char* NodeTreeBuilder::mMaskAttribute = "mask";
	const char* NodeTreeBuilder::mModeAttribute = "mode";
	const char* NodeTreeBuilder::mSizeAttribute = "size";
	const char* NodeTreeBuilder::mClassAttribute = "class";
	const char* NodeTreeBuilder::mModuleAttribute = "module";



	NodeTreeBuilder* NodeTreeBuilder::mInstance = NULL;


	NodeTreeBuilder::NodeTreeBuilder ()
	{
		//------------------------------------------------------------------------------------------------------------------------
		Rule<Node*> lPlainNode;
		lPlainNode.forbid ( NodeTreeBuilder::mClassAttribute )
		.forbid ( NodeTreeBuilder::mModuleAttribute )
		.forbid ( NodeTreeBuilder::mMaskAttribute )
		.optional ( NodeTreeBuilder::mIdAttribute )
		.optional ( NodeTreeBuilder::mAddressAttribute )
		.optional ( NodeTreeBuilder::mPermissionsAttribute )
		.optional ( NodeTreeBuilder::mModeAttribute )
		.optional ( NodeTreeBuilder::mSizeAttribute )
		.optional ( NodeTreeBuilder::mTagsAttribute );
		//------------------------------------------------------------------------------------------------------------------------
		Rule<Node*> lClass;
		lClass.require ( NodeTreeBuilder::mClassAttribute )
		.forbid ( NodeTreeBuilder::mMaskAttribute )
		.forbid ( NodeTreeBuilder::mModuleAttribute )
		.optional ( NodeTreeBuilder::mIdAttribute )
		.optional ( NodeTreeBuilder::mAddressAttribute )
		.optional ( NodeTreeBuilder::mModeAttribute )
		.optional ( NodeTreeBuilder::mSizeAttribute )
		.optional ( NodeTreeBuilder::mPermissionsAttribute )
		.optional ( NodeTreeBuilder::mTagsAttribute );
		//------------------------------------------------------------------------------------------------------------------------
		Rule<Node*> lBitMask;
		lBitMask.require ( NodeTreeBuilder::mMaskAttribute )
		.forbid ( NodeTreeBuilder::mClassAttribute )
		.forbid ( NodeTreeBuilder::mModuleAttribute )
		.optional ( NodeTreeBuilder::mAddressAttribute )		// .forbid ( NodeTreeBuilder::mAddressAttribute ) //see https://svnweb.cern.ch/trac/cactus/ticket/92
		.forbid ( NodeTreeBuilder::mModeAttribute )
		.forbid ( NodeTreeBuilder::mSizeAttribute )
		.optional ( NodeTreeBuilder::mPermissionsAttribute )
		.optional ( NodeTreeBuilder::mTagsAttribute );
		//------------------------------------------------------------------------------------------------------------------------
		mTopLevelNodeParser.addRule ( lPlainNode , boost::bind ( &NodeTreeBuilder::plainNodeCreator , this , false , _1 ) );
		mTopLevelNodeParser.addRule ( lClass , boost::bind ( &NodeTreeBuilder::classNodeCreator , this , false , _1 ) );
		mTopLevelNodeParser.addRule ( lBitMask , boost::bind ( &NodeTreeBuilder::bitmaskNodeCreator , this , false , _1 ) );
		//------------------------------------------------------------------------------------------------------------------------
		lPlainNode.require ( NodeTreeBuilder::mIdAttribute );
		lClass.require ( NodeTreeBuilder::mIdAttribute );
		lBitMask.require ( NodeTreeBuilder::mIdAttribute );
		//------------------------------------------------------------------------------------------------------------------------
		Rule<Node*> lModule;
		lModule.require ( NodeTreeBuilder::mIdAttribute )
		.require ( NodeTreeBuilder::mModuleAttribute )
		.forbid ( NodeTreeBuilder::mMaskAttribute )
		.forbid ( NodeTreeBuilder::mClassAttribute )
		.forbid ( NodeTreeBuilder::mModeAttribute )
		.forbid ( NodeTreeBuilder::mSizeAttribute )
		.forbid ( NodeTreeBuilder::mPermissionsAttribute )
		.optional ( NodeTreeBuilder::mAddressAttribute )
		.optional ( NodeTreeBuilder::mTagsAttribute );
		//------------------------------------------------------------------------------------------------------------------------
		mNodeParser.addRule ( lPlainNode , boost::bind ( &NodeTreeBuilder::plainNodeCreator , this , true , _1 ) );
		mNodeParser.addRule ( lClass , boost::bind ( &NodeTreeBuilder::classNodeCreator , this , true , _1 ) );
		mNodeParser.addRule ( lBitMask , boost::bind ( &NodeTreeBuilder::bitmaskNodeCreator , this , true , _1 ) );
		mNodeParser.addRule ( lModule , boost::bind ( &NodeTreeBuilder::moduleNodeCreator , this , _1 ) );
		//------------------------------------------------------------------------------------------------------------------------
	}

	NodeTreeBuilder::~NodeTreeBuilder () {}



	NodeTreeBuilder& NodeTreeBuilder::getInstance()
	{
		try
		{
			if ( mInstance == NULL )
			{
				mInstance = new NodeTreeBuilder();
			}

			return *mInstance;
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

	Node* NodeTreeBuilder::getNodeTree ( const std::string& aFilenameExpr , const boost::filesystem::path& aPath )
	{
		try
		{
			std::vector< std::pair<std::string, std::string> >  lAddressFiles;
			uhal::utilities::ParseSemicolonDelimitedUriList<true> ( aFilenameExpr , lAddressFiles );

			if ( lAddressFiles.size() != 1 )
			{
				log ( Error() , "Exactly one address table file must be specified. The expression " , Quote ( aFilenameExpr ) , " contains " , Integer ( lAddressFiles.size() ) , " valid file expressions." );
				IncorrectAddressTableFileCount().throwFrom ( ThisLocation() );
			}

			std::vector< const Node* > lNodes;

			if ( !uhal::utilities::OpenFile ( lAddressFiles[0].first , lAddressFiles[0].second , aPath.parent_path() , boost::bind ( &NodeTreeBuilder::CallBack, boost::ref ( *this ) , _1 , _2 , _3 , boost::ref ( lNodes ) ) ) )
			{
				log ( Error() , "Failed to open address table file " , Quote ( lAddressFiles[0].second ) );
				FailedToOpenAddressTableFile().throwFrom ( ThisLocation() );
			}

			if ( lNodes.size() != 1 )
			{
				log ( Error() , "Exactly one address table file must be specified. The expression " , Quote ( lAddressFiles[0].second ) , " refers to " , Integer ( lNodes.size() ) , " valid files." );
				IncorrectAddressTableFileCount().throwFrom ( ThisLocation() );
			}

			Node* lNode ( lNodes[0]->clone() );
			return lNode;
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


	void NodeTreeBuilder::CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile , std::vector< const Node* >& aNodes )
	{
		try
		{
			std::string lName ( aProtocol + ( aPath.string() ) );
			std::hash_map< std::string , const Node* >::iterator lNodeIt = mNodes.find ( lName );

			if ( lNodeIt != mNodes.end() )
			{
				aNodes.push_back ( lNodeIt->second );
				return;
			}

			std::string lExtension ( aPath.extension().string().substr ( 0,4 ) );
			boost::to_lower ( lExtension ); //just in case someone decides to use capitals in their file extensions.

			if ( lExtension == ".xml" )
			{
				log ( Info() , "XML file" );
				pugi::xml_document lXmlDocument;
				pugi::xml_parse_result lLoadResult = lXmlDocument.load_buffer_inplace ( & ( aFile[0] ) , aFile.size() );

				if ( !lLoadResult )
				{
					uhal::utilities::PugiXMLParseResultPrettifier ( lLoadResult , aPath , aFile );
					return;
				}

				pugi::xml_node lXmlNode = lXmlDocument.child ( "node" );

				if ( !lXmlNode )
				{
					log ( Error() , "No XML node called ", Quote ( "node" ) , " in file " , aPath.c_str() );
					return;
				}

				mFileCallStack.push_back ( aPath );
				Node* lNode ( mTopLevelNodeParser ( lXmlNode ) );
				mFileCallStack.pop_back( );
				calculateHierarchicalAddresses ( lNode , 0x00000000 );
				//checkForAddressCollisions ( lNode );  // Needs further investigation - disabled for now as it causes exceptions with valid tables.
				mNodes.insert ( std::make_pair ( lName , lNode ) );
				aNodes.push_back ( lNode );
				return;
			}
			else if ( lExtension == ".txt" )
			{
				log ( Info() , "TXT file" );
				log ( Error() , "Parser problems mean that this method has been disabled." );
				log ( Error() , "At " , ThisLocation() );
				return;
				/*
				uhal::OldHalEntryGrammar lGrammar;
				uhal::OldHalSkipParser lParser;
				std::vector< utilities::OldHalEntryType > lResponse;

				std::vector<uint8_t>::iterator lBegin( aFile.begin() );
				std::vector<uint8_t>::iterator lEnd( aFile.end() );

				boost::spirit::qi::phrase_parse( lBegin , lEnd , lGrammar , lParser , lResponse );

				for( std::vector< utilities::OldHalEntryType >::iterator lIt = lResponse.begin() ; lIt != lResponse.end() ; ++lIt ){
					//log ( Info() , "---------------------------------------------------\n" , *lIt );
				}

				//log ( Info() , "Remaining:" );
				for( ; lBegin != lEnd ; ++lBegin ){
					//log ( Info() , *lBegin;
				}
				std::cout );
				*/
			}
			else
			{
				log ( Error() , "Extension " , Quote ( lExtension ) , " not known." );
				return;
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




	Node* NodeTreeBuilder::plainNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode )
	{
		try
		{
			Node* lNode ( new Node() );
			setUid ( aRequireId , aXmlNode , lNode );
			setAddr ( aXmlNode , lNode );
			setTags ( aXmlNode , lNode );
			setPermissions ( aXmlNode , lNode );
			//setMask( aXmlNode , lNode );
			setModeAndSize ( aXmlNode , lNode );
			addChildren ( aXmlNode , lNode );
			log ( Debug() , lNode->mUid , " built by " , __PRETTY_FUNCTION__ );
			return lNode;
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


	Node* NodeTreeBuilder::classNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode )
	{
		try
		{
			std::string lClassStr;
			uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mClassAttribute , lClassStr );
			std::string::const_iterator lBegin ( lClassStr.begin() );
			std::string::const_iterator lEnd ( lClassStr.end() );
			NodeTreeClassAttribute lClass;
			boost::spirit::qi::phrase_parse ( lBegin , lEnd , mNodeTreeClassAttributeGrammar , boost::spirit::ascii::space , lClass );
			std::hash_map< std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( lClass.mClass );

			if ( lIt == mCreators.end() )
			{
				log ( Error() , "Class " , Quote ( lClass.mClass ) , " is unknown to the NodeTreeBuilder class factory. Known types are:" );

				for ( std::hash_map< std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.begin() ; lIt != mCreators.end() ; ++lIt )
				{
					log ( Error() , " > " , lIt->first );
				}

				LabelUnknownToClassFactory().throwFrom ( ThisLocation() );
			}

			Node* lNode ( lIt->second->create ( lClass.mArguments ) );
			setUid ( aRequireId , aXmlNode , lNode );
			setAddr ( aXmlNode , lNode );
			setTags ( aXmlNode , lNode );
			setPermissions ( aXmlNode , lNode );
			//setMask( aXmlNode , lNode );
			setModeAndSize ( aXmlNode , lNode );
			addChildren ( aXmlNode , lNode );
			log ( Debug() , lNode->mUid , " built by " , __PRETTY_FUNCTION__ );
			return lNode;
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


	Node* NodeTreeBuilder::moduleNodeCreator ( const pugi::xml_node& aXmlNode )
	{
		try
		{
			std::string lModule;
			uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mModuleAttribute , lModule );
			Node* lNode ( getNodeTree ( lModule , mFileCallStack.back( ) ) );
			setUid ( true , aXmlNode , lNode );
			setAddr ( aXmlNode , lNode );
			setTags ( aXmlNode , lNode );
			//setPermissions( aXmlNode , lNode );
			//setMask( aXmlNode , lNode );
			//setModeAndSize( aXmlNode , lNode );
			//addChildren( aXmlNode , lNode );
			log ( Debug() , lNode->mUid , " built by " , __PRETTY_FUNCTION__ );
			return lNode;
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


	Node* NodeTreeBuilder::bitmaskNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode )
	{
		try
		{
			if ( aXmlNode.child ( "node" ) )
			{
				log ( Error() , "Bit-masked nodes are not allowed to have child nodes" );
				MaskedNodeCannotHaveChild().throwFrom ( ThisLocation() );
			}

			Node* lNode ( new Node() );
			setUid ( aRequireId , aXmlNode , lNode );
			setAddr ( aXmlNode , lNode ); //was commented out, see https://svnweb.cern.ch/trac/cactus/ticket/92
			setTags ( aXmlNode , lNode );
			setPermissions ( aXmlNode , lNode );
			setMask ( aXmlNode , lNode );
			//setModeAndSize( aXmlNode , lNode );
			//addChildren( aXmlNode , lNode );
			log ( Debug() , lNode->mUid , " built by " , __PRETTY_FUNCTION__ );
			return lNode;
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




	void NodeTreeBuilder::setUid ( const bool& aRequireId , const pugi::xml_node& aXmlNode , Node* aNode )
	{
		try
		{
			if ( aRequireId )
			{
				if ( ! uhal::utilities::GetXMLattribute<true> ( aXmlNode , NodeTreeBuilder::mIdAttribute , aNode->mUid ) )
				{
					//error description is given in the function itself so no more elaboration required
					NodeMustHaveUID().throwFrom ( ThisLocation() );
				}
			}
			else
			{
				uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mIdAttribute , aNode->mUid );
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

	void NodeTreeBuilder::setAddr ( const pugi::xml_node& aXmlNode , Node* aNode )
	{
		try
		{
			//Address is an optional attribute for hierarchical addressing
			uint32_t lAddr ( 0 );
			uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mAddressAttribute , lAddr );
			aNode->mPartialAddr |= lAddr;
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

	void NodeTreeBuilder::setTags ( const pugi::xml_node& aXmlNode , Node* aNode )
	{
		try
		{
			std::string lStr;
			//Tags is an optional attribute to allow the user to add a description to a node
			uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mTagsAttribute , lStr );

			if ( lStr.size() && aNode->mTags.size() )
			{
				aNode->mTags += "[";
				aNode->mTags += lStr;
				aNode->mTags += "]";
			}
			else if ( lStr.size() && !aNode->mTags.size() )
			{
				aNode->mTags = lStr;
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

	void NodeTreeBuilder::setPermissions ( const pugi::xml_node& aXmlNode , Node* aNode )
	{
		try
		{
			//Permissions is an optional attribute for specifying read/write permissions
			std::string lPermission;

			if ( uhal::utilities::GetXMLattribute<false> ( aXmlNode , "permission" , lPermission ) )
			{
				try
				{
					boost::spirit::qi::phrase_parse (
						lPermission.begin(),
						lPermission.end(),
						NodeTreeBuilder::mPermissionsLut,
						boost::spirit::ascii::space,
						aNode->mPermission
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


	void NodeTreeBuilder::setMask ( const pugi::xml_node& aXmlNode , Node* aNode )
	{
		try
		{
			//Tags is an optional attribute to allow the user to add a description to a node
			uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mMaskAttribute , aNode->mMask );
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


	void NodeTreeBuilder::setModeAndSize ( const pugi::xml_node& aXmlNode , Node* aNode )
	{
		try
		{
			//Mode is an optional attribute for specifying whether a block is incremental, non-incremental or a single register
			std::string lMode;

			if ( uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mModeAttribute , lMode ) )
			{
				try
				{
					boost::spirit::qi::phrase_parse (
						lMode.begin(),
						lMode.end(),
						NodeTreeBuilder::mModeLut,
						boost::spirit::ascii::space,
						aNode->mMode
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

				if ( aNode->mMode == defs::INCREMENTAL )
				{
					//If a block is incremental it requires a size attribute
					if ( ! uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mSizeAttribute , aNode->mSize ) )
					{
						log ( Error() , "Node " , Quote ( aNode->mUid ) , " has type " , Quote ( "INCREMENTAL" ) , ", which requires a " , Quote ( NodeTreeBuilder::mSizeAttribute ) , " attribute" );
						IncrementalNodeRequiresSizeAttribute().throwFrom ( ThisLocation() );
					}
				}
				else if ( aNode->mMode == defs::NON_INCREMENTAL )
				{
					//If a block is non-incremental, then a size attribute is recommended
					if ( ! uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mSizeAttribute , aNode->mSize ) )
					{
						log ( Notice() , "Node " , Quote ( aNode->mUid ) , " has type " , Quote ( "NON_INCREMENTAL" ) , " but does not have a " , Quote ( NodeTreeBuilder::mSizeAttribute ) , " attribute. This is not necessarily a problem, but if there is a limit to the size of the read/write operation from this port, then please consider adding this attribute for the sake of safety." );
					}
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



	void NodeTreeBuilder::addChildren ( const pugi::xml_node& aXmlNode , Node* aNode )
	{
		try
		{
			pugi::xml_node lXmlNode = aXmlNode.child ( "node" );

			if ( aNode->mMode != defs::SINGLE )
			{
				if ( lXmlNode )
				{
					log ( Error() , "Block access nodes are not allowed to have child nodes" );
					BlockAccessNodeCannotHaveChild().throwFrom ( ThisLocation() );
				}
			}
			else
			{
				for ( ; lXmlNode; lXmlNode = lXmlNode.next_sibling ( "node" ) )
				{
					aNode->mChildren.push_back ( mNodeParser ( lXmlNode ) );
				}

				for ( std::deque< Node* >::iterator lIt = aNode->mChildren.begin(); lIt != aNode->mChildren.end(); ++lIt )
				{
					aNode->mChildrenMap.insert ( std::make_pair ( ( **lIt ).mUid , *lIt ) );

					for ( std::hash_map< std::string , Node* >::iterator lSubMapIt = ( **lIt ).mChildrenMap.begin() ; lSubMapIt != ( **lIt ).mChildrenMap.end() ; ++lSubMapIt )
					{
						aNode->mChildrenMap.insert ( std::make_pair ( ( **lIt ).mUid +'.'+ ( lSubMapIt->first ) , lSubMapIt->second ) );
					}
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


	void NodeTreeBuilder::calculateHierarchicalAddresses ( Node* aNode , const uint32_t& aAddr )
	{
		try
		{
			if ( aNode->mMode == defs::INCREMENTAL )
			{
				uint64_t lTopAddr ( ( uint64_t ) ( aNode->mPartialAddr ) + ( uint64_t ) ( aNode->mSize-1 ) );

				//Check that the requested block size does not extend outside register space
				if ( lTopAddr >> 32 )
				{
					log ( Error() , "A block size of " , Integer ( aNode->mSize ) , " and a base address of " , Integer ( aNode->mAddr , IntFmt<hex,fixed>() ) , " exceeds bounds of address space" );
					ArraySizeExceedsRegisterBound().throwFrom ( ThisLocation() );
				}

				//Test for overlap with parent
				if ( ( uint32_t ) ( lTopAddr ) & aAddr ) //should set the most significant bit of the child address and then AND this with the parent address
				{
					log ( Warning() , "The partial address of the top register in the current branch, " , Quote ( aNode->mUid ) , " , (" , Integer ( ( uint32_t ) ( lTopAddr ) , IntFmt<hex,fixed>() ) , ") overlaps with the partial address of the parent branch (" , Integer ( aAddr , IntFmt<hex,fixed>() ) , "). This is in violation of the hierarchical design principal. For now this is a warning, but in the future this may be upgraded to throw an exception." );
				}
			}
			else
			{
				//Test for overlap with parent
				if ( aNode->mPartialAddr & aAddr ) //should set the most significant bit of the child address and then AND this with the parent address
				{
					log ( Warning() , "The partial address of the top register in the current branch, " , Quote ( aNode->mUid ) , " , (" , Integer ( aNode->mPartialAddr , IntFmt<hex,fixed>() ) , ") overlaps with the partial address of the parent branch (" , Integer ( aAddr , IntFmt<hex,fixed>() ) , "). This is in violation of the hierarchical design principal. For now this is a warning, but in the future this may be upgraded to throw an exception." );
				}
			}

			aNode->mAddr = aNode->mPartialAddr | aAddr;

			for ( std::deque< Node* >::iterator lIt = aNode->mChildren.begin(); lIt != aNode->mChildren.end(); ++lIt )
			{
				calculateHierarchicalAddresses ( *lIt , aNode->mAddr );
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



	void NodeTreeBuilder::checkForAddressCollisions ( Node* aNode )
	{
		try
		{
			std::hash_map< std::string , Node* >::iterator lIt, lIt2;
			Node* lNode1, *lNode2;

			for ( lIt = aNode->mChildrenMap.begin() ; lIt != aNode->mChildrenMap.end() ; ++lIt )
			{
				lNode1 = lIt->second;
				lIt2 = lIt;
				lIt2++;

				if ( lNode1->mMode == defs::INCREMENTAL )
				{
					uint32_t lBottom1 ( lNode1->mAddr );
					uint32_t lTop1 ( lNode1->mAddr + ( lNode1->mSize - 1 ) );

					for ( ; lIt2 != aNode->mChildrenMap.end() ; ++lIt2 )
					{
						lNode2 = lIt2->second;

						if ( lNode2->mMode == defs::INCREMENTAL )
						{
							//Node1 and Node2 are both incremental
							uint32_t lBottom2 ( lNode2->mAddr );
							uint32_t lTop2 ( lNode2->mAddr + ( lNode2->mSize - 1 ) );

							if ( ( ( lTop2 >= lBottom1 ) && ( lTop2 <= lTop1 ) ) || ( ( lTop1 >= lBottom2 ) && ( lTop1 <= lTop2 ) ) )
							{
								log ( Error() , "Branch " , Quote ( lIt->first ) ,
									  " has address range [" , Integer ( lBottom1 , IntFmt<hex,fixed>() ) , " - " , Integer ( lTop1 , IntFmt<hex,fixed>() ) ,
									  "] which overlaps with branch " , Quote ( lIt2->first ) ,
									  " which has address range [" , Integer ( lBottom2 , IntFmt<hex,fixed>() ) , " - " , Integer ( lTop2 , IntFmt<hex,fixed>() ) ,
									  "]."
									);
								AddressSpaceOverlap().throwFrom ( ThisLocation() );
							}
						}
						else
						{
							//Node1 is incremental and Node2 is single address
							uint32_t lAddr2 ( lNode2->mAddr );

							if ( ( lAddr2 >= lBottom1 ) && ( lAddr2 <= lTop1 ) )
							{
								log ( Error() , "Branch " , Quote ( lIt->first ) ,
									  " has address range [" , Integer ( lBottom1 , IntFmt<hex,fixed>() ) , " - " , Integer ( lTop1 , IntFmt<hex,fixed>() ) ,
									  "] which overlaps with branch " , Quote ( lIt2->first ) ,
									  " which has address " , Integer ( lAddr2 , IntFmt<hex,fixed>() ) , "]."
									);
								AddressSpaceOverlap().throwFrom ( ThisLocation() );
							}
						}
					}
				}
				else
				{
					uint32_t lAddr1 ( lNode1->mAddr );

					for ( ; lIt2 != aNode->mChildrenMap.end() ; ++lIt2 )
					{
						lNode2 = lIt2->second;

						if ( lNode2->mMode == defs::INCREMENTAL )
						{
							//Node1 is single address and Node2 is incremental
							uint32_t lBottom2 ( lNode2->mAddr );
							uint32_t lTop2 ( lNode2->mAddr + ( lNode2->mSize - 1 ) );

							if ( ( lAddr1 >= lBottom2 ) && ( lAddr1 <= lTop2 ) )
							{
								log ( Error() , "Branch " , Quote ( lIt->first ) ,
									  " has address " , Integer ( lAddr1 , IntFmt<hex,fixed>() ) ,
									  "] which overlaps with branch " , Quote ( lIt2->first ) ,
									  " which has address range [" , Integer ( lBottom2 , IntFmt<hex,fixed>() ) , " - " , Integer ( lTop2 , IntFmt<hex,fixed>() ) , "]."
									);
								AddressSpaceOverlap().throwFrom ( ThisLocation() );
							}
						}
						else
						{
							//Node1 and Node2 are both single addresses
							uint32_t lAddr2 ( lNode2->mAddr );

							if ( lAddr1 == lAddr2 )
							{
								if ( lNode1->mMask & lNode2->mMask )
								{
									log ( Error() , "Branch " , Quote ( lIt->first ) ,
										  " has address " , Integer ( lAddr1 , IntFmt<hex,fixed>() ) ,
										  " and mask " , Integer ( lNode1->mMask , IntFmt<hex,fixed>() ) ,
										  " which overlaps with branch " , Quote ( lIt2->first ) ,
										  " which has address " , Integer ( lAddr2 , IntFmt<hex,fixed>() ) ,
										  " and mask " , Integer ( lNode2->mMask , IntFmt<hex,fixed>() )
										);
									AddressSpaceOverlap().throwFrom ( ThisLocation() );
								}
							}
						}
					}
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






	NodeTreeBuilder::permissions_lut::permissions_lut()
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

	const NodeTreeBuilder::permissions_lut NodeTreeBuilder::mPermissionsLut;


	NodeTreeBuilder::mode_lut::mode_lut()
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

	const NodeTreeBuilder::mode_lut NodeTreeBuilder::mModeLut;

}
