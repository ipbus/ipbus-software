#include "uhal/ClientImplementation.hpp"

#include "uhal/NodeTreeBuilder.hpp"
#include "uhal/Utilities.hpp"
#include "uhal/log/log.hpp"

#include <boost/algorithm/string.hpp>

namespace uhal
{

	NodeTreeBuilder* NodeTreeBuilder::mInstance = NULL;

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

	boost::shared_ptr< Node > NodeTreeBuilder::getNodeTree ( const std::string& aFilenameExpr , const boost::filesystem::path& aPath , const bool& aCalculateHierarchicalAddresses )
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

			std::vector< boost::shared_ptr< const Node > > lNodes;

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

			boost::shared_ptr< Node > lNode ( new Node ( lNodes[0]->clone() ) );

			if ( aCalculateHierarchicalAddresses )
			{
				lNode->calculateHierarchicalAddresses ( 0x0 , *lNode );
			}

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


	void NodeTreeBuilder::CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile , std::vector< boost::shared_ptr< const Node > >& aNodes )
	{
		try
		{
			std::string lName ( aProtocol + ( aPath.string() ) );
			std::hash_map< std::string , boost::shared_ptr< const Node > >::iterator lNodeIt = mNodes.find ( lName );

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

				boost::shared_ptr< const Node > lNode ( create ( lXmlNode , aPath , false ) );
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



	NodeTreeBuilder::NodeTreeBuilder () {}

	NodeTreeBuilder::~NodeTreeBuilder () {}



	boost::shared_ptr< const Node > NodeTreeBuilder::create ( const pugi::xml_node& aXmlNode , const boost::filesystem::path& aPath , const bool& aRequireId )
	{
		std::string lClass;
		boost::shared_ptr< const Node > lNode;

		if ( uhal::utilities::GetXMLattribute<false> ( aXmlNode , "class" , lClass ) )
		{
			std::hash_map< std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( lClass );

			if ( lIt != mCreators.end() )
			{
				lNode = lIt->second->create ( aXmlNode , aPath , aRequireId );
			}
			else
			{
				std::stringstream lStr;

				for ( std::hash_map< std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt2 = mCreators.begin() ; lIt2 != mCreators.end() ; ++lIt2 )
				{
					lStr << "\n > " << lIt2->first ;
				}

				log ( Warning() , "Node subclass " , Quote ( lClass ) , " does not exists in map of creators. Options are:" , lStr.str() , "\nWill create a plain base node for now but be warned." );
				lNode = boost::shared_ptr< const Node > ( new Node ( aXmlNode , aPath , aRequireId ) );
			}
		}
		else
		{
			lNode = boost::shared_ptr< const Node > ( new Node ( aXmlNode , aPath , aRequireId ) );
		}

		return lNode;
	}



}
