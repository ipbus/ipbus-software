#include "uhal/ClientImplementation.hpp"

#include "uhal/AddressTableBuilder.hpp"
#include "uhal/Utilities.hpp"
#include "uhal/log.hpp"

#include <boost/algorithm/string.hpp>

namespace uhal
{

	AddressTableBuilder* AddressTableBuilder::mInstance = NULL;

	AddressTableBuilder& AddressTableBuilder::getInstance()
	{
		try
		{
			if ( mInstance == NULL )
			{
				mInstance = new AddressTableBuilder();
			}

			return *mInstance;
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}

	Node AddressTableBuilder::getAddressTable ( const std::string& aFilenameExpr , const uint32_t& aAddr , const uint32_t& aAddrMask )
	{
		try
		{
			std::vector< std::pair<std::string, std::string> >  lAddressFiles;
			uhal::utilities::ParseSemicolonDelimitedUriList<true> ( aFilenameExpr , lAddressFiles );

			if ( lAddressFiles.size() != 1 )
			{
				pantheios::log_ERROR ( "Exactly one address table file must be specified. The expression \"" , aFilenameExpr , "\" contains " , pantheios::integer ( lAddressFiles.size() ) , " valid file expressions." );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw IncorrectAddressTableFileCount();
			}

			std::vector< Node > lNodes;

			if ( !uhal::utilities::OpenFile ( lAddressFiles[0].first , lAddressFiles[0].second , boost::bind ( &AddressTableBuilder::CallBack, boost::ref ( *this ) , _1 , _2 , _3 , aAddr , aAddrMask , boost::ref ( lNodes ) ) ) )
			{
				pantheios::log_ERROR ( "Failed to open address table file \"" , lAddressFiles[0].second , "\"" );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw FailedToOpenAddressTableFile();
			}

			if ( lNodes.size() != 1 )
			{
				pantheios::log_ERROR ( "Exactly one address table file must be specified. The expression \"" , lAddressFiles[0].second , "\" refers to " , pantheios::integer ( lNodes.size() ) , " valid files." );
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw IncorrectAddressTableFileCount();
			}

			return lNodes[0];
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	void AddressTableBuilder::CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile , const uint32_t& aAddr , const uint32_t& aAddrMask , std::vector< Node >& aNodes )
	{
		try
		{
			std::hash_map< std::string , Node >::iterator lNodeIt = mNodes.find ( aProtocol+ ( aPath.string() ) );

			if ( lNodeIt != mNodes.end() )
			{
				aNodes.push_back ( lNodeIt->second );
				return;
			}

			std::string lExtension ( aPath.extension().string().substr ( 0,4 ) );
			boost::to_lower ( lExtension ); //just in case someone decides to use capitals in their file extensions.

			if ( lExtension == ".xml" )
			{
				pantheios::log_INFORMATIONAL ( "XML file" );
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
					pantheios::log_ERROR ( "No XML node called \"node\" in file " , lazy_stream_inserter ( aPath ) );
					return;
				}

				Node lNode ( lXmlNode , aAddr , aAddrMask );
				aNodes.push_back ( lNode );
				return;
			}
			else if ( lExtension == ".txt" )
			{
				pantheios::log_INFORMATIONAL ( "TXT file" );
				pantheios::log_ERROR ( "Parser problems mean that this method has been disabled. Please fix me! Please?!?" );
				pantheios::log_ERROR ( "At " , ThisLocation() );
				return;
				/*
				uhal::OldHalEntryGrammar lGrammar;
				uhal::OldHalSkipParser lParser;
				std::vector< utilities::OldHalEntryType > lResponse;

				std::vector<uint8_t>::iterator lBegin( aFile.begin() );
				std::vector<uint8_t>::iterator lEnd( aFile.end() );

				boost::spirit::qi::phrase_parse( lBegin , lEnd , lGrammar , lParser , lResponse );

				for( std::vector< utilities::OldHalEntryType >::iterator lIt = lResponse.begin() ; lIt != lResponse.end() ; ++lIt ){
					//pantheios::log_INFORMATIONAL ( "---------------------------------------------------\n" , *lIt );
				}

				//pantheios::log_INFORMATIONAL ( "Remaining:" );
				for( ; lBegin != lEnd ; ++lBegin ){
					//pantheios::log_INFORMATIONAL ( *lBegin;
				}
				std::cout );
				*/
			}
			else
			{
				pantheios::log_ERROR ( "Extension \"" , lExtension , "\" not known." );
				return;
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}



}
