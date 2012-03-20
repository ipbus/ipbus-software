#include "uhal/ClientImplementation.hpp"

#include "uhal/AddressTableBuilder.hpp"

#include "uhal/Utilities.hpp"

#include "uhal/log.hpp"

namespace uhal
{

    AddressTableBuilder* AddressTableBuilder::mInstance = NULL;

    AddressTableBuilder& AddressTableBuilder::getInstance()
    {
        if ( mInstance == NULL ) {
            mInstance = new AddressTableBuilder();
        }

        return *mInstance;
    }
	
	Node AddressTableBuilder::getAddressTable( const std::string& aFilenameExpr )
	{
		
		std::vector< std::pair<std::string, std::string> >  lAddressFiles;
		uhal::utilities::ParseSemicolonDelimitedUriList<true>( aFilenameExpr , lAddressFiles );
	
		if( lAddressFiles.size() != 1 ){
			pantheios::log_ALERT ( "Exactly one address table file must be specified. The expression \"" , aFilenameExpr , "\" contains " , pantheios::integer(lAddressFiles.size()) , " valid file expressions." );
			throw IncorrectAddressTableFileCount();
		}

		std::vector< Node > lNodes;
		
		pantheios::log_LOCATION;
		uhal::utilities::OpenFile( lAddressFiles[0].first , lAddressFiles[0].second , boost::bind( &AddressTableBuilder::CallBack, boost::ref(*this) , _1 , _2 , _3 , boost::ref(lNodes) ) );	
		pantheios::log_LOCATION;
			
		if( lNodes.size() != 1 ){
			pantheios::log_ALERT ( "Exactly one address table file must be specified. The expression \"" , lAddressFiles[0].second , "\" contains " , pantheios::integer(lNodes.size()) , " valid file expressions." );
			throw IncorrectAddressTableFileCount();
		}
		
		return lNodes[0];

	}

	
	void AddressTableBuilder::CallBack( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile , std::vector< Node >& aNodes ){
	
		std::map< std::string , Node >::iterator lNodeIt = mNodes.find( aProtocol+(aPath.string()) );
		if( lNodeIt != mNodes.end() ){
			aNodes.push_back( lNodeIt->second );
			return;
		}
	
		std::string lExtension( aPath.extension().string() );
		if( lExtension == ".xml" ){
			pantheios::log_INFORMATIONAL ( "XML file" );
			
			pugi::xml_document lXmlDocument;
			
			pugi::xml_parse_result lLoadResult = lXmlDocument.load_buffer_inplace( &(aFile[0]) , aFile.size() );
			if( !lLoadResult ){
				uhal::utilities::PugiXMLParseResultPrettifier( lLoadResult , aPath , aFile );			
				throw FailedToParseAddressTableFile();
			}

			pantheios::log_LOCATION;
			pugi::xml_node lXmlNode = lXmlDocument.child("node");
			pantheios::log_LOCATION;

			if( lXmlNode ){
				pantheios::log_INFORMATIONAL ( "Returned Valid" );
			}else{
				pantheios::log_INFORMATIONAL ( "Returned Invalid" );			
			}
			
			Node lNode( lXmlNode );
			pantheios::log_LOCATION;

			aNodes.push_back( lNode );
			
			pantheios::log_LOCATION;
		
			return;
					
		}else if( lExtension == ".txt" ) {
			//pantheios::log_INFORMATIONAL ( "TXT file" );
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
		}else{
		
		}
		
	
	}

	
	
}
