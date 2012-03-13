#include "uhal/ClientImplementation.hpp"

#include "uhal/AddressTableBuilder.hpp"

#include "uhal/Utilities.hpp"


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
			std::cout << "Exactly one address table file must be specified. The expression \"" << aFilenameExpr << "\" contains " << lAddressFiles.size() << " valid file expressions." << std::endl;
			throw IncorrectAddressTableFileCount();
		}

		std::vector< Node > lAddressTables;
		uhal::utilities::OpenFile( lAddressFiles[0].first , lAddressFiles[0].second , boost::bind( &AddressTableBuilder::CallBack, boost::ref(*this) , _1 , _2 , _3 ) , lAddressTables );	
		
		
		if( lAddressTables.size() != 1 ){
			std::cout << "Exactly one address table file must be specified. The expression \"" << lAddressFiles[0].second << "\" contains " << lAddressTables.size() << " valid file expressions." << std::endl;
			throw IncorrectAddressTableFileCount();
		}
		
		return lAddressTables[0];

	}

	
	Node AddressTableBuilder::CallBack( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile ){
	
		std::map< std::string , Node >::iterator lAddressTableIt = mAddressTables.find( aProtocol+(aPath.string()) );
		if( lAddressTableIt != mAddressTables.end() ){
			return lAddressTableIt->second;
		}
	
		std::string lExtension( aPath.extension().string() );
		if( lExtension == ".xml" ){
			std::cout << "XML file" << std::endl;
			
			pugi::xml_document lXmlDocument;

			
			
			pugi::xml_parse_result lLoadResult = lXmlDocument.load_buffer_inplace( &(aFile[0]) , aFile.size() );
			if( !lLoadResult ){
				uhal::utilities::PugiXMLParseResultPrettifier( lLoadResult , aPath , aFile );			
				throw FailedToParseAddressTableFile();
			}
			
			
			pugi::xpath_node lTopNode = lXmlDocument.select_single_node("//node");
			return Node( lTopNode.node() );
					
		}else if( lExtension == ".txt" ) {
			std::cout << "TXT file" << std::endl;
			/*
			uhal::OldHalEntryGrammar lGrammar;
			uhal::OldHalSkipParser lParser;
			std::vector< utilities::OldHalEntryType > lResponse;
			
			std::vector<uint8_t>::iterator lBegin( aFile.begin() );
			std::vector<uint8_t>::iterator lEnd( aFile.end() );
			
			boost::spirit::qi::phrase_parse( lBegin , lEnd , lGrammar , lParser , lResponse );
			
			for( std::vector< utilities::OldHalEntryType >::iterator lIt = lResponse.begin() ; lIt != lResponse.end() ; ++lIt ){
				std::cout << "---------------------------------------------------\n" << *lIt << std::endl;
			}
			
			std::cout << "Remaining:" << std::endl;
			for( ; lBegin != lEnd ; ++lBegin ){
				std::cout << *lBegin;
			}
			std::cout << std::endl;
			*/
		}else{
		
		}
		
	
	}

	
	
}
