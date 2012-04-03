#include "uhal/ConnectionManager.hpp"

#include "uhal/Node.hpp"
#include "uhal/AddressTableBuilder.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/ClientFactory.hpp"
#include "uhal/Utilities.hpp"

#include "uhal/log.hpp"


namespace uhal
{
	
	
	ConnectionManager::tConnectionDescriptor::tConnectionDescriptor( const pugi::xml_node& aNode , bool& aSuccess ){
		aSuccess=false;
		if ( ! uhal::utilities::GetXMLattribute<true>( aNode , "id" , id ) ) return;
		if ( ! uhal::utilities::GetXMLattribute<true>( aNode , "uri" , uri ) ) return;
		if ( ! uhal::utilities::GetXMLattribute<true>( aNode , "address_table" , address_table ) ) return;
		aSuccess=true;
	}			
	
	bool ConnectionManager::tConnectionDescriptor::operator==( const tConnectionDescriptor& aConnectionDescriptor ) const{
		if( id != aConnectionDescriptor.id ) return false;
		if( uri != aConnectionDescriptor.uri ) return false;
		if( address_table != aConnectionDescriptor.address_table ) return false;
		return true;
	}
		




	//!Given a glob expression, parse all the files matching it (e.g. $BUILD/config/*.xml). If one parsing fails throw an exception and return filename and line number
	ConnectionManager::ConnectionManager ( const std::string& aFilenameExpr )
	{
		uhal::utilities::ParseSemicolonDelimitedUriList<true>( aFilenameExpr , mConnectionFiles );
		for ( std::vector< std::pair<std::string, std::string> >::iterator lIt = mConnectionFiles.begin() ; lIt != mConnectionFiles.end() ; ++lIt ){
			uhal::utilities::OpenFile( lIt->first , lIt->second , boost::bind( &ConnectionManager::CallBack, boost::ref(*this) , _1 , _2 , _3 ) );	
		}
	}
	
	ConnectionManager::~ConnectionManager () {}
	
	
	/**
	 * Retrieves protocol, host, and port from the connection file to create the ClientInterface.
	 * Retrieves the address table file from the connection file to create the HwInterface.
	 */
	HwInterface ConnectionManager::getDevice ( const std::string& aId )
	{
		if( mConnectionDescriptors.size() == 0 ){
			pantheios::log_ALERT ( "Connection map contains no entries" );
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw ConnectionUIDDoesNotExist();			
		}
	
		std::map< std::string, tConnectionDescriptor >::iterator lIt = mConnectionDescriptors.find( aId );
		
		if( lIt == mConnectionDescriptors.end() ){
			pantheios::log_ALERT ( aId , " does not exist in connection map" );
			pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
			throw ConnectionUIDDoesNotExist();
		}
		
		Node lNode = AddressTableBuilder::getInstance().getAddressTable( lIt->second.address_table );
		pantheios::log_INFORMATIONAL ( "ConnectionManager created node tree: " , lazy_inserter(lNode) );

		boost::shared_ptr<ClientInterface> lClientInterface = ClientFactory::getInstance().getClient ( lIt->second.id , lIt->second.uri );	
		
		return HwInterface ( lClientInterface , lNode );
	}


	//Given a regex return the ids that match the
	std::vector<std::string> ConnectionManager::getDevices ( )
	{
		std::vector<std::string> lDevices;
		lDevices.reserve( mConnectionDescriptors.size() ); //prevent reallocations

		for ( std::map< std::string, tConnectionDescriptor >::iterator lIt = mConnectionDescriptors.begin() ; lIt != mConnectionDescriptors.end() ; ++lIt ){
			lDevices.push_back ( lIt->first );
		}
		
		return lDevices;
	}	
	
	//Given a regex return the ids that match the
	std::vector<std::string> ConnectionManager::getDevices ( const boost::regex& aRegex )
	{
		std::vector<std::string> lDevices;
		lDevices.reserve( mConnectionDescriptors.size() ); //prevent reallocations

		for ( std::map< std::string, tConnectionDescriptor >::iterator lIt = mConnectionDescriptors.begin() ; lIt != mConnectionDescriptors.end() ; ++lIt ){
			boost::cmatch lMatch;
			if( boost::regex_match( lIt->first.c_str() , lMatch , aRegex ) ) //to allow partial match, add  boost::match_default|boost::match_partial  as fourth argument
			{
				lDevices.push_back ( lIt->first );
			}
		}
		
		return lDevices;
	}

	std::vector<std::string> ConnectionManager::getDevices ( const char* aRegex )
	{
		return getDevices ( boost::regex( aRegex ) );
	}

	std::vector<std::string> ConnectionManager::getDevices ( const std::string& aRegex )
	{
		return getDevices ( boost::regex( aRegex ) );
	}	




	void ConnectionManager::CallBack( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile ){

		std::pair< std::set< std::string >::iterator , bool > lInsert = mPreviouslyOpenedFiles.insert( aProtocol+(aPath.string()) );
		if( ! lInsert.second )
		{
			pantheios::log_INFORMATIONAL ( "File \"" , (aProtocol+(aPath.string())) , "\" has already been parsed. I am not reparsing and will continue with next document for now but be aware!" );
			return;
		}
		
		pugi::xml_document lXmlDocument;

		pugi::xml_parse_result lLoadResult = lXmlDocument.load_buffer_inplace( &(aFile[0]) , aFile.size() );
		if( !lLoadResult ){
			//Mark says to throw on this condition, I will leave it continuing for now...
			uhal::utilities::PugiXMLParseResultPrettifier( lLoadResult , aPath , aFile );			
			return;
		}
		
		pugi::xpath_node_set lConnections = lXmlDocument.select_nodes("/connections/connection" );
		
		for (pugi::xpath_node_set::const_iterator lConnectionIt = lConnections.begin(); lConnectionIt != lConnections.end(); ++lConnectionIt )
		{
			bool lSuccess;
			tConnectionDescriptor lDescriptor( lConnectionIt->node() , lSuccess );
				
			if ( lSuccess ){
				std::pair< std::map< std::string, tConnectionDescriptor >::iterator , bool > lInsert = mConnectionDescriptors.insert( std::make_pair( lDescriptor.id , lDescriptor ) );
				if( !lInsert.second ){
					if( lInsert.first->second == lDescriptor ){
						pantheios::log_INFORMATIONAL ( "Duplicate connection entry found:"
														"\n > id = " , lDescriptor.id , 
														"\n > uri = " , lDescriptor.uri , 
														"\n > address_table = " , lDescriptor.address_table , 
														"\n Continuing for now but be aware!" );
					}else{
						pantheios::log_ALERT ( "Duplicate connection ID found but parameters do not match! Bailing!" );
						pantheios::log_ALERT ( "Throwing at " , ThisLocation() );
						throw DuplicatedUID();
					}
				}								
			}else{
				pantheios::log_ERROR ( "Construction of Connection Descriptor failed. Continuing with next Connection Descriptor for now but be aware!" );
			}
				
		}
			
	}
			
				
			

}



