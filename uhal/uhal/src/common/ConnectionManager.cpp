#include "uhal/ConnectionManager.hpp"

#include "uhal/Node.hpp"
#include "uhal/NodeTreeBuilder.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/ClientFactory.hpp"
#include "uhal/Utilities.hpp"

#include "log/log.hpp"


namespace uhal
{


	ConnectionManager::ConnectionDescriptor::ConnectionDescriptor ( const pugi::xml_node& aNode , bool& aSuccess ) try
	{
		aSuccess=false;

		if ( ! uhal::utilities::GetXMLattribute<true> ( aNode , "id" , id ) )
		{
			return;
		}

		if ( ! uhal::utilities::GetXMLattribute<true> ( aNode , "uri" , uri ) )
		{
			return;
		}

		if ( ! uhal::utilities::GetXMLattribute<true> ( aNode , "address_table" , address_table ) )
		{
			return;
		}

		aSuccess=true;
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw uhal::exception ( aExc );
	}

	bool ConnectionManager::ConnectionDescriptor::operator== ( const ConnectionDescriptor& aConnectionDescriptor ) const
	{
		try
		{
			if ( id != aConnectionDescriptor.id )
			{
				return false;
			}

			if ( uri != aConnectionDescriptor.uri )
			{
				return false;
			}

			if ( address_table != aConnectionDescriptor.address_table )
			{
				return false;
			}

			return true;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}





	// Given a glob expression, parse all the files matching it (e.g. $BUILD/config/*.xml). If one parsing fails throw an exception and return filename and line number

	ConnectionManager::ConnectionManager ( const std::string& aFilenameExpr ) try
	{
		uhal::utilities::ParseSemicolonDelimitedUriList<true> ( aFilenameExpr , mConnectionFiles );

		for ( std::vector< std::pair<std::string, std::string> >::iterator lIt = mConnectionFiles.begin() ; lIt != mConnectionFiles.end() ; ++lIt )
		{
			uhal::utilities::OpenFile ( lIt->first , lIt->second , boost::bind ( &ConnectionManager::CallBack, boost::ref ( *this ) , _1 , _2 , _3 ) );
		}
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
		throw uhal::exception ( aExc );
	}

	ConnectionManager::~ConnectionManager () {}


	/*
		Retrieves protocol, host, and port from the connection file to create the ClientInterface.
		Retrieves the address table file from the connection file to create the HwInterface.
	*/
	HwInterface ConnectionManager::getDevice ( const std::string& aId )
	{
		try
		{
			if ( mConnectionDescriptors.size() == 0 )
			{
				log ( Error() , "Connection map contains no entries" );
				log ( Error() , "Throwing at " , ThisLocation() );
				throw ConnectionUIDDoesNotExist();
			}

			std::map< std::string, ConnectionDescriptor >::iterator lIt = mConnectionDescriptors.find ( aId );

			if ( lIt == mConnectionDescriptors.end() )
			{
				log ( Error() , aId , " does not exist in connection map" );
				log ( Error() , "Throwing at " , ThisLocation() );
				throw ConnectionUIDDoesNotExist();
			}

			//The node tree builder returns a newly created shared_ptr to a Node
			boost::shared_ptr< const Node > lNode = NodeTreeBuilder::getInstance().getNodeTree ( lIt->second.address_table );
			log ( Info() , "ConnectionManager created node tree: " , *lNode );
			boost::shared_ptr<ClientInterface> lClientInterface = ClientFactory::getInstance().getClient ( lIt->second.id , lIt->second.uri );
			return HwInterface ( lClientInterface , lNode );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}


	//Given a regex return the ids that match the
	std::vector<std::string> ConnectionManager::getDevices ( )
	{
		try
		{
			std::vector<std::string> lDevices;
			lDevices.reserve ( mConnectionDescriptors.size() ); //prevent reallocations

			for ( std::map< std::string, ConnectionDescriptor >::iterator lIt = mConnectionDescriptors.begin() ; lIt != mConnectionDescriptors.end() ; ++lIt )
			{
				lDevices.push_back ( lIt->first );
			}

			return lDevices;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	//Given a regex return the ids that match the
	std::vector<std::string> ConnectionManager::getDevices ( const boost::regex& aRegex )
	{
		try
		{
			std::vector<std::string> lDevices;
			lDevices.reserve ( mConnectionDescriptors.size() ); //prevent reallocations

			for ( std::map< std::string, ConnectionDescriptor >::iterator lIt = mConnectionDescriptors.begin() ; lIt != mConnectionDescriptors.end() ; ++lIt )
			{
				boost::cmatch lMatch;

				if ( boost::regex_match ( lIt->first.c_str() , lMatch , aRegex ) ) //to allow partial match, add  boost::match_default|boost::match_partial  as fourth argument
				{
					lDevices.push_back ( lIt->first );
				}
			}

			return lDevices;
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	std::vector<std::string> ConnectionManager::getDevices ( const char* aRegex )
	{
		try
		{
			return getDevices ( boost::regex ( aRegex ) );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

	std::vector<std::string> ConnectionManager::getDevices ( const std::string& aRegex )
	{
		try
		{
			return getDevices ( boost::regex ( aRegex ) );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}




	void ConnectionManager::CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile )
	{
		try
		{
			std::pair< std::set< std::string >::iterator , bool > lInsert = mPreviouslyOpenedFiles.insert ( aProtocol+ ( aPath.string() ) );

			if ( ! lInsert.second )
			{
				log ( Info() , "File \"" , ( aProtocol+ ( aPath.string() ) ) , "\" has already been parsed. I am not reparsing and will continue with next document for now but be aware!" );
				return;
			}

			pugi::xml_document lXmlDocument;
			pugi::xml_parse_result lLoadResult = lXmlDocument.load_buffer_inplace ( & ( aFile[0] ) , aFile.size() );

			if ( !lLoadResult )
			{
				//Mark says to throw on this condition, I will leave it continuing for now...
				uhal::utilities::PugiXMLParseResultPrettifier ( lLoadResult , aPath , aFile );
				return;
			}

			pugi::xpath_node_set lConnections = lXmlDocument.select_nodes ( "/connections/connection" );

			for ( pugi::xpath_node_set::const_iterator lConnectionIt = lConnections.begin(); lConnectionIt != lConnections.end(); ++lConnectionIt )
			{
				bool lSuccess;
				ConnectionDescriptor lDescriptor ( lConnectionIt->node() , lSuccess );

				if ( lSuccess )
				{
					std::pair< std::map< std::string, ConnectionDescriptor >::iterator , bool > lInsert = mConnectionDescriptors.insert ( std::make_pair ( lDescriptor.id , lDescriptor ) );

					if ( !lInsert.second )
					{
						if ( lInsert.first->second == lDescriptor )
						{
							log ( Info() , "Duplicate connection entry found:"
								  "\n > id = " , lDescriptor.id ,
								  "\n > uri = " , lDescriptor.uri ,
								  "\n > address_table = " , lDescriptor.address_table ,
								  "\n Continuing for now but be aware!" );
						}
						else
						{
							log ( Error() , "Duplicate connection ID found but parameters do not match! Bailing!" );
							log ( Error() , "Throwing at " , ThisLocation() );
							throw DuplicatedUID();
						}
					}
				}
				else
				{
					log ( Error() , "Construction of Connection Descriptor failed. Continuing with next Connection Descriptor for now but be aware!" );
				}
			}
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}




}



