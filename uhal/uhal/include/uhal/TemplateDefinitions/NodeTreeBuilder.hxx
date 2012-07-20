
namespace uhal
{

	template <class T>
	void NodeTreeBuilder::add ( const std::string& aNodeTypeIdentifier )
	{
		try
		{
			std::hash_map<std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( aNodeTypeIdentifier ) ;

			if ( lIt != mCreators.end() )
			{
				//
				//ProtocolAlreadyExist().throwFrom( ThisLocation() );
				log ( Warning() , "Protocol \"" , aNodeTypeIdentifier , "\" already exists in map of creators. Continuing for now, but be warned." );
				return;
			}

			mCreators[aNodeTypeIdentifier] =  boost::shared_ptr<CreatorInterface> ( new Creator<T>() );
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


	template <class T>
	boost::shared_ptr< const Node > NodeTreeBuilder::Creator<T>::create ( const pugi::xml_node& aXmlNode , const boost::filesystem::path& aPath )
	{
		try
		{
			return boost::shared_ptr< const Node > ( new T ( aXmlNode , aPath ) );
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


	template< typename T >
	NodeTreeBuilder::RegistrationHelper<T>::RegistrationHelper ( const std::string& aNodeTypeIdentifier )
	{
		NodeTreeBuilder::getInstance().add< T > ( aNodeTypeIdentifier );
	}

}
