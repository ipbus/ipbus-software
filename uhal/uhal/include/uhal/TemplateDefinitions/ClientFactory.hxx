
namespace uhal
{

	template <class T>
	void ClientFactory::add ( const std::string& aProtocol , const std::string& aDescription )
	{
		try
		{
			std::hash_map<std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( aProtocol ) ;

			if ( lIt != mCreators.end() )
			{
				//pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				//throw ProtocolAlreadyExist();
				pantheios::log_WARNING ( "Protocol \"" , aProtocol , "\" already exists in map of creators. Continuing for now, but be warned." );
				return;
			}

			mCreators[aProtocol] =  boost::shared_ptr<CreatorInterface> ( new Creator<T>() );
			if( aDescription.size() == 0 ){
				mProductDescriptions.insert( std::make_pair( aProtocol , T::description() ) );
			}else{
				mProductDescriptions.insert( std::make_pair( aProtocol , aDescription ) );
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


	template <class T>
	boost::shared_ptr<ClientInterface> ClientFactory::Creator<T>::create ( const std::string& aId , const URI& aUri )
	{
		try
		{
			return boost::shared_ptr<ClientInterface> ( new T ( aId , aUri ) );
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


}
