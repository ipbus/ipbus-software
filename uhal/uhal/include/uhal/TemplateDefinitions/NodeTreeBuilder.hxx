
namespace uhal
{

	template <class T>
	void NodeTreeBuilder::add ( const std::string& aNodeClassName )
	{
		try
		{
			std::hash_map<std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( aNodeClassName ) ;

			if ( lIt != mCreators.end() )
			{
				//
				//ProtocolAlreadyExist().throwFrom( ThisLocation() );
				log ( Warning() , "Protocol \"" , aNodeClassName , "\" already exists in map of creators. Continuing for now, but be warned." );
				return;
			}

			mCreators[aNodeClassName] =  boost::shared_ptr<CreatorInterface> ( new Creator<T>() );
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
	Node* NodeTreeBuilder::Creator<T>::create ( const std::vector< std::pair<std::string, std::string> >& aAttributes )
	{
		try
		{
			return new T ( aAttributes );
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
	RegistrationHelper< T >::RegistrationHelper ( const std::string& aDerivedClassName )
	{
		NodeTreeBuilder::getInstance().add< T > ( aDerivedClassName );
	}


}
