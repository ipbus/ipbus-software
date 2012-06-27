

namespace uhal
{

	template< typename T>
	T& Node::getNode ( const std::string& aId )
	{
		try
		{
			return dynamic_cast< T > ( getNode ( aId ) );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

}

