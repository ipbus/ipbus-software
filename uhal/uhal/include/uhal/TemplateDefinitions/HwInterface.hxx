

namespace uhal
{

	template< typename T>
	T& HwInterface::getNode ( const std::string& aId )
	{
		try
		{
			return mNode->getNode< T > ( aId );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception " , Quote( aExc.what() ) , " caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}

}

