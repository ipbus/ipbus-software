

namespace uhal
{

	template< typename T>
	T& Node::getNode ( const std::string& aId )
	{
		try
		{
			return dynamic_cast< T > ( getNode ( aId ) );
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

}

