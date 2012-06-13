
namespace uhal
{

	template< typename T >
	template <class InputIterator>
	void ValVector<T>::assign ( InputIterator aBegin , InputIterator aEnd )
	{
		try
		{
			if ( !/* *mValid */ mMembers->valid )
			{
				/* mValues-> */ mMembers->value.assign ( aBegin , aEnd );
			}
			else
			{
				log ( Error() , "Throwing at " , ThisLocation() );
				throw ValMemImutabilityViolation();
			}
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			throw uhal::exception ( aExc );
		}
	}


}


