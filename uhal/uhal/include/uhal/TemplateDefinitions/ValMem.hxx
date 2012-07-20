
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
				ValMemImutabilityViolation().throwFrom ( ThisLocation() );
			}
		}
		catch ( uhal::exception& aExc )
		{
			aExc.rethrowFrom ( ThisLocation() );
		}
		catch ( const std::exception& aExc )
		{
			log ( Error() , "Exception \"" , aExc.what() , "\" caught at " , ThisLocation() );
			StdException ( aExc ).throwFrom ( ThisLocation() );
		}
	}


}


