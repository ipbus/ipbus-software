
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
				pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				throw ValMemImutabilityViolation();
			}
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


}


