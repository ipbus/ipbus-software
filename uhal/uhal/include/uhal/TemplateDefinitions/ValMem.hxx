
namespace uhal
{

	template< typename T >

_ValWord_<T>::_ValWord_ ( const T& aValue , const bool& aValid , const uint32_t aMask ) try :
		value ( aValue ) ,
			  valid ( aValid ) ,
			  mask ( aMask )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}


	template< typename T >

_ValVector_<T>::_ValVector_ ( const std::vector<T>& aValue , const bool& aValid ) try :
		value ( aValue ) ,
			  valid ( aValid )
		{}
	catch ( const std::exception& aExc )
	{
		pantheios::log_EXCEPTION ( aExc );
		throw uhal::exception ( aExc );
	}


	template< typename T >
	template <class InputIterator>
	void ValVector<T>::assign ( InputIterator aFirst , InputIterator aLast )
	{
		try
		{
			if ( !/* *mValid */ mMembers->valid )
			{
				/* mValues-> */ mMembers->value.assign ( aFirst , aLast );
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


