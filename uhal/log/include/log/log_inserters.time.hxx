
#include <log/log_configuration.hpp>

namespace uhal
{

	template< time_element T0, char D0 ,
			time_element T1 , char D1 ,
			time_element T2 , char D2 ,
			time_element T3 , char D3 ,
			time_element T4 , char D4 ,
			time_element T5 , char D5 ,
			time_element T6 >
	void log_inserter ( const _Time< timeval , TimeFmt<T0,D0,T1,D1,T2,D2,T3,D3,T4,D4,T5,D5,T6> >& aTime )
	{
		tm* lLocalTime ( localtime ( &aTime.value().tv_sec ) );
		TimeSpecializationHelper< T0 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D0 , log_configuration::getDestination() );
		TimeSpecializationHelper< T1 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D1 , log_configuration::getDestination() );
		TimeSpecializationHelper< T2 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D2 , log_configuration::getDestination() );
		TimeSpecializationHelper< T3 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D3 , log_configuration::getDestination() );
		TimeSpecializationHelper< T4 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D4 , log_configuration::getDestination() );
		TimeSpecializationHelper< T5 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D5 , log_configuration::getDestination() );
		TimeSpecializationHelper< T6 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
	}

	template< time_element T0, char D0 ,
			time_element T1 , char D1 ,
			time_element T2 , char D2 ,
			time_element T3 , char D3 ,
			time_element T4 , char D4 ,
			time_element T5 >
	void log_inserter ( const _Time< timeval , TimeFmt<T0,D0,T1,D1,T2,D2,T3,D3,T4,D4,T5,' ',null> >& aTime )
	{
		tm* lLocalTime ( localtime ( &aTime.value().tv_sec ) );
		TimeSpecializationHelper< T0 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D0 , log_configuration::getDestination() );
		TimeSpecializationHelper< T1 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D1 , log_configuration::getDestination() );
		TimeSpecializationHelper< T2 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D2 , log_configuration::getDestination() );
		TimeSpecializationHelper< T3 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D3 , log_configuration::getDestination() );
		TimeSpecializationHelper< T4 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D4 , log_configuration::getDestination() );
		TimeSpecializationHelper< T5 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
	}

	template< time_element T0, char D0 ,
			time_element T1 , char D1 ,
			time_element T2 , char D2 ,
			time_element T3 , char D3 ,
			time_element T4 >
	void log_inserter ( const _Time< timeval , TimeFmt<T0,D0,T1,D1,T2,D2,T3,D3,T4,' ',null,' ',null> >& aTime )
	{
		tm* lLocalTime ( localtime ( &aTime.value().tv_sec ) );
		TimeSpecializationHelper< T0 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D0 , log_configuration::getDestination() );
		TimeSpecializationHelper< T1 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D1 , log_configuration::getDestination() );
		TimeSpecializationHelper< T2 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D2 , log_configuration::getDestination() );
		TimeSpecializationHelper< T3 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D3 , log_configuration::getDestination() );
		TimeSpecializationHelper< T4 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
	}

	template< time_element T0, char D0 ,
			time_element T1 , char D1 ,
			time_element T2 , char D2 ,
			time_element T3 >
	void log_inserter ( const _Time< timeval , TimeFmt<T0,D0,T1,D1,T2,D2,T3,' ',null,' ',null,' ',null> >& aTime )
	{
		tm* lLocalTime ( localtime ( &aTime.value().tv_sec ) );
		TimeSpecializationHelper< T0 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D0 , log_configuration::getDestination() );
		TimeSpecializationHelper< T1 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D1 , log_configuration::getDestination() );
		TimeSpecializationHelper< T2 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D2 , log_configuration::getDestination() );
		TimeSpecializationHelper< T3 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
	}

	template< time_element T0, char D0 ,
			time_element T1 , char D1 ,
			time_element T2 >
	void log_inserter ( const _Time< timeval , TimeFmt<T0,D0,T1,D1,T2,' ',null,' ',null,' ',null,' ',null> >& aTime )
	{
		tm* lLocalTime ( localtime ( &aTime.value().tv_sec ) );
		TimeSpecializationHelper< T0 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D0 , log_configuration::getDestination() );
		TimeSpecializationHelper< T1 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D1 , log_configuration::getDestination() );
		TimeSpecializationHelper< T2 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
	}

	template< time_element T0, char D0 ,
			time_element T1 >
	void log_inserter ( const _Time< timeval , TimeFmt<T0,D0,T1,' ',null,' ',null,' ',null,' ',null,' ',null> >& aTime )
	{
		tm* lLocalTime ( localtime ( &aTime.value().tv_sec ) );
		TimeSpecializationHelper< T0 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
		fputc ( D0 , log_configuration::getDestination() );
		TimeSpecializationHelper< T1 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
	}

	template< time_element T0 >
	void log_inserter ( const _Time< timeval , TimeFmt<T0,' ',null,' ',null,' ',null,' ',null,' ',null,' ',null> >& aTime )
	{
		tm* lLocalTime ( localtime ( &aTime.value().tv_sec ) );
		TimeSpecializationHelper< T0 >::print ( log_configuration::getDestination() , lLocalTime , aTime.value().tv_usec );
	}




	template< typename FORMAT >
	struct TimeFactory < timeval , FORMAT >
	{
		static _Time< timeval , FORMAT > Construct ( const timeval& aTime )
		{
			return _Time< timeval , FORMAT > ( aTime );
		}
	};


	template< typename T >
	_Time< T , TimeFmt<day,'/',mth,'/',year,' ',hr,':',min,':',sec> > Time ( const T& aT )
	{
		return TimeFactory< T , TimeFmt<day,'/',mth,'/',year,' ',hr,':',min,':',sec> >::Construct ( aT );
	}


	template< typename T , typename FORMAT >
	_Time< T , FORMAT > Time ( const T& aT , const FORMAT& aFmt )
	{
		return TimeFactory< T , FORMAT >::Construct ( aT );
	}


}

