
#include <log/log_inserters.time.hpp>
#include <log/log_inserters.integer.hpp>

#include <log/log_configuration.hpp>


namespace uhal
{

	template<>
	void TimeSpecializationHelper< year >::print ( FILE* aFile , const tm* aTm , const uint32_t& aUsec )
	{
		log_inserter ( Integer ( aTm->tm_year+1900 ) );
	}

	template<>
	void TimeSpecializationHelper< yr >::print ( FILE* aFile , const tm* aTm , const uint32_t& aUsec )
	{
		static const char* lCharacterMapping (	"00010203040506070809"
												"10111213141516171819"
												"20212223242526272829"
												"30313233343536373839"
												"40414243444546474849"
												"50515253545556575859"
												"60616263646566676869"
												"70717273747576777879"
												"80818283848586878889"
												"90919293949596979899" );
		fwrite ( lCharacterMapping + ( ( aTm->tm_year%100 ) <<1 ) , 1 , 2 , aFile );
	}

	template<>
	void TimeSpecializationHelper< strmth >::print ( FILE* aFile , const tm* aTm , const uint32_t& aUsec )
	{
		static const char* lCharacterMapping ( "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec" );
		fwrite ( lCharacterMapping + ( aTm->tm_mon<<2 ) , 1 , 3 , aFile );
	}

	template<>
	void TimeSpecializationHelper< mth >::print ( FILE* aFile , const tm* aTm , const uint32_t& aUsec )
	{
		static const char* lCharacterMapping ( "010203040506070809101112" );
		fwrite ( lCharacterMapping + ( aTm->tm_mon<<1 ) , 1 , 2 , aFile );
	}

	template<>
	void TimeSpecializationHelper< day >::print ( FILE* aFile , const tm* aTm , const uint32_t& aUsec )
	{
		static const char* lCharacterMapping (	"xx010203040506070809"
												"10111213141516171819"
												"20212223242526272829"
												"3031" );
		fwrite ( lCharacterMapping + ( aTm->tm_mday<<1 ) , 1 , 2 , aFile );
	}

	template<>
	void TimeSpecializationHelper< hr >::print ( FILE* aFile , const tm* aTm , const uint32_t& aUsec )
	{
		static const char* lCharacterMapping (	"00010203040506070809"
												"10111213141516171819"
												"20212223" );
		fwrite ( lCharacterMapping + ( aTm->tm_hour<<1 ) , 1 , 2 , aFile );
	}

	template<>
	void TimeSpecializationHelper< min >::print ( FILE* aFile , const tm* aTm , const uint32_t& aUsec )
	{
		static const char* lCharacterMapping (	"00010203040506070809"
												"10111213141516171819"
												"20212223242526272829"
												"30313233343536373839"
												"40414243444546474849"
												"50515253545556575859" );
		fwrite ( lCharacterMapping + ( aTm->tm_min<<1 ) , 1 , 2 , aFile );
	}

	template<>
	void TimeSpecializationHelper< sec >::print ( FILE* aFile , const tm* aTm , const uint32_t& aUsec )
	{
		static const char* lCharacterMapping (	"00010203040506070809"
												"10111213141516171819"
												"20212223242526272829"
												"30313233343536373839"
												"40414243444546474849"
												"50515253545556575859"
												"6061" );
		fwrite ( lCharacterMapping + ( aTm->tm_sec<<1 ) , 1 , 2 , aFile );
	}

	template<>
	void TimeSpecializationHelper< usec >::print ( FILE* aFile , const tm* aTm , const uint32_t& aUsec )
	{
		log_inserter ( Integer ( aUsec ) );
	}

	void log_inserter ( const  Time<usec,' ',null,' ',null,' ',null,' ',null,' ',null,' ',null>& aTime )
	{
		tm* lLocalTime ( NULL );
		TimeSpecializationHelper< usec >::print ( log_configuration::getDestination() , lLocalTime , ( aTime.value() ).tv_usec );
	}

}
