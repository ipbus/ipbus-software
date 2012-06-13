
#include <log/log_inserters.boolean.hpp>

#include <log/log_configuration.hpp> 

namespace uhal{

	template<>
	void log_inserter< _Boolean< alpha > >( const _Boolean< alpha >& aBoolean ){
		if( aBoolean.value() ){
			fputs ( "True" , log_configuration::getDestination() );
		}else{
			fputs ( "False" , log_configuration::getDestination() );
		}
	}


	template<>
	void log_inserter< _Boolean< numeric > >( const _Boolean< numeric >& aBoolean ){
		if( aBoolean.value() ){
			fputc ( '1' , log_configuration::getDestination() );
		}else{
			fputc ( '0' , log_configuration::getDestination() );
		}
	}


	_Boolean< DefaultBooleanFormat > Boolean( const bool& aBool ){
		return _Boolean< DefaultBooleanFormat >( aBool );
	}


}
