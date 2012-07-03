
#include "uhal/uhal.hpp"
#include "uhal/log/log.hpp"

#include <vector>
#include <string>


int main ( int argc , char** argv )
{
	using namespace uhal;

	try
	{
		ConnectionManager lConnectionManager ( "     file://~/uhal/tutorials/tutorial_2/addr*/*connections.xml   ; file://~/connection*.xml  ;  ;;; ; ;  " );
		ConnectionManager lConnectionManager2 ( " http://svnweb.cern.ch/world/wsvn/cactus/trunk/uhal/tutorials/tutorial_2/addr/connections.xml?op=dl&rev=head      " );
		std::vector<std::string> lDeviceIds = lConnectionManager.getDevices ();

		for ( std::vector<std::string>::const_iterator lIt = lDeviceIds.begin() ; lIt != lDeviceIds.end(); ++lIt )
		{
			log ( Info() , "All Device IDs included string " , Quote ( *lIt ) );
		}

		std::vector<std::string> lRegexDeviceIds = lConnectionManager.getDevices ( "sys1.crate1.*" );

		for ( std::vector<std::string>::const_iterator lIt = lRegexDeviceIds.begin() ; lIt != lRegexDeviceIds.end(); ++lIt )
		{
			log ( Info() , "Regex returned Device ID string " , Quote ( *lIt ) );
		}

		HwInterface lHwInterface = lConnectionManager.getDevice ( "sys1.crate1.slot1" );
		lHwInterface.ping();
	}
	catch ( const std::exception& aExc )
	{
		log ( Error() , "Exception " , Quote ( aExc.what() ) , " caught at " , ThisLocation() );
	}
}

