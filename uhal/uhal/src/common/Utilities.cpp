#include "uhal/Utilities.hpp"

#include "uhal/log.hpp"

// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	namespace utilities
	{
		void PugiXMLParseResultPrettifier( const pugi::xml_parse_result& aLoadResult , const boost::filesystem::path& aPath , const std::vector<uint8_t>& aFile )
		{
		
			pantheios::log_ERROR ( "Failed to parse file \"" , aPath.c_str() , "\". PugiXML returned the following description \"" , aLoadResult.description() , "\"." );
				
			std::size_t lLineCounter(1);	
			std::vector<uint8_t>::const_iterator lIt0 ( aFile.begin() );
			std::vector<uint8_t>::const_iterator lIt1 ( aFile.begin() + aLoadResult.offset );
			std::vector<uint8_t>::const_iterator lIt2 (lIt1);

			
			for(  ; lIt0!=lIt1 ; ++lIt0 ){
				if( *lIt0 == '\n' )  lLineCounter++;
			}
			
			for( ; lIt1 != aFile.begin() ; --lIt1 ){
				if ( *lIt1 == '\n' ){
					++lIt1;
					break;
				}
			}

			for( ; lIt2 != aFile.end() ; ++lIt2 ){
				if ( *lIt2 == '\n' ){
					--lIt2;
					break;
				}
			}				

			std::size_t lDist0( lIt0 - lIt1 );
			std::size_t lDist1( lIt2 - lIt0 );

			pantheios::log_ERROR ( "Error occured at line number " , pantheios::integer(lLineCounter) , ", character " , pantheios::integer(lDist0+1) );		
			
			std::string lLine;
			lLine.reserve( lIt2 - lIt1 );
			for (  ; lIt1 != lIt2 ; ++lIt1 ){
				if( isprint( *lIt1 ) || *lIt1==10 ) lLine += *lIt1;
				else lLine += '#';
			}
			pantheios::log_ERROR ( "LINE           : " , lLine , "\n"
									"ERROR LOCATION : " , std::string( lDist0 , '_') , "^" , std::string( lDist1 , '_') );		
	
		
		
		}
		
	}
}

