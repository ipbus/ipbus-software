#ifndef _uhal_UtilityTypes_hpp_
#define _uhal_UtilityTypes_hpp_

#include <vector>
#include <string>
#include <iostream>

#include <boost/fusion/include/adapt_struct.hpp>


// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
namespace uhal
{
	namespace utilities
	{
	
		//! Struct to store an http response received from a server when parsed by boost spirit 
		struct HttpResponseType
		{
			//! the http transport method
			std::string method;
			//! the http version number
			double version;
			//! the response status
			int status;
			//! the response status string
			std::string status_string;
			//! parsed headers
			std::string headers;
			//! parsed message content
			std::vector< uint8_t > content;
		};	
		

		std::ostream& operator<< ( std::ostream& aStream , const HttpResponseType& aHttpResponse )
		{
			aStream << std::endl;
			aStream << " > method : " << aHttpResponse.method << "\n";
			aStream << " > version : " << aHttpResponse.version << "\n";
			aStream << " > status : " << aHttpResponse.status << "\n";
			aStream << " > status_string : " << aHttpResponse.status_string << "\n";
			aStream << " > NameValuePairs :\n" << aHttpResponse.headers << "\n";
			aStream << " > Content :\n";
			for ( std::vector<uint8_t>::const_iterator lIt = aHttpResponse.content.begin() ; lIt != aHttpResponse.content.end() ; ++lIt ){
				aStream << char(*lIt);
			}
			aStream << std::endl;
			return aStream;
		}
		
	}
}

// Call to BOOST_FUSION_ADAPT_STRUCT must be at global scope
BOOST_FUSION_ADAPT_STRUCT(
	uhal::utilities::HttpResponseType,
	(std::string, method)
	(double , version)
	(int, status)
	(std::string, status_string)
	(std::string, headers)
	(std::vector< uint8_t >, content)
);	
// ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
		
	
#endif
