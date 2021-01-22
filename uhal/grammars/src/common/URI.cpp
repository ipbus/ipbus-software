/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.


      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

---------------------------------------------------------------------------
*/

#include "uhal/grammars/URI.hpp"


#include <iostream>
#include <sstream>


namespace uhal {

  std::ostream& operator<< ( std::ostream& aStr , const uhal::URI& aURI )
  {
    aStr << " > protocol : " << aURI.mProtocol << "\n";
    aStr << " > hostname : " << aURI.mHostname << "\n";
    aStr << " > port : " << aURI.mPort << "\n";
    aStr << " > path : " << aURI.mPath << "\n";
    aStr << " > extension : " << aURI.mExtension << "\n";
    aStr << " > arguments :\n";

    for ( const auto& lArg: aURI.mArguments )
    {
      aStr << "   > " << lArg.first << " = " << lArg.second << "\n";
    }

    aStr << std::flush;
    return aStr;
  }


  std::string toString( const uhal::URI& aURI )
  {
    std::stringstream lReturn;
    // url is always of the form "protocol://hostname:port"
    lReturn << aURI.mProtocol << "://" << aURI.mHostname;
    if ( !aURI.mPort.empty() )
      lReturn << ":" << aURI.mPort;

    // there is sometimes a path
    if ( aURI.mPath != "" )
    {
      lReturn << "/" << aURI.mPath;
    }

    // there is sometimes a filename extension
    if ( aURI.mExtension != "" )
    {
      lReturn << "." << aURI.mExtension;
    }

    // there are sometimes arguments
    if ( aURI.mArguments.size() )
    {
      lReturn << "?";
      uhal::NameValuePairVectorType::const_iterator lIt = aURI.mArguments.begin();

      while ( true )
      {
        lReturn << lIt->first << "=" << lIt->second;

        if ( ++lIt == aURI.mArguments.end() )
        {
          break;
        }

        lReturn << "&";
      }
    }

    return lReturn.str();    
  }


}
