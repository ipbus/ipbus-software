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


std::ostream& operator<< ( std::ostream& aStr , const uhal::URI& aURI )
{
  aStr << " > protocol : " << aURI.mProtocol << "\n";
  aStr << " > hostname : " << aURI.mHostname << "\n";
  aStr << " > port : " << aURI.mPort << "\n";
  aStr << " > path : " << aURI.mPath << "\n";
  aStr << " > extension : " << aURI.mExtension << "\n";
  aStr << " > arguments :\n";

  for ( uhal::NameValuePairVectorType::const_iterator lIt = aURI.mArguments.begin() ; lIt != aURI.mArguments.end() ; ++lIt )
  {
    aStr << "   > " << lIt->first << " = " << lIt->second << "\n";
  }

  aStr << std::flush;
  return aStr;
}

