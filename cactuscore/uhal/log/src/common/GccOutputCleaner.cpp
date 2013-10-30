
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

#include "uhal/log/GccOutputCleaner.hpp"
#include <iostream>
#include <sstream>
#include <algorithm>


std::string GccOutputCleaner::SquareBracketStyle ( const uint32_t& aIndex )
{
  std::stringstream lStr;
  lStr << "[" << aIndex << "]";
  return lStr.str();
}

std::string GccOutputCleaner::HashStyle ( const uint32_t& aIndex )
{
  std::stringstream lStr;
  lStr << "#" << aIndex;
  return lStr.str();
}

std::string GccOutputCleaner::TStyle ( const uint32_t& aIndex )
{
  std::stringstream lStr;
  lStr << "T" << aIndex;
  return lStr.str();
}

GccOutputCleaner::GccOutputCleaner ( const uint32_t& aIndent , std::string ( *aStyling ) ( const uint32_t& ) ) : mTypes() , mIndent ( aIndent , ' ' ) , mStyling ( aStyling )
{
  mTypes.reserve ( 256 );
}

GccOutputCleaner::~GccOutputCleaner() {}


std::string GccOutputCleaner::operator() ( const std::string& aStr )
{
  mTypes.clear();
  // Helpful local variables
  std::stringstream lStr;
  RecursiveClean ( aStr.begin() , aStr.end() );
  std::vector< std::string >::iterator lTypesIt ( mTypes.end() );
  lStr << * ( --lTypesIt );

  while ( --lTypesIt >= mTypes.begin() )
  {
    lStr << "\n" << mIndent << ( *mStyling ) ( lTypesIt - mTypes.begin() ) << " = " << *lTypesIt;
  }

  return lStr.str();
}


uint32_t GccOutputCleaner::RecursiveClean ( std::string::const_iterator aIt , std::string::const_iterator aEnd )
{
  // Strip leading and trailing spaces
  while ( * ( aEnd-1 ) == ' ' )
  {
    aEnd--;
  }

  while ( *aIt == ' ' )
  {
    aIt++;
  }

  // Helpful local variables
  std::stringstream lStr;
  std::string::const_iterator lStart;
  uint32_t lCounter ( 0 );

  // Run the iteration
  for ( ; aIt != aEnd ; ++aIt )
  {
    switch ( *aIt )
    {
      case '(' :      // We are starting a function argument list
      case '<' :      // We are starting a new template child

        if ( lCounter++ == 0 )
        {
          lStart = aIt+1;
          lStr << *aIt;
        }

        break;
      case ')' :      // We are finishing function argument list
      case '>' :      // We are finishing template children

        if ( --lCounter == 0 )
        {
          if ( aIt - lStart > 1 )
          {
            lStr << " " << ( *mStyling ) ( RecursiveClean ( lStart, aIt ) ) << " ";
          }

          lStr << *aIt;
        }

        break;
      case ',' :

        if ( lCounter == 1 )     // We have finished one template child or function argument and are about to start another
        {
          if ( aIt - lStart > 1 )
          {
            lStr << " " << ( *mStyling ) ( RecursiveClean ( lStart, aIt ) ) << " ";
          }

          lStr << ",";
          lStart = aIt+2;
        }

        break;
      case '*' :
      case '&' :

        if ( lCounter == 1 )     // We have finished one template child and about to start another
        {
          lStr << " " << ( *mStyling ) ( RecursiveClean ( lStart, aIt ) ) << *aIt << " ";
          lStart = aIt;
        }

        break;
      default:  // We are a regular character

        if ( lCounter == 0 )
        {
          if ( ( *aIt != ' ' ) || ( aIt+1 == aEnd ) || ( * ( aIt+1 ) != ' ' ) ) // Reduce multiple spaces down to 1
          {
            lStr << *aIt;
          }
        }

        break;
    }
  }

  // If the string we have constructed is not in the list, add it, otherwise return the index into the vector.
  std::string lTemp ( lStr.str() );
  std::vector< std::string >::iterator lTypesIt = std::find ( mTypes.begin() , mTypes.end() , lTemp );

  if ( lTypesIt != mTypes.end() )
  {
    return lTypesIt - mTypes.begin();
  }
  else
  {
    mTypes.push_back ( lTemp );
    return mTypes.size() -1;
  }
}

