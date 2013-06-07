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

/**
  @file
  @author Andrew W. Rose
  @date 2013
*/

#ifndef _uhal_log_GccOutputCleaner_hpp_
#define _uhal_log_GccOutputCleaner_hpp_

#include <string>
#include <vector>
#include <stdint.h>


class GccOutputCleaner
{

  public:
    GccOutputCleaner ( const uint32_t& aIndent = 2 , std::string ( *aStyling ) ( const uint32_t& ) = &GccOutputCleaner::SquareBracketStyle );

    virtual ~GccOutputCleaner();


    std::string operator() ( const std::string& aStr );

    static std::string SquareBracketStyle ( const uint32_t& aIndex );
    static std::string HashStyle ( const uint32_t& aIndex );
    static std::string TStyle ( const uint32_t& aIndex );

  private:
    std::vector< std::string > mTypes;
    std::string mIndent;
    std::string ( *mStyling ) ( const uint32_t& );

    uint32_t RecursiveClean ( std::string::const_iterator aIt , std::string::const_iterator aEnd );

};


#endif
