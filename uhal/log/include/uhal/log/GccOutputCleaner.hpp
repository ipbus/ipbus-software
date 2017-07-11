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

/**
	Parameterized Functor which parses and formats GCC call stack so that they are human readable.
	It does this by substituting every template parameter and argument with an integer and then printing this across multiple lines
*/
class GccOutputCleaner
{

  public:
    /**
    	Constructor
    	@param aIndent the number of spaces ahead of the formatted input
    	@param aStyling functor which formats numbers to strings and which is used to format type substitutions
    */
    GccOutputCleaner ( const uint32_t& aIndent = 2 , std::string ( *aStyling ) ( const uint32_t& ) = &GccOutputCleaner::SquareBracketStyle );

    /**
    	Destructor
    */
    virtual ~GccOutputCleaner();

    /**
    	Functor implementation
    	@param aStr A string containing a GCC call stack to format
    	@return A nicely formatted string of the GCC call stack
    */
    std::string operator() ( const std::string& aStr );

    /**
    	Wrap the type-substitution index in square braces
    	@param aIndex a type-substitution index to wrap
    	@return a formatted string
    */
    static std::string SquareBracketStyle ( const uint32_t& aIndex );
    /**
    	Prefix the type-substitution index with a `#`
    	@param aIndex a type-substitution index to prefix
    	@return a formatted string
    */
    static std::string HashStyle ( const uint32_t& aIndex );
    /**
    	Prefix the type-substitution index with a `T`
    	@param aIndex a type-substitution index to prefix
    	@return a formatted string
    */
    static std::string TStyle ( const uint32_t& aIndex );

  private:
    /// A vector of known types, the position of which into this list becomes the substitution index
    std::vector< std::string > mTypes;
    /// The number of spaces by which to indent the lines
    std::string mIndent;
    /// The functor for styling the type-substitution index
    std::string ( *mStyling ) ( const uint32_t& );

    /**
    	Recursively perform sub-term extraction from the string
    	@param aIt the starting point for the current extraction
    	@param aEnd the end-point for the current extraction
    	@return the index into the vector which contains the type corresponding to the string between aIt and aEnd (which is added by the function, if it doesn't already exist)
    */
    uint32_t RecursiveClean ( std::string::const_iterator aIt , std::string::const_iterator aEnd );

};


#endif
