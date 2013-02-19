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
	@date 2012
*/

#ifndef _uhal_XmlParser_hpp_
#define _uhal_XmlParser_hpp_

#include <string>
#include <deque>
#include <set>

#include "pugixml/pugixml.hpp"
#include "uhal/log/log.hpp"
#include "uhal/Utilities.hpp"
#include "uhal/log/exception.hpp"

namespace uhal
{

  namespace exception
  {
    //! Exception class to handle the case where an attribute is both required and forbidden.
    ExceptionClass ( ContradictoryParserRule , "Exception class to handle the case where an attribute is both required and forbidden." );
    //! Exception class to handle the case where a callback is requested without it being specified.
    ExceptionClass ( NoActionSpecified , "Exception class to handle the case where a callback is requested without it being specified." );
    //! Exception class to handle the case where the parser is asked to handle more than 64 attributes.
    ExceptionClass ( TooManyAttributes , "Exception class to handle the case where the parser is asked to handle more than 64 attributes." );
    //! Exception class to handle the case where an unknown attribute is parsed.
    ExceptionClass ( UnknownAttribute , "Exception class to handle the case where an unknown attribute is parsed." );
    //! Exception class to handle the case where two or more equally strict rules are passed.
    ExceptionClass ( AmbiguousParserRules , "Exception class to handle the case where two or more equally strict rules are passed." );
    //! Exception class to handle the case where no rules were parsed.
    ExceptionClass ( NoRulesPassed , "Exception class to handle the case where no rules were parsed." );
  }

  template < typename R >
  class Parser;

  template < typename R >
  class BaseFunctionObject
  {
    public:
      BaseFunctionObject();
      virtual ~BaseFunctionObject();
      virtual R operator() ( const pugi::xml_node& aNode ) = 0;
  };

  template < typename R , typename T >
  class FunctionObject : public BaseFunctionObject<R>
  {
    public:
      FunctionObject ( T& aT );

      R operator() ( const pugi::xml_node& aNode );

    private:
      T mT;
  };

  template < typename R , typename T >
  class FunctionObject<R,T*> : public BaseFunctionObject<R>
  {
    public:
      FunctionObject ( T* aT );

      R operator() ( const pugi::xml_node& aNode );

    private:
      T* mT;
  };





  template < typename R >
  class Rule
  {
      friend class Parser< R >;
    public:
      Rule( );

      virtual ~Rule();

      Rule<R>& require ( const std::string& aStr );

      Rule<R>& forbid ( const std::string& aStr );

      Rule<R>& optional ( const std::string& aStr );

      std::string description() const;

    private:

      R operator() ( const pugi::xml_node& aNode );


    private:
      std::set<std::string> mRequired;
      std::set<std::string> mForbidden;
      std::set<std::string> mOptional;

      uint32_t mRuleId;
      uint64_t mRequiredHash;
      uint64_t mForbiddenHash;

      BaseFunctionObject<R>* mFuncPtr;
  };


  template < typename R >
  class Parser
  {
    public:
      Parser();

      ~Parser();

      template < typename T >
      void addRule ( const Rule<R> & aRule , T aCallbackHandler );

      R operator() ( const pugi::xml_node& aNode );


    private:
      uint64_t mNextHash;
      std::hash_map< std::string , uint64_t > mHashes;
      std::deque< Rule<R> > mRules;
      uint32_t mRuleCounter;

  };

}

#include "uhal/TemplateDefinitions/XmlParser.hxx"

#endif
