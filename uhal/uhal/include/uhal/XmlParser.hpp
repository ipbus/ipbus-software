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
#include "uhal/exception.hpp"

namespace uhal
{

	//! Exception class to handle the case where an attribute is both required and forbidden. Uses the base uhal::exception implementation of what()
	class ContradictoryParserRule: public uhal::_exception< ContradictoryParserRule > {  };
	//! Exception class to handle the case where a callback is requested without it being specified. Uses the base uhal::exception implementation of what()
	class NoActionSpecified: public uhal::_exception< NoActionSpecified > {  };
	//! Exception class to handle the case where the parser is asked to handle more than 64 attributes. Uses the base uhal::exception implementation of what()
	class TooManyAttributes: public uhal::_exception< TooManyAttributes > {  };
	//! Exception class to handle the case where an unknown attribute is parsed. Uses the base uhal::exception implementation of what()
	class UnknownAttribute: public uhal::_exception< UnknownAttribute > {  };
	//! Exception class to handle the case where two or more equally strict rules are passed. Uses the base uhal::exception implementation of what()
	class AmbiguousParserRules: public uhal::_exception< AmbiguousParserRules > {  };
	//! Exception class to handle the case where no rules were parsed. Uses the base uhal::exception implementation of what()
	class NoRulesPassed: public uhal::_exception< NoRulesPassed > {  };


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
