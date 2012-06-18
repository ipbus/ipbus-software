/**
	@file
	@author Andrew W. Rose
	@author Marc Magrans De Abril
	@date 2012
*/

#ifndef _uhal_NodeTreeBuilder_hpp_
#define _uhal_NodeTreeBuilder_hpp_

#include "uhal/exception.hpp"
#include "uhal/definitions.hpp"
#include "uhal/Node.hpp"

#include <boost/utility.hpp>
#include <boost/filesystem.hpp>

#include "pugixml/pugixml.hpp"

#include <map>

namespace uhal
{
	//! Exception class to handle the case where too many or two few address files are specified. Uses the base uhal::exception implementation of what()
	class IncorrectAddressTableFileCount: public uhal::exception {  };
	//! Exception class to handle the case where the address file failed to open. Uses the base uhal::exception implementation of what()
	class FailedToOpenAddressTableFile: public uhal::exception {  };

	//! A class to build a node tree from an Address table file
	class NodeTreeBuilder: private boost::noncopyable
	{
			friend class Node;

		private:
			/**
				Default constructor
				This is private since only a single instance is to be created, using the getInstance method
			*/
			NodeTreeBuilder ();

			/**
				Destructor
			*/
			virtual ~NodeTreeBuilder ();


		public:
			/**
				Static method to retrieve the single instance of the class
				@return the single instance of the class
			*/
			static NodeTreeBuilder& getInstance();

			/**
				Method to create an associate between a node type identifier and a Creator of that particular node type
				@param aProtocol the protocol identifier
			*/
			template <class T>
			void add ( const std::string& aProtocol );


			/**
				Construct a node tree from file whose name is specified
				@param aFilenameExpr a Filename Expression
				@return a shared_ptr to a const node tree, such that which must be copied by the final user
			*/
			boost::shared_ptr< const Node > getNodeTree ( const std::string& aFilenameExpr , const uint32_t& aAddr = 0x00000000 , const uint32_t& aAddrMask = 0xFFFFFFFF );

			/**
				Method called once the file specified in the call to getNodeTree( aFilenameExpr ) has been opened
				@param aProtocol The protocol by which the file was loaded
				@param aPath The fully qualified path to the file which has been opened
				@param aFile A byte vector containing the content of the opened file. Done like this since the routine handles local and http files identically
				@param aAddressTable The address table constructed from the file
			*/
			void CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile , const uint32_t& aAddr , const uint32_t& aAddrMask , std::vector< boost::shared_ptr< const Node > >& aAddressTable );

		private:

			//! An abstract base class for defining the interface to the creators
			class CreatorInterface
			{
				public:
					/**
						Default constructor
					*/
					CreatorInterface() {}
					/**
						Destructor
					*/
					virtual ~CreatorInterface() {}
					/**
						Interface to a function which create a new IPbus client based on the protocol identifier specified
						@param aXmlNode a PugiXML node from which to construct a node
						@return a shared pointer to a node tree which must be copied before it can be used
					*/
					virtual boost::shared_ptr< const Node > create ( const pugi::xml_node& aXmlNode , const uint32_t& aParentAddr , const uint32_t& aParentMask ) = 0;
			};

			//! Templated concrete implementation with a CreatorInterface interface
			template <class T>
			class Creator: public CreatorInterface
			{
				public:

					/**
						Default constructor
					*/
					Creator() {}
					/**
						Destructor
					*/
					virtual ~Creator() {}
					/**
						Concrete function which creates a new IPbus client based on the protocol identifier specified
						@param aXmlNode a PugiXML node from which to construct a node
						@return a shared pointer to a node tree which must be copied before it can be used
					*/
					boost::shared_ptr< const Node > create ( const pugi::xml_node& aXmlNode , const uint32_t& aParentAddr = 0x00000000 , const uint32_t& aParentMask = 0xFFFFFFFF );
			};


			boost::shared_ptr< const Node > create ( const pugi::xml_node& aXmlNode , const uint32_t& aParentAddr , const uint32_t& aParentMask );


		private:
			//! The single instance of the class
			static NodeTreeBuilder* mInstance;

			//! Hash map associating a Node tree with a file name so that we do not need to recursively parse the xml documents if someone asks for a second copy of a particular node tree
			std::hash_map< std::string , boost::shared_ptr< const Node > > mNodes;

			//! Hash map associating a creator for a particular node type with a string identifier for that node type
			std::hash_map< std::string , boost::shared_ptr< CreatorInterface > > mCreators;

	};

}

#include "uhal/TemplateDefinitions/NodeTreeBuilder.hxx"

#endif
