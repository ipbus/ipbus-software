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

/**
	EXPERIMENTAL! Method for adding methods to the factory
	@param class the class to add to the factory
	@warning EXPERIMENTAL!
*/
#define REGISTER_NODE_TYPE( class ) NodeTreeBuilder::RegistrationHelper< class >( #class );

namespace uhal
{
	//! Exception class to handle the case where too many or two few address files are specified. Uses the base uhal::exception implementation of what()
	class IncorrectAddressTableFileCount: public uhal::exception {  };
	//! Exception class to handle the case where the address file failed to open. Uses the base uhal::exception implementation of what()
	class FailedToOpenAddressTableFile: public uhal::exception {  };


	//! A class to build a node tree from an Address table file
	class NodeTreeBuilder: private boost::noncopyable
	{
		public:
			//! Give the node access to the private factory
			friend class Node;

			/**
				EXPERIMENTAL! A helper class whose constructor registers a class with the factory
				@warning EXPERIMENTAL!
			*/
			template< typename T >
			struct RegistrationHelper
			{
				/**
					EXPERIMENTAL! Constructor
					@param aNodeTypeIdentifier the string used as the identifier by the factory
					@warning EXPERIMENTAL!
				*/
				RegistrationHelper ( const std::string& aNodeTypeIdentifier );
			};

			//! EXPERIMENTAL! Give the RegistrationHelper access to the private factory
			template< typename T > friend class RegistrationHelper;

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
				Construct a node tree from file whose name is specified
				@param aFilenameExpr a Filename Expression
				@param aPath a path that will be prepended to relative filenames for local files. Ignored for http files.
				@param aAddr the address of the parent node for hierarchical addressing and address collision detection
				@param aAddrMask the address-mask of the parent node for hierarchical addressing and address collision detection
				@return a node tree which must be copied before it can be used
			*/
			boost::shared_ptr< const Node > getNodeTree ( const std::string& aFilenameExpr , const boost::filesystem::path& aPath , const uint32_t& aAddr = 0x00000000 , const uint32_t& aAddrMask = 0xFFFFFFFF );

			/**
				Method called once the file specified in the call to getNodeTree( aFilenameExpr ) has been opened
				@param aProtocol The protocol by which the file was loaded
				@param aPath The fully qualified path to the file which has been opened
				@param aFile A byte vector containing the content of the opened file. Done like this since the routine handles local and http files identically
				@param aAddr the address of the parent node for hierarchical addressing and address collision detection
				@param aAddrMask the address-mask of the parent node for hierarchical addressing and address collision detection
				@param aAddressTable The address table constructed from the file
			*/
			void CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile , const uint32_t& aAddr , const uint32_t& aAddrMask , std::vector< boost::shared_ptr< const Node > >& aAddressTable );

		private:

			/**
				Method to create an associate between a node type identifier and a Creator of that particular node type
				@param aNodeTypeIdentifier the node type identifier
			*/
			template <class T>
			void add ( const std::string& aNodeTypeIdentifier );

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
						@param aPath The fully qualified path to the XML file containing this node
						@param aParentAddr the address of the parent node for hierarchical addressing and address collision detection
						@param aParentMask the address-mask of the parent node for hierarchical addressing and address collision detection
						@return a shared pointer to a node tree which must be copied before it can be used
					*/
					virtual boost::shared_ptr< const Node > create ( const pugi::xml_node& aXmlNode , const boost::filesystem::path& aPath , const uint32_t& aParentAddr , const uint32_t& aParentMask ) = 0;
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
						@param aPath The fully qualified path to the XML file containing this node
						@param aParentAddr the address of the parent node for hierarchical addressing and address collision detection
						@param aParentMask the address-mask of the parent node for hierarchical addressing and address collision detection
						@return a node tree which must be copied before it can be used
					*/
					boost::shared_ptr< const Node > create ( const pugi::xml_node& aXmlNode , const boost::filesystem::path& aPath , const uint32_t& aParentAddr = 0x00000000 , const uint32_t& aParentMask = 0xFFFFFFFF );
			};


			/**
				Helper function which reads the class type from the XML node and calls the appropriate creator
				@param aXmlNode a PugiXML node from which to construct a node
				@param aPath The fully qualified path to the XML file containing this node
				@param aParentAddr the address of the parent node for hierarchical addressing and address collision detection
				@param aParentMask the address-mask of the parent node for hierarchical addressing and address collision detection
				@return a node tree which must be copied before it can be used
			*/
			boost::shared_ptr< const Node > create ( const pugi::xml_node& aXmlNode , const boost::filesystem::path& aPath , const uint32_t& aParentAddr , const uint32_t& aParentMask );


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
