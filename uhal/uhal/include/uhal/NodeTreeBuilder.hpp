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
#include <boost/spirit/include/qi.hpp>
#include <boost/regex.hpp>
#include <boost/shared_ptr.hpp>


#include "pugixml/pugixml.hpp"

#include "uhal/XmlParser.hpp"

#include <map>

// /**
// EXPERIMENTAL! Method for adding methods to the factory
// @param class the class to add to the factory
// @warning EXPERIMENTAL!
// */
// #define REGISTER_NODE_TYPE( class ) NodeTreeBuilder::RegistrationHelper< class >( #class );

namespace uhal
{
	//! Exception class to handle the case where too many or two few address files are specified. Uses the base uhal::exception implementation of what()
	class IncorrectAddressTableFileCount: public uhal::_exception< IncorrectAddressTableFileCount > {  };
	//! Exception class to handle the case where the address file failed to open. Uses the base uhal::exception implementation of what()
	class FailedToOpenAddressTableFile: public uhal::_exception< FailedToOpenAddressTableFile > {  };
	//! Exception class to handle the case where an incremental node is specified without a size attribute. Uses the base uhal::exception implementation of what()
	class IncrementalNodeRequiresSizeAttribute: public uhal::_exception< IncrementalNodeRequiresSizeAttribute > {  };
	//! Exception class to handle the case where a memory block has a size which would exceed the available register space. Uses the base uhal::exception implementation of what()
	class ArraySizeExceedsRegisterBound: public uhal::_exception< ArraySizeExceedsRegisterBound > {  };
	//! Exception class to handle the case where two addresses overlap. Uses the base uhal::exception implementation of what()
	class AddressSpaceOverlap: public uhal::_exception< AddressSpaceOverlap > {  };
	//! Exception class to handle the case when someone tries to give a block access node a child. Uses the base uhal::exception implementation of what()
	class BlockAccessNodeCannotHaveChild: public uhal::_exception< AddressSpaceOverlap > {  };


	//! A class to build a node tree from an Address table file
	class NodeTreeBuilder: private boost::noncopyable
	{
		public:
			// //! Give the node access to the private factory
			// friend class Node;

			// /**
			// EXPERIMENTAL! A helper class whose constructor registers a class with the factory
			// @warning EXPERIMENTAL!
			// */
			// template< typename T >
			// struct RegistrationHelper
			// {
			// /**
			// EXPERIMENTAL! Constructor
			// @param aNodeTypeIdentifier the string used as the identifier by the factory
			// @warning EXPERIMENTAL!
			// */
			// RegistrationHelper ( const std::string& aNodeTypeIdentifier );
			// };

			// //! EXPERIMENTAL! Give the RegistrationHelper access to the private factory
			// template< typename T > friend class RegistrationHelper;

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
				@param aCalculateHierarchicalAddresses flag to indicate whether address tree should be calculated. For retrieving top-level trees, then this should be true, for sub-trees false.
				@return a freshly cloned node tree
			*/
			Node* getNodeTree ( const std::string& aFilenameExpr , const boost::filesystem::path& aPath , const bool& aCalculateHierarchicalAddresses = true );

		private:
			/**
				Method called once the file specified in the call to getNodeTree( aFilenameExpr ) has been opened
				@param aProtocol The protocol by which the file was loaded
				@param aPath The fully qualified path to the file which has been opened
				@param aFile A byte vector containing the content of the opened file. Done like this since the routine handles local and http files identically
				@param aAddressTable The address table constructed from the file
			*/
			void CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile , std::vector< const Node* >& aAddressTable );


			/**
				Propagate the addresses down through the hierarchical structure
				@param aNode the node whose address we are calculating
				@param aAddr the parent address which will be applied to the children
			*/
			void calculateHierarchicalAddresses ( Node* aNode , const uint32_t& aAddr );


			Node* plainNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode );
			Node* classNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode );
			Node* moduleNodeCreator ( const pugi::xml_node& aXmlNode );
			Node* bitmaskNodeCreator ( const pugi::xml_node& aXmlNode );

			void setUid ( const bool& aRequireId , const pugi::xml_node& aXmlNode , Node* aNode );
			void setAddr ( const pugi::xml_node& aXmlNode , Node* aNode );
			void setTags ( const pugi::xml_node& aXmlNode , Node* aNode );
			void setPermissions ( const pugi::xml_node& aXmlNode , Node* aNode );
			void setMask ( const pugi::xml_node& aXmlNode , Node* aNode );
			void setModeAndSize ( const pugi::xml_node& aXmlNode , Node* aNode );
			void addChildren ( const pugi::xml_node& aXmlNode , Node* aNode );

			static const char* mIdAttribute;
			static const char* mAddressAttribute;
			static const char* mTagsAttribute;
			static const char* mPermissionsAttribute;
			static const char* mMaskAttribute;
			static const char* mModeAttribute;
			static const char* mSizeAttribute;
			static const char* mClassAttribute;
			static const char* mModuleAttribute;

			Parser< Node* > mTopLevelNodeParser;
			Parser< Node* > mNodeParser;


			std::deque< boost::filesystem::path > mFileCallStack;

			// private:

			// /**
			// Method to create an associate between a node type identifier and a Creator of that particular node type
			// @param aNodeTypeIdentifier the node type identifier
			// */
			// template <class T>
			// void add ( const std::string& aNodeTypeIdentifier );

			// //! An abstract base class for defining the interface to the creators
			// class CreatorInterface
			// {
			// public:
			// /**
			// Default constructor
			// */
			// CreatorInterface() {}
			// /**
			// Destructor
			// */
			// virtual ~CreatorInterface() {}
			// /**
			// Interface to a function which create a new IPbus client based on the protocol identifier specified
			// @param aXmlNode a PugiXML node from which to construct a node
			// @param aPath The fully qualified path to the XML file containing this node
			// @param aRequireId specify whether an exception should be thrown if the id attribute is not set
			// @return a shared pointer to a node tree which must be copied before it can be used
			// */
			// virtual const Node* create ( const pugi::xml_node& aXmlNode , const boost::filesystem::path& aPath , const bool& aRequireId = true ) = 0;
			// };

			// //! Templated concrete implementation with a CreatorInterface interface
			// template <class T>
			// class Creator: public CreatorInterface
			// {
			// public:

			// /**
			// Default constructor
			// */
			// Creator() {}
			// /**
			// Destructor
			// */
			// virtual ~Creator() {}
			// /**
			// Concrete function which creates a new IPbus client based on the protocol identifier specified
			// @param aXmlNode a PugiXML node from which to construct a node
			// @param aPath The fully qualified path to the XML file containing this node
			// @param aRequireId specify whether an exception should be thrown if the id attribute is not set
			// @return a node tree which must be copied before it can be used
			// */
			// const Node* create ( const pugi::xml_node& aXmlNode , const boost::filesystem::path& aPath , const bool& aRequireId = true );
			// };


			// /**
			// Helper function which reads the class type from the XML node and calls the appropriate creator
			// @param aXmlNode a PugiXML node from which to construct a node
			// @param aPath The fully qualified path to the XML file containing this node
			// @param aRequireId specify whether an exception should be thrown if the id attribute is not set
			// @return a node tree which must be copied before it can be used
			// */
			// const Node* create ( const pugi::xml_node& aXmlNode , const boost::filesystem::path& aPath , const bool& aRequireId = true );


		private:
			//! The single instance of the class
			static NodeTreeBuilder* mInstance;

			//! Hash map associating a Node tree with a file name so that we do not need to repeatedly parse the xml documents if someone asks for a second copy of a particular node tree
			std::hash_map< std::string , const Node* > mNodes;

			// //! Hash map associating a creator for a particular node type with a string identifier for that node type
			// std::hash_map< std::string , boost::shared_ptr< CreatorInterface > > mCreators;

			//! A look-up table that the boost qi parser uses for associating strings ("r","w","rw","wr","read","write","readwrite","writeread") with enumerated permissions types
			static const struct permissions_lut : boost::spirit::qi::symbols<char, defs::NodePermission>
			{
				//! The actual function that the boost qi parser uses for associating strings with enumerated permissions types
				permissions_lut();
			} mPermissionsLut; //!< An instance of a look-up table that the boost qi parser uses for associating strings with enumerated permissions types


			//! A look-up table that the boost qi parser uses for associating strings ("single","block","port","incremental","non-incremental","inc","non-inc") with enumerated mode types
			static const struct mode_lut : boost::spirit::qi::symbols<char, defs::BlockReadWriteMode>
			{
				//! The actual function that the boost qi parser uses for associating strings with enumerated permissions types
				mode_lut();
			} mModeLut; //!< An instance of a look-up table that the boost qi parser uses for associating strings with enumerated permissions types


	};

}

#include "uhal/TemplateDefinitions/NodeTreeBuilder.hxx"

#endif
