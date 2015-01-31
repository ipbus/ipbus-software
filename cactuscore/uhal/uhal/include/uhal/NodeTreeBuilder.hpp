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
	@author Marc Magrans De Abril
	@date 2012
*/

#ifndef _uhal_NodeTreeBuilder_hpp_
#define _uhal_NodeTreeBuilder_hpp_

#include "uhal/log/exception.hpp"
#include "uhal/definitions.hpp"
#include "uhal/Node.hpp"

#include <boost/filesystem/path.hpp>
#include <boost/spirit/include/qi.hpp>

#include "uhal/grammars/NodeTreeClassAttributeGrammar.hpp"
#include "uhal/grammars/NodeTreeParametersGrammar.hpp"
#include "uhal/grammars/NodeTreeFirmwareInfoAttributeGrammar.hpp"

#include "pugixml/pugixml.hpp"

#include "uhal/XmlParser.hpp"


namespace uhal
{
  namespace exception
  {
    //! Exception class to handle the case where creation of a node was attempted without it having a UID.
    ExceptionClass ( NodeMustHaveUID , "Exception class to handle the case where creation of a node was attempted without it having a UID." )
    //! Exception class to handle the case where too many or two few address files are specified.
    ExceptionClass ( IncorrectAddressTableFileCount , "Exception class to handle the case where too many or two few address files are specified." )
    //! Exception class to handle the case where the address file failed to open.
    ExceptionClass ( FailedToOpenAddressTableFile , "Exception class to handle the case where the address file failed to open." )
    //! Exception class to handle the case where an incremental node is specified without a size attribute.
    ExceptionClass ( IncrementalNodeRequiresSizeAttribute , "Exception class to handle the case where an incremental node is specified without a size attribute." )
    //! Exception class to handle the case where a memory block has a size which would exceed the available register space.
    ExceptionClass ( ArraySizeExceedsRegisterBound , "Exception class to handle the case where a memory block has a size which would exceed the available register space." )

#ifdef THROW_ON_ADDRESS_SPACE_OVERLAP
    //! Exception class to handle the case where two addresses overlap.
    ExceptionClass ( AddressSpaceOverlap , "Exception class to handle the case where two addresses overlap." )
#endif

    //! Exception class to handle the case when someone tries to give a block access node a child.
    ExceptionClass ( BlockAccessNodeCannotHaveChild , "Exception class to handle the case when someone tries to give a block access node a child." )

    //! Exception class to handle the case when someone tries to give a bit-masked node a child.
    ExceptionClass ( MaskedNodeCannotHaveChild , "Exception class to handle the case when someone tries to give a bit-masked node a child." )

    // //! Exception class to handle the case when a node has both masked and unmasked children. Uses the base uhal::exception implementation of what()
    // class BothMaskedAndUnmaskedChildren : public exception {};


    // //! Exception class to handle the case where a child node has an address which overlaps with the parent. Uses the base uhal::exception implementation of what()
    // class ChildHasAddressOverlap : public exception {};
    // //! Exception class to handle the case where a child node has an address mask which overlaps with the parent. Uses the base uhal::exception implementation of what()
    // class ChildHasAddressMaskOverlap : public exception {};
    //    //! Exception class to handle the case where a class is requested which does not exist in the class factory.
    //    ExceptionClass ( LabelUnknownToClassFactory , "Exception class to handle the case where a class is requested which does not exist in the class factory." )
  }


  /**
    A class to build a node tree from an Address table file
    NOTE! This is a factory method and must be Mutex protected if it is used in multithreaded environments!
  */
  class NodeTreeBuilder: private boost::noncopyable
  {

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
        Construct a node tree from file whose name is specified. NOT thread safe; for thread-safety, use ConnectionManager getDevice/getDevices methods
      	@param aFilenameExpr a Filename Expression
      	@param aPath a path that will be prepended to relative filenames for local files. Ignored for http files.
      	@return a freshly cloned node tree
      */
      Node* getNodeTree ( const std::string& aFilenameExpr , const boost::filesystem::path& aPath );

      //! Clears address filename -> Node tree cache. NOT thread safe; for tread-safety, use ConnectionManager method
      void clearAddressFileCache();


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


      void checkForAddressCollisions ( Node* aNode , const boost::filesystem::path& aPath );


      static bool NodePtrCompare ( Node* aNodeL, Node* aNodeR );


      Node* plainNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode );
      //       Node* classNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode );
      Node* moduleNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode );
      Node* bitmaskNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode );

      void setUid ( const bool& aRequireId , const pugi::xml_node& aXmlNode , Node* aNode );
      void setClassName ( const pugi::xml_node& aXmlNode , Node* aNode );
      void setPars ( const pugi::xml_node& aXmlNode , Node* aNode );
      void setAddr ( const pugi::xml_node& aXmlNode , Node* aNode );
      void setTags ( const pugi::xml_node& aXmlNode , Node* aNode );
      void setDescription ( const pugi::xml_node& aXmlNode , Node* aNode );
      void setModule ( const pugi::xml_node& aXmlNode , Node* aNode );
      void setPermissions ( const pugi::xml_node& aXmlNode , Node* aNode );
      void setMask ( const pugi::xml_node& aXmlNode , Node* aNode );
      void setModeAndSize ( const pugi::xml_node& aXmlNode , Node* aNode );
      void setFirmwareInfo ( const pugi::xml_node& aXmlNode , Node* aNode );
      void addChildren ( const pugi::xml_node& aXmlNode , Node* aNode );

      static const char* mIdAttribute;
      static const char* mAddressAttribute;
      static const char* mParametersAttribute;
      static const char* mTagsAttribute;
      static const char* mDescriptionAttribute;
      static const char* mPermissionsAttribute;
      static const char* mMaskAttribute;
      static const char* mModeAttribute;
      static const char* mSizeAttribute;
      static const char* mClassAttribute;
      static const char* mModuleAttribute;
      static const char* mFirmwareInfo;

      Parser< Node* > mTopLevelNodeParser;
      Parser< Node* > mNodeParser;

      std::deque< boost::filesystem::path > mFileCallStack;

    private:
      //! The single instance of the class
      static NodeTreeBuilder* mInstance;

      //! Hash map associating a Node tree with a file name so that we do not need to repeatedly parse the xml documents if someone asks for a second copy of a particular node tree
      boost::unordered_map< std::string , const Node* > mNodes;

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


      grammars::NodeTreeClassAttributeGrammar mNodeTreeClassAttributeGrammar;
      grammars::NodeTreeParametersGrammar mNodeTreeParametersGrammar;
      grammars::NodeTreeFirmwareinfoAttributeGrammar mNodeTreeFirmwareInfoAttributeGrammar;

  };

}

#endif
