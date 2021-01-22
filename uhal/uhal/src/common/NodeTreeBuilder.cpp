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

#include "uhal/NodeTreeBuilder.hpp"


#include <chrono>
#include <functional>

#include <boost/spirit/include/qi.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/filesystem.hpp>

#include "uhal/detail/utilities.hpp"
#include "uhal/DerivedNodeFactory.hpp"
#include "uhal/log/log.hpp"
#include "uhal/utilities/files.hpp"
#include "uhal/utilities/xml.hpp"


// Resolve std bind placeholders (_1, _2, ...)
namespace arg = std::placeholders;


namespace uhal
{

  const std::string NodeTreeBuilder::mIdAttribute = "id";
  const std::string NodeTreeBuilder::mAddressAttribute = "address";
  const std::string NodeTreeBuilder::mParametersAttribute = "parameters";
  const std::string NodeTreeBuilder::mTagsAttribute = "tags";
  const std::string NodeTreeBuilder::mDescriptionAttribute = "description";
  const std::string NodeTreeBuilder::mPermissionsAttribute = "permission";
  const std::string NodeTreeBuilder::mMaskAttribute = "mask";
  const std::string NodeTreeBuilder::mModeAttribute = "mode";
  const std::string NodeTreeBuilder::mSizeAttribute = "size";
  const std::string NodeTreeBuilder::mClassAttribute = "class";
  const std::string NodeTreeBuilder::mModuleAttribute = "module";
  const std::string NodeTreeBuilder::mFirmwareInfo = "fwinfo";


  std::shared_ptr<NodeTreeBuilder> NodeTreeBuilder::mInstance;


  NodeTreeBuilder::NodeTreeBuilder ()
  {
    //------------------------------------------------------------------------------------------------------------------------
    Rule<Node*> lPlainNode;
    lPlainNode.optional ( NodeTreeBuilder::mClassAttribute ) //.forbid ( NodeTreeBuilder::mClassAttribute ) //see https://svnweb.cern.ch/trac/cactus/ticket/452
    .forbid ( NodeTreeBuilder::mModuleAttribute )
    .forbid ( NodeTreeBuilder::mMaskAttribute )
    .optional ( NodeTreeBuilder::mIdAttribute )
    .optional ( NodeTreeBuilder::mAddressAttribute )
    .optional ( NodeTreeBuilder::mParametersAttribute )
    .optional ( NodeTreeBuilder::mFirmwareInfo )
    .optional ( NodeTreeBuilder::mPermissionsAttribute )
    .optional ( NodeTreeBuilder::mModeAttribute )
    .optional ( NodeTreeBuilder::mSizeAttribute )
    .optional ( NodeTreeBuilder::mTagsAttribute )
    .optional ( NodeTreeBuilder::mDescriptionAttribute );
    //------------------------------------------------------------------------------------------------------------------------
    Rule<Node*> lBitMask;
    lBitMask.require ( NodeTreeBuilder::mMaskAttribute )
    .forbid ( NodeTreeBuilder::mClassAttribute )
    .forbid ( NodeTreeBuilder::mModuleAttribute )
    .optional ( NodeTreeBuilder::mAddressAttribute )		// .forbid ( NodeTreeBuilder::mAddressAttribute ) //see https://svnweb.cern.ch/trac/cactus/ticket/92
    .forbid ( NodeTreeBuilder::mModeAttribute )
    .forbid ( NodeTreeBuilder::mSizeAttribute )
    .optional ( NodeTreeBuilder::mParametersAttribute )
    .optional ( NodeTreeBuilder::mFirmwareInfo )
    .optional ( NodeTreeBuilder::mPermissionsAttribute )
    .optional ( NodeTreeBuilder::mTagsAttribute )
    .optional ( NodeTreeBuilder::mDescriptionAttribute );
    //------------------------------------------------------------------------------------------------------------------------
    Rule<Node*> lModule;
    lModule.optional ( NodeTreeBuilder::mIdAttribute )
    .require ( NodeTreeBuilder::mModuleAttribute )
    .forbid ( NodeTreeBuilder::mMaskAttribute )
    .optional ( NodeTreeBuilder::mClassAttribute ) //.forbid ( NodeTreeBuilder::mClassAttribute ) //see https://svnweb.cern.ch/trac/cactus/ticket/452
    .forbid ( NodeTreeBuilder::mModeAttribute )
    .forbid ( NodeTreeBuilder::mSizeAttribute )
    .forbid ( NodeTreeBuilder::mPermissionsAttribute )
    .optional ( NodeTreeBuilder::mAddressAttribute )
    .optional ( NodeTreeBuilder::mParametersAttribute )
    .optional ( NodeTreeBuilder::mFirmwareInfo )
    .optional ( NodeTreeBuilder::mTagsAttribute )
    .optional ( NodeTreeBuilder::mDescriptionAttribute );
    //------------------------------------------------------------------------------------------------------------------------
    mTopLevelNodeParser.addRule ( lPlainNode , std::bind ( &NodeTreeBuilder::plainNodeCreator , this , false , arg::_1 ) );
    mTopLevelNodeParser.addRule ( lBitMask , std::bind ( &NodeTreeBuilder::bitmaskNodeCreator , this , false , arg::_1 ) );
    mTopLevelNodeParser.addRule ( lModule , std::bind ( &NodeTreeBuilder::moduleNodeCreator , this , false , arg::_1 ) );
    //------------------------------------------------------------------------------------------------------------------------
    lPlainNode.require ( NodeTreeBuilder::mIdAttribute );
    lBitMask.require ( NodeTreeBuilder::mIdAttribute );
    lModule.require ( NodeTreeBuilder::mIdAttribute );
    //------------------------------------------------------------------------------------------------------------------------
    mNodeParser.addRule ( lPlainNode , std::bind ( &NodeTreeBuilder::plainNodeCreator , this , true , arg::_1 ) );
    mNodeParser.addRule ( lBitMask , std::bind ( &NodeTreeBuilder::bitmaskNodeCreator , this , true , arg::_1 ) );
    mNodeParser.addRule ( lModule , std::bind ( &NodeTreeBuilder::moduleNodeCreator , this , true , arg::_1 ) );
    //------------------------------------------------------------------------------------------------------------------------
  }


  NodeTreeBuilder::~NodeTreeBuilder ()
  {
    clearAddressFileCache();
  }


  NodeTreeBuilder& NodeTreeBuilder::getInstance()
  {
    if ( ! mInstance )
    {
      mInstance.reset(new NodeTreeBuilder());
    }

    return *mInstance;
  }


  Node* NodeTreeBuilder::getNodeTree ( const std::string& aFilenameExpr , const boost::filesystem::path& aPath )
  {
    std::vector< std::pair<std::string, std::string> >  lAddressFiles;
    uhal::utilities::ParseSemicolonDelimitedUriList ( aFilenameExpr , lAddressFiles );

    if ( lAddressFiles.size() != 1 )
    {
      exception::IncorrectAddressTableFileCount lExc;
      log ( lExc , "Exactly one address table file must be specified. The expression " , Quote ( aFilenameExpr ) , " contains " , Integer ( lAddressFiles.size() ) , " valid file expressions." );
      throw lExc;
    }

    std::vector< const Node* > lNodes;
    uhal::utilities::OpenFile ( lAddressFiles[0].first , lAddressFiles[0].second , aPath.parent_path() , std::bind ( &NodeTreeBuilder::CallBack, std::ref ( *this ) , arg::_1 , arg::_2 , arg::_3 , std::ref ( lNodes ) ) );

    if ( lNodes.size() != 1 )
    {
      exception::IncorrectAddressTableFileCount lExc;
      log ( lExc , "Exactly one address table file must be specified. The expression " , Quote ( lAddressFiles[0].second ) , " refers to " , Integer ( lNodes.size() ) , " valid files." );
      throw lExc;
    }

    Node* lNode ( lNodes[0]->clone() );
    return lNode;
  }


  void NodeTreeBuilder::clearAddressFileCache()
  {
    for(const auto& x: mNodes)
      delete x.second;
    mNodes.clear();
  }


  Node* NodeTreeBuilder::build(const pugi::xml_node& aNode, const boost::filesystem::path& aAddressFilePath)
  {
    mFileCallStack.push_back ( aAddressFilePath );
    Node* lNode ( mTopLevelNodeParser ( aNode ) );
    mFileCallStack.pop_back( );
    calculateHierarchicalAddresses ( lNode , 0x00000000 );
    checkForAddressCollisions ( lNode , aAddressFilePath );  // Needs further investigation - disabled for now as it causes exceptions with valid tables.

    return lNode;
  }


  void NodeTreeBuilder::CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile , std::vector< const Node* >& aNodes )
  {
    std::string lName ( aProtocol + ( aPath.string() ) );
    std::unordered_map< std::string , const Node* >::iterator lNodeIt = mNodes.find ( lName );

    if ( lNodeIt != mNodes.end() )
    {
      aNodes.push_back ( lNodeIt->second );
      return;
    }

    std::string lExtension ( aPath.extension().string().substr ( 0,4 ) );
    boost::to_lower ( lExtension ); //just in case someone decides to use capitals in their file extensions.

    if ( lExtension == ".xml" )
    {
      log ( Info() , "Reading XML address file " , Quote( aPath.c_str() ) );
      pugi::xml_document lXmlDocument;
      pugi::xml_parse_result lLoadResult = lXmlDocument.load_buffer_inplace ( & ( aFile[0] ) , aFile.size() );

      if ( !lLoadResult )
      {
        uhal::utilities::PugiXMLParseResultPrettifier ( lLoadResult , aPath , aFile );
        return;
      }

      pugi::xml_node lXmlNode = lXmlDocument.child ( "node" );

      if ( !lXmlNode )
      {
        log ( Error() , "No XML node called ", Quote ( "node" ) , " in file " , aPath.c_str() );
        return;
      }

      Node* lNode ( build ( lXmlNode , aPath ) );
      mNodes.insert ( std::make_pair ( lName , lNode ) );
      aNodes.push_back ( lNode );
      return;
    }
    else if ( lExtension == ".txt" )
    {
      log ( Info() , "TXT file" );
      log ( Error() , "Parser problems mean that this method has been disabled." );
      return;
    }
    else
    {
      log ( Error() , "Extension " , Quote ( lExtension ) , " not known." );
      return;
    }
  }


  Node* NodeTreeBuilder::plainNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode )
  {
    Node* lNode ( new Node() );
    setUid ( aRequireId , aXmlNode , lNode );
    setAddr ( aXmlNode , lNode );
    setPars ( aXmlNode , lNode );
    setFirmwareInfo ( aXmlNode , lNode );
    setClassName ( aXmlNode , lNode );
    setTags ( aXmlNode , lNode );
    setDescription ( aXmlNode , lNode );
    setModule ( aXmlNode , lNode );
    setPermissions ( aXmlNode , lNode );
    //setMask( aXmlNode , lNode );
    setModeAndSize ( aXmlNode , lNode );
    addChildren ( aXmlNode , lNode );
    log ( Debug() , lNode->mUid , " built by " , __PRETTY_FUNCTION__ );

    if ( lNode->mClassName.size() )
    {
      return DerivedNodeFactory::getInstance().convertToClassType ( lNode );
    }
    else
    {
      return lNode;
    }
  }

  Node* NodeTreeBuilder::moduleNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode )
  {
    std::string lModule;
    uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mModuleAttribute , lModule );
    Node* lNode ( getNodeTree ( lModule , mFileCallStack.back( ) ) );
    setUid ( aRequireId , aXmlNode , lNode );
    setAddr ( aXmlNode , lNode );
    setClassName ( aXmlNode , lNode );
    setPars ( aXmlNode , lNode );
    setFirmwareInfo ( aXmlNode , lNode );
    setTags ( aXmlNode , lNode );
    setDescription ( aXmlNode , lNode );
    setModule ( aXmlNode , lNode );
    //setPermissions( aXmlNode , lNode );
    //setMask( aXmlNode , lNode );
    //setModeAndSize( aXmlNode , lNode );
    //addChildren( aXmlNode , lNode );
    log ( Debug() , lNode->mUid , " built by " , __PRETTY_FUNCTION__ );

    if ( lNode->mClassName.size() )
    {
      return DerivedNodeFactory::getInstance().convertToClassType ( lNode );
    }
    else
    {
      return lNode;
    }
  }


  Node* NodeTreeBuilder::bitmaskNodeCreator ( const bool& aRequireId , const pugi::xml_node& aXmlNode )
  {
    if ( aXmlNode.child ( "node" ) )
    {
      exception::MaskedNodeCannotHaveChild lExc;
      log ( lExc , "Bit-masked nodes are not allowed to have child nodes" );
      throw lExc;
    }

    Node* lNode ( new Node() );
    setUid ( aRequireId , aXmlNode , lNode );
    setAddr ( aXmlNode , lNode ); //was commented out, see https://svnweb.cern.ch/trac/cactus/ticket/92
    setClassName ( aXmlNode , lNode );
    setPars ( aXmlNode , lNode );
    setFirmwareInfo ( aXmlNode , lNode );
    setTags ( aXmlNode , lNode );
    setDescription ( aXmlNode , lNode );
    setModule ( aXmlNode , lNode );
    setPermissions ( aXmlNode , lNode );
    setMask ( aXmlNode , lNode );
    //setModeAndSize( aXmlNode , lNode );
    //addChildren( aXmlNode , lNode );
    log ( Debug() , lNode->mUid , " built by " , __PRETTY_FUNCTION__ );
    return lNode;
  }




  void NodeTreeBuilder::setUid ( const bool& aRequireId , const pugi::xml_node& aXmlNode , Node* aNode )
  {
    const bool lHasId = uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mIdAttribute , aNode->mUid );

    if ( aRequireId and ( not lHasId ) )
    {
      //error description is given in the function itself so no more elaboration required
      throw exception::NodeMustHaveUID("'id' attribute is missing from address table node");
    }

    if ( lHasId )
    {
      if ( aNode->mUid.empty() )
        throw exception::NodeAttributeIncorrectValue("Invalid node ID specified (empty)");
      else if ( aNode->mUid.find('.') != std::string::npos )
        throw exception::NodeAttributeIncorrectValue("Invalid node ID '" + aNode->mUid + "' specified (contains dots)");
      else if ( ( aNode->mUid.at(0) == ' ' ) or ( aNode->mUid.at(aNode->mUid.size()-1) == ' ' ) )
        throw exception::NodeAttributeIncorrectValue("Invalid node ID '" + aNode->mUid + "' specified (contains spaces)");
    }
  }

  void NodeTreeBuilder::setAddr ( const pugi::xml_node& aXmlNode , Node* aNode )
  {
    //Address is an optional attribute for hierarchical addressing
    uint32_t lAddr ( 0 );
    uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mAddressAttribute , lAddr );
    aNode->mPartialAddr |= lAddr;
  }


  void NodeTreeBuilder::setClassName ( const pugi::xml_node& aXmlNode , Node* aNode )
  {
    //Address is an optional attribute for hierarchical addressing
    std::string lClassStr;
    uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mClassAttribute , lClassStr );

    aNode->mClassName = lClassStr;
  }

  void NodeTreeBuilder::setPars ( const pugi::xml_node& aXmlNode , Node* aNode )
  {
    std::string lParsStr;
    //get attribute from xml file as string
    uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mParametersAttribute , lParsStr );

    if ( lParsStr.size() )
    {
      //parse the string into a NodeTreeParameters object
      std::string::const_iterator lBegin ( lParsStr.begin() );
      std::string::const_iterator lEnd ( lParsStr.end() );
      std::unordered_map<std::string, std::string> lPars;
      boost::spirit::qi::phrase_parse ( lBegin , lEnd , mNodeTreeParametersGrammar , boost::spirit::ascii::space , lPars );
      // Update the parameters map
      // Add to lPars those previously defined (module node)
      lPars.insert ( aNode->mParameters.begin(), aNode->mParameters.end() );
      // Swap the containers
      aNode->mParameters.swap ( lPars );
    }
  }

  void NodeTreeBuilder::setTags ( const pugi::xml_node& aXmlNode , Node* aNode )
  {
    std::string lStr;
    //Tags is an optional attribute to allow the user to add a description to a node
    uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mTagsAttribute , lStr );

    if ( lStr.size() && aNode->mTags.size() )
    {
      aNode->mTags += "[";
      aNode->mTags += lStr;
      aNode->mTags += "]";
    }
    else if ( lStr.size() && !aNode->mTags.size() )
    {
      aNode->mTags = lStr;
    }
  }


  void NodeTreeBuilder::setDescription ( const pugi::xml_node& aXmlNode , Node* aNode )
  {
    std::string lStr;
    //Tags is an optional attribute to allow the user to add a description to a node
    uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mDescriptionAttribute , lStr );

    if ( lStr.size() && aNode->mDescription.size() )
    {
      aNode->mDescription += "[";
      aNode->mDescription += lStr;
      aNode->mDescription += "]";
    }
    else if ( lStr.size() && !aNode->mDescription.size() )
    {
      aNode->mDescription = lStr;
    }
  }

  void NodeTreeBuilder::setModule ( const pugi::xml_node& , Node* aNode )
  {
    if ( mFileCallStack.size() )
    {
      aNode->mModule = mFileCallStack.back( ).string();
    }
  }

  void NodeTreeBuilder::setPermissions ( const pugi::xml_node& aXmlNode , Node* aNode )
  {
    //Permissions is an optional attribute for specifying read/write permissions
    std::string lPermissionAttr;

    if ( uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mPermissionsAttribute , lPermissionAttr ) )
    {
      const defs::NodePermission* const lPermission = mPermissionsLut.find(lPermissionAttr.c_str());
      if (lPermission == NULL)
      {
        throw exception::NodeAttributeIncorrectValue("Permission attribute for node with ID '" + aNode->mUid + "' has incorrect value '" + lPermissionAttr + "'");
      }
      else
        aNode->mPermission = *lPermission;
    }
  }


  void NodeTreeBuilder::setMask ( const pugi::xml_node& aXmlNode , Node* aNode )
  {
    //Tags is an optional attribute to allow the user to add a description to a node
    uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mMaskAttribute , aNode->mMask );
  }


  void NodeTreeBuilder::setModeAndSize ( const pugi::xml_node& aXmlNode , Node* aNode )
  {
    //Mode is an optional attribute for specifying whether a block is incremental, non-incremental or a single register
    std::string lModeAttr;

    if ( uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mModeAttribute , lModeAttr ) )
    {
      const defs::BlockReadWriteMode* const lMode = mModeLut.find(lModeAttr.c_str());
      if (lMode == NULL)
      {
        throw exception::NodeAttributeIncorrectValue("Mode attribute for node with ID '" + aNode->mUid + "' has incorrect value '" + lModeAttr + "'");
      }
      else
        aNode->mMode = *lMode;

      if ( aNode->mMode == defs::INCREMENTAL )
      {
        //If a block is incremental it requires a size attribute
        if ( ! uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mSizeAttribute , aNode->mSize ) )
        {
          exception::IncrementalNodeRequiresSizeAttribute lExc;
          log ( lExc , "Node " , Quote ( aNode->mUid ) , " has type " , Quote ( "INCREMENTAL" ) , ", which requires a " , Quote ( NodeTreeBuilder::mSizeAttribute ) , " attribute" );
          throw lExc;
        }
      }
      else if ( aNode->mMode == defs::NON_INCREMENTAL )
      {
        //If a block is non-incremental, then a size attribute is recommended
        if ( ! uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mSizeAttribute , aNode->mSize ) )
        {
          log ( Notice() , "Node " , Quote ( aNode->mUid ) , " has type " , Quote ( "NON_INCREMENTAL" ) , " but does not have a " , Quote ( NodeTreeBuilder::mSizeAttribute ) , " attribute. This is not necessarily a problem, but if there is a limit to the size of the read/write operation from this port, then please consider adding this attribute for the sake of safety." );
        }
      }
    }
    else if ( not aXmlNode.attribute ( NodeTreeBuilder::mSizeAttribute.c_str() ).empty() )
    {
      log ( Warning() , "Invalid combination of attributes for node " , Quote ( aNode->mUid ) , ": Size attribute specified, but mode missing, hence size ignored. Please specify mode here or remove the size attribute. Address table parser will throw an exception for this in future releases.");
    }

  }

  void NodeTreeBuilder::setFirmwareInfo ( const pugi::xml_node& aXmlNode , Node* aNode )
  {
    //Address is an optional attribute for hierarchical addressing
    std::string lFwInfoStr;
    uhal::utilities::GetXMLattribute<false> ( aXmlNode , NodeTreeBuilder::mFirmwareInfo , lFwInfoStr );

    if ( lFwInfoStr.size() )
    {
      //parse the string into a NodeTreeFwInfoAttribute object
      std::string::const_iterator lBegin ( lFwInfoStr.begin() );
      std::string::const_iterator lEnd ( lFwInfoStr.end() );
      NodeTreeFirmwareInfoAttribute lFwInfo;
      boost::spirit::qi::phrase_parse ( lBegin , lEnd , mNodeTreeFirmwareInfoAttributeGrammar , boost::spirit::ascii::space , lFwInfo );
      aNode->mFirmwareInfo.insert ( make_pair ( "type",lFwInfo.mType ) );

      if ( lFwInfo.mArguments.size() )
      {
        aNode->mFirmwareInfo.insert ( lFwInfo.mArguments.begin() , lFwInfo.mArguments.end() );
      }
    }
  }


  void NodeTreeBuilder::addChildren ( const pugi::xml_node& aXmlNode , Node* aNode )
  {
    pugi::xml_node lXmlNode = aXmlNode.child ( "node" );

    if ( aNode->mMode == defs::NON_INCREMENTAL )
    {
      if ( lXmlNode )
      {
        exception::BlockAccessNodeCannotHaveChild lExc;
        log ( lExc , "Block access nodes are not allowed to have child nodes, but the node " , Quote ( aNode->mUid ) , " has a child node in the address table" );
        throw lExc;
      }
    }
    else
    {
      for ( ; lXmlNode; lXmlNode = lXmlNode.next_sibling ( "node" ) )
      {
        aNode->mChildren.push_back ( mNodeParser ( lXmlNode ) );
      }

      for (const auto& lChild: aNode->mChildren)
        aNode->mChildrenMap.insert ( std::make_pair ( lChild->mUid , lChild ) );
    }
  }

  void NodeTreeBuilder::calculateHierarchicalAddresses ( Node* aNode , const uint32_t& aAddr )
  {
    if ( aNode->mMode == defs::HIERARCHICAL )
    {
      if ( aNode->mChildren.size() == 0 )
      {
        aNode->mMode = defs::SINGLE;
      }
      else
      {
        // bool lAnyMasked( false );
        bool lAllMasked ( true );

        for (Node* lChild: aNode->mChildren)
        {
          if ( lChild->mMask == defs::NOMASK )
            lAllMasked = false;

          // else
          // {
          // lAnyMasked = true;
          // }
        }

        // if( lAnyMasked && !lAllMasked )
        // {
        // log ( Error() , "Both masked and unmasked children found in branch " , Quote ( aNode->mUid ) );
        // throw exception::// BothMaskedAndUnmaskedChildren();
        // }

        if ( lAllMasked )
        {
          aNode->mMode = defs::SINGLE;
        }
      }
    }

    if ( aNode->mMode == defs::INCREMENTAL )
    {
      uint64_t lTopAddr ( ( uint64_t ) ( aNode->mPartialAddr ) + ( uint64_t ) ( aNode->mSize-1 ) );

      //Check that the requested block size does not extend outside register space
      if ( lTopAddr >> 32 )
      {
        exception::ArraySizeExceedsRegisterBound lExc;
        log ( lExc , "A block size of " , Integer ( aNode->mSize ) , " and a base address of " , Integer ( aNode->mAddr , IntFmt<hex,fixed>() ) , " exceeds bounds of address space" );
        throw lExc;
      }

      /*
            //Test for overlap with parent
            if ( ( uint32_t ) ( lTopAddr ) & aAddr ) //should set the most significant bit of the child address and then AND this with the parent address
            {
              log ( Warning() , "The partial address of the top register in the current branch, " , Quote ( aNode->mUid ) , " , (" , Integer ( ( uint32_t ) ( lTopAddr ) , IntFmt<hex,fixed>() ) , ") overlaps with the partial address of the parent branch (" , Integer ( aAddr , IntFmt<hex,fixed>() ) , "). This might contradict the hierarchical design principal. For now this is a warning, but in the future this may be upgraded to throw an exception." );
            }

          }
          else
          {
            //Test for overlap with parent
            if ( aNode->mPartialAddr & aAddr ) //should set the most significant bit of the child address and then AND this with the parent address
            {
              log ( Warning() , "The partial address of the top register in the current branch, " , Quote ( aNode->mUid ) , " , (" , Integer ( aNode->mPartialAddr , IntFmt<hex,fixed>() ) , ") overlaps with the partial address of the parent branch (" , Integer ( aAddr , IntFmt<hex,fixed>() ) , "). This might contradict the hierarchical design principal. For now this is a warning, but in the future this may be upgraded to throw an exception." );
            }
      */
    }

    aNode->mAddr = aNode->mPartialAddr + aAddr;

    for (Node* lChild: aNode->mChildren)
    {
      lChild->mParent = aNode;
      calculateHierarchicalAddresses ( lChild , aNode->mAddr );
    }

    std::sort ( aNode->mChildren.begin() , aNode->mChildren.end() , detail::compareNodeAddr );
  }


  void NodeTreeBuilder::checkForAddressCollisions ( Node* aNode , const boost::filesystem::path& aPath )
  {
    std::vector<std::pair<const Node*, const Node*> > lOverlappingNodes = detail::getAddressOverlaps(*aNode);

    if ( not lOverlappingNodes.empty() )
    {
      // Add username to the collisions report filepath if environment variable USER is defined
      std::string lDirName("/tmp");
      if (char* lUsername = std::getenv("USER"))
      {
        lDirName += "/" + std::string(lUsername);
      }
      lDirName += "/uhal";

      boost::filesystem::path lDir ( lDirName );
      lDir.make_preferred();

      try{
        if ( !boost::filesystem::is_directory ( lDir ) )
        {
          if ( boost::filesystem::create_directories ( lDir ) )
          {
            boost::filesystem::permissions( lDir , boost::filesystem::all_all );
          }
          else
          {
            log ( Error() , "Address overlaps observed - attempted and failed to create directory " , Quote ( lDirName ) );
            return;
          }

        }
      }
      catch(const boost::filesystem::filesystem_error& e)
      {
        log ( Error() , "Address overlaps observed - failed to create directory " , Quote ( lDirName ) , " for report file; caught filesystem_error exception with what returning:  ", e.what() );
        return;
      }

      std::string lFilename ( aPath.string() );
      boost::replace_all ( lFilename , "/" , "-" );
      const std::string lReportPath( ( lDir / ( "OverlapReport" + lFilename + ".txt" ) ).string() );

      if ( detail::writeNodeOverlapReport(lReportPath, lOverlappingNodes, "Overlap report for \"" + aPath.string() + "\".") )
      {
        log ( Warning() , "Address overlaps observed - report file written at " , Quote ( lReportPath ) );
      }
      else
      {
        log ( Error() , "Address overlaps observed - failed to create report file " , Quote ( lReportPath ) );
      }
    }
  }


  NodeTreeBuilder::permissions_lut::permissions_lut()
  {
    add
    ( "r"			, defs::READ )
    ( "w"			, defs::WRITE )
    ( "read"		, defs::READ )
    ( "write"		, defs::WRITE )
    ( "rw"			, defs::READWRITE )
    ( "wr"			, defs::READWRITE )
    ( "readwrite"	, defs::READWRITE )
    ( "writeread"	, defs::READWRITE )
    ;
  }

  const NodeTreeBuilder::permissions_lut NodeTreeBuilder::mPermissionsLut;


  NodeTreeBuilder::mode_lut::mode_lut()
  {
    add
    ( "single"			, defs::SINGLE )
    ( "block"			, defs::INCREMENTAL )
    ( "port"			, defs::NON_INCREMENTAL )
    ( "incremental"		, defs::INCREMENTAL )
    ( "non-incremental"	, defs::NON_INCREMENTAL )
    ( "inc"				, defs::INCREMENTAL )
    ( "non-inc"			, defs::NON_INCREMENTAL )
    ;
  }

  const NodeTreeBuilder::mode_lut NodeTreeBuilder::mModeLut;

}
