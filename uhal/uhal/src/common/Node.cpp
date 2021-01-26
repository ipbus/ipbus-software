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

#include "uhal/Node.hpp"

#include <iomanip>
#ifdef __GNUG__
#include <cxxabi.h>
#endif

#include <boost/regex.hpp>

#include "uhal/log/log.hpp"
#include "uhal/HwInterface.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/NodeTreeBuilder.hpp"


namespace uhal
{

  Node::Node ( )  :
    mHw ( NULL ),
    mUid ( "" ),
    mPartialAddr ( 0x00000000 ),
    mAddr ( 0x00000000 ),
    mMask ( defs::NOMASK ),
    mPermission ( defs::READWRITE ),
    mMode ( defs::HIERARCHICAL ),
    mSize ( 0x00000001 ),
    mTags ( "" ),
    mDescription ( "" ),
    mModule ( "" ),
    mClassName ( "" ),
    mParameters ( ),
    mFirmwareInfo( ),
    mParent ( NULL ),
    mChildren ( ),
    mChildrenMap ( )
  {
  }


  Node::Node ( const Node& aNode )  :
    mHw ( aNode.mHw ),
    mUid ( aNode.mUid ),
    mPartialAddr ( aNode.mPartialAddr ),
    mAddr ( aNode.mAddr ),
    mMask ( aNode.mMask ),
    mPermission ( aNode.mPermission ),
    mMode ( aNode.mMode ),
    mSize ( aNode.mSize ),
    mTags ( aNode.mTags ),
    mDescription ( aNode.mDescription ),
    mModule ( aNode.mModule ),
    mClassName ( aNode.mClassName ),
    mParameters ( aNode.mParameters ),
    mFirmwareInfo ( aNode.mFirmwareInfo ),
    mParent ( NULL ),
    mChildren ( ),
    mChildrenMap ( )
  {
    mChildren.reserve(aNode.mChildren.size());
    for (Node* lChild : aNode.mChildren)
      mChildren.push_back (lChild->clone());

    for (Node* lChild : mChildren)
    {
      lChild->mParent = this;
      mChildrenMap.insert ( std::make_pair ( lChild->mUid , lChild ) );
    }
  }


  Node& Node::operator= ( const Node& aNode )
  {
    mHw = aNode.mHw;
    mUid = aNode.mUid ;
    mPartialAddr = aNode.mPartialAddr;
    mAddr = aNode.mAddr;
    mMask = aNode.mMask;
    mPermission = aNode.mPermission;
    mMode = aNode.mMode;
    mSize = aNode.mSize;
    mTags = aNode.mTags;
    mDescription = aNode.mDescription;
    mModule = aNode.mModule;
    mClassName = aNode.mClassName;
    mParameters = aNode.mParameters;

    for (Node* lChild: mChildren)
    {
      if (lChild)
      {
        delete lChild;
        lChild = NULL;
      }
    }

    mChildren.clear();
    mChildrenMap.clear();

    mChildren.reserve(aNode.mChildren.size());
    for (Node* lNode: aNode.mChildren)
      mChildren.push_back ( lNode->clone() );

    for (Node* lNode: mChildren)
    {
      lNode->mParent = this;
      mChildrenMap.insert ( std::make_pair ( lNode->mUid , lNode ) );
    }

    return *this;
  }


  Node* Node::clone ( ) const
  {
    return new Node ( *this );
  }


  Node::~Node()
  {
    for (Node* lNode: mChildren)
    {
      if (lNode)
      {
        delete lNode;
        lNode = NULL;
      }
    }

    mChildren.clear();
    mChildrenMap.clear();
  }




  Node::const_iterator Node::begin() const
  {
    return Node::const_iterator ( this );
  }


  Node::const_iterator Node::end() const
  {
    return Node::const_iterator();
  }


  bool Node::operator == ( const Node& aNode ) const
  {
    return this->getAddress() == aNode.getAddress() &&
           this->getMask() == aNode.getMask() &&
           this->getPermission() == aNode.getPermission() &&
           this->getId() == aNode.getId();
  }


  const std::string& Node::getId() const
  {
    return mUid;
  }


  std::string Node::getPath() const
  {
    std::deque< const Node* > lPath;
    getAncestors ( lPath );
    std::string lRet;

    for (const Node* lNode: lPath)
    {
      if ( lNode->mUid.size() )
      {
        lRet += lNode->mUid;
        lRet += ".";
      }
    }

    if ( lRet.size() )
    {
      lRet.resize ( lRet.size() - 1 );
    }

    return lRet;
  }


  std::string Node::getRelativePath(const Node& aAncestor) const
  {
    std::deque< const Node* > lPath;
    getAncestors ( lPath );
    std::string lRet;

    for (std::deque< const Node* >::iterator lIt ( std::find(lPath.begin(), lPath.end(), &aAncestor) + 1 ) ; lIt != lPath.end() ; ++lIt )
    {
      if ( ( **lIt ).mUid.size() )
      {
        lRet += ( **lIt ).mUid;
        lRet += ".";
      }
    }

    if ( lRet.size() )
    {
      lRet.resize ( lRet.size() - 1 );
    }

    return lRet;
  }


  void Node::getAncestors ( std::deque< const Node* >& aPath ) const
  {
    aPath.push_front ( this );

    if ( mParent )
    {
      mParent->getAncestors ( aPath );
    }
  }


  const uint32_t& Node::getAddress() const
  {
    return mAddr;
  }


  const uint32_t& Node::getMask() const
  {
    return mMask;
  }


  const defs::BlockReadWriteMode& Node::getMode() const
  {
    return mMode;
  }


  const uint32_t& Node::getSize() const
  {
    return mSize;
  }


  const defs::NodePermission& Node::getPermission() const
  {
    return mPermission;
  }


  const std::string& Node::getTags() const
  {
    return mTags;
  }


  const std::string& Node::getDescription() const
  {
    return mDescription;
  }


  const std::string& Node::getModule() const
  {
    return mModule;
  }


  const std::unordered_map< std::string, std::string >& Node::getParameters() const
  {
    return mParameters;
  }


  const std::unordered_map< std::string, std::string >& Node::getFirmwareInfo() const
  {
    return mFirmwareInfo;
  }


  void Node::stream ( std::ostream& aStr , std::size_t aIndent ) const
  {
    std::ios_base::fmtflags original_flags = std::cout.flags();

    aStr << std::setfill ( '0' ) << std::uppercase;
    aStr << '\n' << std::string ( aIndent , ' ' ) << "+ ";
    aStr << "Node \"" << mUid << "\", ";

    if ( &typeid ( *this ) != &typeid ( Node ) )
    {
      aStr << "of type \"";
#ifdef __GNUG__
      // this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
      int lStatus ( 0 );
      static std::size_t lSize ( 1024 );
      static char* lDemangled = new char[lSize];
      aStr << ( abi::__cxa_demangle ( typeid ( *this ).name() , lDemangled , &lSize , &lStatus ) );
#else
      aStr << typeid ( *this ).name();
#endif
      aStr << "\", ";
    }

    switch ( mMode )
    {
      case defs::SINGLE:
        aStr << "SINGLE register, "
             << std::hex << "Address 0x" << std::setw ( 8 ) << mAddr << ", "
             << std::hex << "Mask 0x" << std::setw ( 8 ) << mMask << ", "
             << "Permissions " << ( mPermission&defs::READ?'r':'-' ) << ( mPermission&defs::WRITE?'w':'-' ) ;
        break;
      case defs::INCREMENTAL:
        aStr << "INCREMENTAL block, "
             << std::dec << "Size " << mSize << ", "
             << std::hex << "Addresses [0x" << std::setw ( 8 ) << mAddr << "-" << std::setw ( 8 ) << ( mAddr+mSize-1 ) << "], "
             << "Permissions " << ( mPermission&defs::READ?'r':'-' ) << ( mPermission&defs::WRITE?'w':'-' ) ;
        break;
      case defs::NON_INCREMENTAL:
        aStr << "NON-INCREMENTAL block, ";

        if ( mSize != 1 )
        {
          aStr << std::dec << "Size " << mSize << ", ";
        }

        aStr << std::hex << "Address 0x"  << std::setw ( 8 ) << mAddr << ", "
             << "Permissions " << ( mPermission&defs::READ?'r':'-' ) << ( mPermission&defs::WRITE?'w':'-' ) ;
        break;
      case defs::HIERARCHICAL:
        aStr << std::hex << "Address 0x" << std::setw ( 8 ) << mAddr;
        break;
    }

    if ( mTags.size() )
    {
      aStr << ", Tags \"" << mTags << "\"";
    }

    if ( mDescription.size() )
    {
      aStr << ", Description \"" << mDescription << "\"";
    }

    if ( mModule.size() )
    {
      aStr << ", Module \"" << mModule << "\"";
    }

    if ( mClassName.size() )
    {
      aStr << ", Class Name \"" << mClassName << "\"";
    }

    if ( mParameters.size() )
    {
      aStr << ", Parameters: ";
      std::unordered_map<std::string, std::string>::const_iterator lIt;

      for ( lIt = mParameters.begin(); lIt != mParameters.end(); ++lIt )
      {
        aStr << lIt->first << "=" << lIt->second << ";";
      }
    }

    // Reset the integer base (i.e. std::dec, std::hex, etc) to its original value
    aStr.flags(original_flags);

    // Recursively print children 
    for ( std::vector< Node* >::const_iterator lIt = mChildren.begin(); lIt != mChildren.end(); ++lIt )
    {
      ( **lIt ).stream ( aStr , aIndent+2 );
    }
  }


  const Node& Node::getNode ( const std::string& aId ) const
  {
    if ( aId.size() == 0 )
    {
      return *this;
    }

    size_t lStartIdx = 0;
    size_t lDotIdx = 0;

    const Node* lDescendant = this;

    do {
      lDotIdx = aId.find('.', lStartIdx);
      std::unordered_map< std::string , Node* >::const_iterator lIt = lDescendant->mChildrenMap.find ( aId.substr(lStartIdx, lDotIdx - lStartIdx) );

      if (lIt != lDescendant->mChildrenMap.end()) {
        lDescendant = lIt->second;
      }
      else if (lDescendant == this) {
        exception::NoBranchFoundWithGivenUID lExc;
        log ( lExc , "No branch found with ID-path " ,  Quote ( aId ) , " from node " , Quote ( this->getPath() ) , " (no partial match)" );
        throw lExc;
      }
      else {
        exception::NoBranchFoundWithGivenUID lExc;
        log ( lExc , "No branch found with ID-path " ,  Quote ( aId ) , " from node " , Quote ( this->getPath() ) , " (partial match for node ", Quote ( lDescendant->getRelativePath(*this) ), ")" );
        throw lExc;
      }

      lStartIdx = lDotIdx + 1;
    } while (lDotIdx != std::string::npos);

    return *lDescendant;
  }


  std::vector<std::string> Node::getNodes() const
  {
    std::vector<std::string> lNodes;

    // The 'begin' method returns an iterator to this instance, so skip to the next node straight away
    for (Node::const_iterator lIt = ++begin(); lIt != end(); lIt++)
    {
      lNodes.push_back(lIt->getRelativePath(*this));
    }

    return lNodes;
  }


  std::vector<std::string> Node::getNodes ( const std::string& aRegex ) const
  {
    std::vector<std::string> lNodes;
    log ( Info() , "Regular Expression : " , aRegex );

    // The 'begin' method returns an iterator to this instance, so skip to the next node straight away
    for (Node::const_iterator lIt = ++begin(); lIt != end(); lIt++)
    {
      const std::string lRelativePath(lIt->getRelativePath(*this));
      boost::cmatch lMatch;

      if ( boost::regex_match ( lRelativePath.c_str() , lMatch , boost::regex ( aRegex ) ) ) //to allow partial match, add  boost::match_default|boost::match_partial  as fourth argument
      {
        log ( Info() , lRelativePath , " matches" );
        lNodes.push_back ( lRelativePath );
      }
    }

    //bit dirty but since the hash map sorts them by the hash, not the value, they are completely scrambled here making it very hard to use.
    std::sort ( lNodes.begin(), lNodes.end() );
    return lNodes;
  }


  ValHeader  Node::write ( const uint32_t& aValue ) const
  {
    if ( mPermission & defs::WRITE )
    {
      if ( mMask == defs::NOMASK )
      {
        return mHw->getClient().write ( mAddr , aValue );
      }
      else if ( mPermission & defs::READ )
      {
        return mHw->getClient().write ( mAddr , aValue , mMask );
      }
      else // Masked write-only register
      {
        // RMWbits transaction should not be performed automatically on masked write-only node, since RMW transactions involve reading the register
        exception::WriteAccessDenied lExc;
        log ( lExc , "Cannot automatically perform write on write-only masked node " , Quote ( this->getPath() ) , "; masked write would have to be implemented as RMW transaction, which requires read permissions." ); 
      }
    }

    exception::WriteAccessDenied lExc;
    log ( lExc , "Node " , Quote ( this->getPath() ) , ": permissions denied write access" );
    throw lExc;
  }


  ValHeader  Node::writeBlock ( const std::vector< uint32_t >& aValues ) const // , const defs::BlockReadWriteMode& aMode )
  {
    if ( ( mMode == defs::SINGLE ) && ( aValues.size() != 1 ) ) //We allow the user to call a bulk access of size=1 to a single register
    {
      exception::BulkTransferOnSingleRegister lExc;
      log ( lExc , "Bulk Transfer requested on single register node " , Quote ( this->getPath() ) );
      log ( lExc , "If you were expecting an incremental write, please modify your address file to add the 'mode=",  Quote ( "incremental" ) , "' flags there" );
      throw lExc;
    }

    if ( ( mSize != 1 ) && ( aValues.size() >mSize ) )
    {
      exception::BulkTransferRequestedTooLarge lExc;
      log ( lExc , "Requested bulk write of greater size than the specified endpoint size of node ", Quote ( this->getPath() ) );
      throw lExc;
    }

    if ( mPermission & defs::WRITE )
    {
      return mHw->getClient().writeBlock ( mAddr , aValues , mMode ); //aMode );
    }
    else
    {
      exception::WriteAccessDenied lExc;
      log ( lExc , "Node " , Quote ( this->getPath() ) , ": permissions denied write access" );
      throw lExc;
    }
    
  }


  ValHeader  Node::writeBlockOffset ( const std::vector< uint32_t >& aValues , const uint32_t& aOffset ) const // , const defs::BlockReadWriteMode& aMode )
  {
    if ( mMode == defs::NON_INCREMENTAL )
    {
      exception::BulkTransferOffsetRequestedForFifo lExc;
      log ( lExc , "Bulk Transfer Offset requested for non-incremental node " , Quote ( this->getPath() ) );
      throw lExc;
    }

    if ( mMode == defs::SINGLE ) //We allow the user to call a bulk access of size=1 to a single register
    {
      exception::BulkTransferOffsetRequestedForSingleRegister lExc;
      log ( lExc , "Bulk Transfer with offset requested on single register node " , Quote ( this->getPath() ) );
      log ( lExc , "If you were expecting an incremental write, please modify your address file to add the 'mode=",  Quote ( "incremental" ) , "' flags there" );
      throw lExc;
    }

    if ( (aValues.size()+aOffset) > mSize )
    {
      exception::BulkTransferRequestedTooLarge lExc;
      log ( lExc , "Requested bulk write size and offset would overflow the specified endpoint node ", Quote ( this->getPath() ) );
      throw lExc;
    }

    if ( mPermission & defs::WRITE )
    {
      return mHw->getClient().writeBlock ( mAddr+aOffset , aValues , mMode ); //aMode );
    }
    else
    {
      exception::WriteAccessDenied lExc;
      log ( lExc , "Node " , Quote ( this->getPath() ) , ": permissions denied write access" );
      throw lExc;
    }
    
  }


  ValWord< uint32_t > Node::read() const
  {
    if ( mPermission & defs::READ )
    {
      if ( mMask == defs::NOMASK )
      {
        return mHw->getClient().read ( mAddr );
      }
      else
      {
        return mHw->getClient().read ( mAddr , mMask );
      }
    }

    exception::ReadAccessDenied lExc;
    log ( lExc , "Node " , Quote ( this->getPath() ) , ": permissions denied read access" );
    throw lExc;
    
  }


  ValVector< uint32_t > Node::readBlock ( const uint32_t& aSize ) const //, const defs::BlockReadWriteMode& aMode )
  {
    if ( ( mMode == defs::SINGLE ) && ( aSize != 1 ) ) //We allow the user to call a bulk access of size=1 to a single register
    {
      exception::BulkTransferOnSingleRegister lExc;
      log ( lExc , "Bulk Transfer requested on single register node ", Quote ( this->getPath() ) );
      log ( lExc , "If you were expecting an incremental read, please modify your address file to add the 'mode=",  Quote ( "incremental" ) , "' flags there" );
      throw lExc;
    }

    if ( ( mSize != 1 ) && ( aSize>mSize ) )
    {
      exception::BulkTransferRequestedTooLarge lExc;
      log ( lExc , "Requested bulk read of greater size than the specified endpoint size of node " , Quote ( this->getPath() ) );
      throw lExc;
    }

    if ( mPermission & defs::READ )
    {
      return mHw->getClient().readBlock ( mAddr , aSize , mMode ); //aMode );
    }
    else
    {
      exception::ReadAccessDenied lExc;
      log ( lExc , "Node " , Quote ( this->getPath() ) , ": permissions denied read access" );
      throw lExc;
    }
    
  }


  ValVector< uint32_t > Node::readBlockOffset ( const uint32_t& aSize , const uint32_t& aOffset ) const //, const defs::BlockReadWriteMode& aMode )
  {
    if ( mMode == defs::NON_INCREMENTAL )
    {
      exception::BulkTransferOffsetRequestedForFifo lExc;
      log ( lExc , "Bulk Transfer offset requested for non-incremental node " , Quote ( this->getPath() ) );
      throw lExc;
    }

    if ( mMode == defs::SINGLE ) //We do not allow the user to use an offset from a single register
    {
      exception::BulkTransferOffsetRequestedForSingleRegister lExc;
      log ( lExc , "Bulk Transfer with offset requested on single register node ", Quote ( this->getPath() ) );
      log ( lExc , "If you were expecting an incremental read, please modify your address file to add the 'mode=",  Quote ( "incremental" ) , "' flags there" );
      throw lExc;
    }

    if ( (aSize+aOffset) > mSize )
    {
      exception::BulkTransferRequestedTooLarge lExc;
      log ( lExc , "Requested bulk read size and offset would overflow the specified endpoint node " , Quote ( this->getPath() ) );
      throw lExc;
    }

    if ( mPermission & defs::READ )
    {
      return mHw->getClient().readBlock ( mAddr+aOffset , aSize , mMode ); //aMode );
    }
    else
    {
      exception::ReadAccessDenied lExc;
      log ( lExc , "Node " , Quote ( this->getPath() ) , ": permissions denied read access" );
      throw lExc;
    }
    
  }


  ClientInterface& Node::getClient() const
  {
    return mHw->getClient();
  }


  std::vector<const Node*> Node::getLineage(const Node& aAncestor) const
  {
    std::vector<const Node*> lAncestors;
    const Node* lAncestor = mParent;

    do {
      if (lAncestor == NULL)
        throw std::runtime_error("Node '" + aAncestor.getPath() + "' is not an ancestor of '" + getPath() + "'");

      lAncestors.push_back(lAncestor);
      lAncestor = lAncestor->mParent;
    }
    while (lAncestors.back() != &aAncestor);

    return std::vector<const Node*>(lAncestors.rbegin(), lAncestors.rend());
  }

  bool Node::isChildOf(const Node& aParent) const
  {
    return (&aParent == mParent);
  }


  Node::const_iterator::const_iterator() :
    mBegin ( NULL )
  {}


  Node::const_iterator::const_iterator ( const Node* aBegin ) :
    mBegin ( aBegin )
  {}


  Node::const_iterator::const_iterator ( const const_iterator& aOrig ) :
    mBegin ( aOrig.mBegin ) ,
    mItStack ( aOrig.mItStack )
  {}


  Node::const_iterator::~const_iterator()
  {}


  const Node& Node::const_iterator::operator*() const
  {
    return value();
  }


  const Node* Node::const_iterator::operator->() const
  {
    return & ( value() );
  }


  const Node& Node::const_iterator::value() const
  {
    return ( mItStack.empty() ) ? ( *mBegin ) : ( **mItStack.front() );
  }


  Node::const_iterator& Node::const_iterator::operator++()
  {
    next();
    return *this;
  }


  Node::const_iterator Node::const_iterator::operator++ ( int )
  {
    Node::const_iterator lTemp ( *this );
    next();
    return lTemp;
  }


  bool Node::const_iterator::next()
  {
    // Null iterator can't be incremented...
    if ( !mBegin )
    {
      return false;
    }

    if ( mItStack.empty() )
    {
      //We have just started and have no stack...
      if ( mBegin->mChildren.size() )
      {
        //We have children so recurse down to them
        mItStack.push_front ( mBegin->mChildren.begin() );
        return true;
      }

      //We have no children so we are at the end of the iteration. Make Buffer NULL to stop infinite loop
      mBegin = NULL;
      return false;
    }

    //We are already in the tree...
    if ( not ( **mItStack.front() ).mChildren.empty() )
    {
      // Entry has children, recurse...
      mItStack.push_front ( ( **mItStack.front() ).mChildren.begin() );
      return true;
    }

    // No children so go to the next entry on this level
    while ( not mItStack.empty() )
    {
      if ( ++ ( mItStack.front() ) != ( ( mItStack.size() == 1 ) ? ( *mBegin ) : ( **mItStack.at(1) ) ).mChildren.end() )
      {
        // Next entry on this level is valid - return
        return true;
      }

      // No more valid entries in this level, go back up tree
      mItStack.pop_front();
    }

    //We have no more children so we are at the end of the iteration. Make Buffer NULL to stop infinite loop
    mBegin = NULL;
    return false;
  }


  bool Node::const_iterator::operator!= ( const Node::const_iterator& aIt ) const
  {
    return ! ( *this == aIt ) ;
  }


  bool Node::const_iterator::operator== ( const Node::const_iterator& aIt ) const
  {
    return ( aIt.mBegin == mBegin ) && ( aIt.mItStack == mItStack ) ;
  }


  std::ostream& operator<< ( std::ostream& aStr ,  const uhal::Node& aNode )
  {
    aNode.stream ( aStr );
    return aStr;
  }

}
