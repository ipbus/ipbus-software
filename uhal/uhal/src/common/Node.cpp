#include "uhal/Node.hpp"

#include "uhal/HwInterface.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/NodeTreeBuilder.hpp"
#include "uhal/Utilities.hpp"

#include "uhal/log/log.hpp"

#include <iomanip>

#ifdef __GNUG__
#include <cxxabi.h>
#endif

namespace uhal
{
  /**
  	The log_inserter function to add the node object to a log entry
  	@param aNode a node to format and print to log
  */
  template < >
  void log_inserter< uhal::Node > ( const uhal::Node& aNode )
  {
    std::stringstream lStream;
    aNode.stream ( lStream );
    std::istreambuf_iterator<char> lEnd;
    std::istreambuf_iterator<char> lIt ( lStream.rdbuf() );

    while ( lIt!=lEnd )
    {
      put ( *lIt++ );
    }
  }
}







namespace uhal
{






Node::Node ( ) try :
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
        mChildren ( ),
        mChildrenMap ( )
  {
  }
  catch ( uhal::exception& aExc )
  {
    aExc.rethrowFrom ( ThisLocation() );
  }
  catch ( const std::exception& aExc )
  {
    StdException ( aExc ).throwFrom ( ThisLocation() );
  }


Node::Node ( const Node& aNode ) try :
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
        mChildren ( ),
        mChildrenMap ( )
  {
    for ( std::deque< Node* >::const_iterator lIt = aNode.mChildren.begin(); lIt != aNode.mChildren.end(); ++lIt )
    {
      mChildren.push_back ( ( **lIt ).clone() );
    }

    for ( std::deque< Node* >::iterator lIt = mChildren.begin(); lIt != mChildren.end(); ++lIt )
    {
      mChildrenMap.insert ( std::make_pair ( ( **lIt ).mUid , *lIt ) );

      for ( std::hash_map< std::string , Node* >::iterator lSubMapIt = ( **lIt ).mChildrenMap.begin() ; lSubMapIt != ( **lIt ).mChildrenMap.end() ; ++lSubMapIt )
      {
        mChildrenMap.insert ( std::make_pair ( ( ( **lIt ).mUid ) +'.'+ ( lSubMapIt->first ) , lSubMapIt->second ) );
      }
    }
  }
  catch ( uhal::exception& aExc )
  {
    aExc.rethrowFrom ( ThisLocation() );
  }
  catch ( const std::exception& aExc )
  {
    StdException ( aExc ).throwFrom ( ThisLocation() );
  }



  Node& Node::operator= ( const Node& aNode )
  {
    try
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

      for ( std::deque< Node* >::iterator lIt = mChildren.begin(); lIt != mChildren.end(); ++lIt )
      {
        if ( *lIt )
        {
          delete ( *lIt );
          ( *lIt ) = NULL;
        }
      }

      mChildren.clear();
      mChildrenMap.clear();

      for ( std::deque< Node* >::const_iterator lIt = aNode.mChildren.begin(); lIt != aNode.mChildren.end(); ++lIt )
      {
        mChildren.push_back ( ( **lIt ).clone() );
      }

      for ( std::deque< Node* >::iterator lIt = mChildren.begin(); lIt != mChildren.end(); ++lIt )
      {
        mChildrenMap.insert ( std::make_pair ( ( **lIt ).mUid , *lIt ) );

        for ( std::hash_map< std::string , Node* >::iterator lSubMapIt = ( **lIt ).mChildrenMap.begin() ; lSubMapIt != ( **lIt ).mChildrenMap.end() ; ++lSubMapIt )
        {
          mChildrenMap.insert ( std::make_pair ( ( ( **lIt ).mUid ) +'.'+ ( lSubMapIt->first ) , lSubMapIt->second ) );
        }
      }

      return *this;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  Node* Node::clone ( ) const
  {
    try
    {
      return new Node ( *this );
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }




  Node::~Node()
  {
    try
    {
      for ( std::deque< Node* >::iterator lIt = mChildren.begin(); lIt != mChildren.end(); ++lIt )
      {
        if ( *lIt )
        {
          delete ( *lIt );
          ( *lIt ) = NULL;
        }
      }

      mChildren.clear();
      mChildrenMap.clear();
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  bool Node::operator == ( const Node& aNode ) const
  {
    try
    {
      return this->getAddress() == aNode.getAddress() &&
             this->getMask() == aNode.getMask() &&
             this->getPermission() == aNode.getPermission() &&
             this->getId() == aNode.getId();
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  const std::string& Node::getId() const
  {
    try
    {
      return mUid;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }

  const uint32_t& Node::getAddress() const
  {
    try
    {
      return mAddr;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }

  const uint32_t& Node::getMask() const
  {
    try
    {
      return mMask;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }

  const defs::BlockReadWriteMode& Node::getMode() const
  {
    try
    {
      return mMode;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }

  const uint32_t& Node::getSize() const
  {
    try
    {
      return mSize;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }

  const defs::NodePermission& Node::getPermission() const
  {
    try
    {
      return mPermission;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  const std::string& Node::getTags() const
  {
    try
    {
      return mTags;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  const std::string& Node::getDescription() const
  {
    try
    {
      return mDescription;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  void Node::stream ( std::ostream& aStream , std::size_t aIndent ) const
  {
    try
    {
      aStream << std::setfill ( '0' ) << std::uppercase;
      aStream << '\n' << std::string ( aIndent , ' ' ) << "+ ";
      aStream << "Node \"" << mUid << "\", ";
      
      if( &typeid( *this ) != &typeid( Node ) )
      {
        aStream << "of type \"";

#ifdef __GNUG__
        // this is fugly but necessary due to the way that typeid::name() returns the object type name under g++.
        int lStatus ( 0 );
        aStream << abi::__cxa_demangle ( typeid ( *this ).name() , 0 , 0 , &lStatus );
#else
        aStream << typeid ( *this ).name();
#endif   

        aStream << "\", ";
      }

      switch ( mMode )
      {
        case defs::SINGLE:
          aStream << "SINGLE register, "
          << std::hex << "Address 0x" << std::setw ( 8 ) << mAddr << ", "
          << std::hex << "Mask 0x" << std::setw ( 8 ) << mMask << ", "
          << "Permissions " << ( mPermission&defs::READ?'r':'-' ) << ( mPermission&defs::WRITE?'w':'-' ) ;
          break;
        case defs::INCREMENTAL:
          aStream << "INCREMENTAL block, "
          << std::dec << "Size " << mSize << ", "
          << std::hex << "Addresses [0x" << std::setw ( 8 ) << mAddr << "-" << std::setw ( 8 ) << ( mAddr+mSize-1 ) << "], "
          << "Permissions " << ( mPermission&defs::READ?'r':'-' ) << ( mPermission&defs::WRITE?'w':'-' ) ;
          break;
        case defs::NON_INCREMENTAL:
          aStream << "NON-INCREMENTAL block, ";

          if ( mSize != 1 )
          {
            aStream << std::dec << "Size " << mSize << ", ";
          }

          aStream << std::hex << "Address 0x"  << std::setw ( 8 ) << mAddr << ", "
          << "Permissions " << ( mPermission&defs::READ?'r':'-' ) << ( mPermission&defs::WRITE?'w':'-' ) ;
          break;
        case defs::HIERARCHICAL:
          aStream << std::hex << "Address 0x" << std::setw ( 8 ) << mAddr;
          break;
      }

      if ( mTags.size() )
      {
        aStream << ", Tags \"" << mTags << "\"";
      }

      if ( mDescription.size() )
      {
        aStream << ", Description \"" << mDescription << "\"";
      }

      for ( std::deque< Node* >::const_iterator lIt = mChildren.begin(); lIt != mChildren.end(); ++lIt )
      {
        ( **lIt ).stream ( aStream , aIndent+2 );
      }
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  Node& Node::getNode ( const std::string& aId ) const
  {
    try
    {
      std::hash_map< std::string , Node* >::const_iterator lIt = mChildrenMap.find ( aId );

      if ( lIt==mChildrenMap.end() )
      {
        log ( Error() , "No branch found with ID-path " ,  Quote ( aId ) );
        std::size_t lPos ( std::string::npos );
        bool lPartialMatch ( false );

        while ( true )
        {
          lPos = aId.rfind ( '.' , lPos );

          if ( lPos == std::string::npos )
          {
            break;
          }

          std::hash_map< std::string , Node* >::const_iterator lIt = mChildrenMap.find ( aId.substr ( 0 , lPos ) );

          if ( lIt!=mChildrenMap.end() )
          {
            log ( Error() , "Partial match " ,  Quote ( aId.substr ( 0 , lPos ) ) , " found for ID-path " ,  Quote ( aId ) );
            log ( Error() , "Tree structure of partial match is:" , * ( lIt->second ) );
            lPartialMatch = true;
            break;
          }

          lPos--;
        }

        if ( !lPartialMatch )
        {
          log ( Error() , "Not even a partial match found for ID-path " ,  Quote ( aId ) , ". If this address looks correct, please check for leading, trailing and stray whitespace.\nTree structure is:" , *this );
        }

        NoBranchFoundWithGivenUID().throwFrom ( ThisLocation() );
      }

      return * ( lIt->second );
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  std::vector<std::string> Node::getNodes() const
  {
    try
    {
      std::vector<std::string> lNodes;
      lNodes.reserve ( mChildrenMap.size() ); //prevent reallocations

      for ( std::hash_map< std::string , Node* >::const_iterator lIt = mChildrenMap.begin(); lIt != mChildrenMap.end(); ++lIt )
      {
        lNodes.push_back ( lIt->first );
      }

      return lNodes;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }

  std::vector<std::string> Node::getNodes ( const std::string& aRegex ) const
  {
    try
    {
      std::vector<std::string> lNodes;
      lNodes.reserve ( mChildrenMap.size() ); //prevent reallocations
      log ( Info() , "Regular Expression : " , aRegex );

      for ( std::hash_map< std::string , Node* >::const_iterator lIt = mChildrenMap.begin(); lIt != mChildrenMap.end(); ++lIt )
      {
        boost::cmatch lMatch;

        if ( boost::regex_match ( lIt->first.c_str() , lMatch , boost::regex ( aRegex ) ) ) //to allow partial match, add  boost::match_default|boost::match_partial  as fourth argument
        {
          log ( Info() , lIt->first , " matches" );
          lNodes.push_back ( lIt->first );
        }
      }

      //bit dirty but since the hash map sorts them by the hash, not the value, they are completely scrambled here making it very hard to use.
      std::sort ( lNodes.begin(), lNodes.end() );
      return lNodes;
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }

  ValHeader  Node::write ( const uint32_t& aValue ) const
  {
    try
    {
      if ( mPermission & defs::WRITE )
      {
        if ( mMask == defs::NOMASK )
        {
          return mHw->getClient().write ( mAddr , aValue );
        }
        else
        {
          return mHw->getClient().write ( mAddr , aValue , mMask );
        }
      }
      else
      {
        log ( Error() , "Node permissions denied write access" );
        WriteAccessDenied().throwFrom ( ThisLocation() );
      }
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  ValHeader  Node::writeBlock ( const std::vector< uint32_t >& aValues ) const // , const defs::BlockReadWriteMode& aMode )
  {
    try
    {
      if ( ( mMode == defs::SINGLE ) && ( aValues.size() != 1 ) ) //We allow the user to call a bulk access of size=1 to a single register
      {
        log ( Error() , "Bulk Transfer requested on single register node" );
        log ( Error() , "If you were expecting an incremental write, please modify your address file to add the 'mode=",  Quote ( "incremental" ) , "' flags there" );
        BulkTransferOnSingleRegister().throwFrom ( ThisLocation() );
      }
      else
      {
        if ( ( mSize != 1 ) && ( aValues.size() >mSize ) )
        {
          log ( Error() , "Requested bulk write of greater size than the specified endpoint size" );
          BulkTransferRequestedTooLarge().throwFrom ( ThisLocation() );
        }

        if ( mPermission & defs::WRITE )
        {
          return mHw->getClient().writeBlock ( mAddr , aValues , mMode ); //aMode );
        }
        else
        {
          log ( Error() , "Node permissions denied write access" );
          WriteAccessDenied().throwFrom ( ThisLocation() );
        }
      }
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  ValWord< uint32_t > Node::read() const
  {
    try
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
      else
      {
        log ( Error() , "Node permissions denied read access" );
        ReadAccessDenied().throwFrom ( ThisLocation() );
      }
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }


  ValVector< uint32_t > Node::readBlock ( const uint32_t& aSize ) const //, const defs::BlockReadWriteMode& aMode )
  {
    try
    {
      if ( ( mMode == defs::SINGLE ) && ( aSize != 1 ) ) //We allow the user to call a bulk access of size=1 to a single register
      {
        log ( Error() , "Bulk Transfer requested on single register node" );
        log ( Error() , "If you were expecting an incremental read, please modify your address file to add the 'mode=",  Quote ( "incremental" ) , "' flags there" );
        BulkTransferOnSingleRegister().throwFrom ( ThisLocation() );
      }
      else
      {
        if ( ( mSize != 1 ) && ( aSize>mSize ) )
        {
          log ( Error() , "Requested bulk read of greater size than the specified endpoint size" );
          BulkTransferRequestedTooLarge().throwFrom ( ThisLocation() );
        }

        if ( mPermission & defs::READ )
        {
          return mHw->getClient().readBlock ( mAddr , aSize , mMode ); //aMode );
        }
        else
        {
          log ( Error() , "Node permissions denied read access" );
          ReadAccessDenied().throwFrom ( ThisLocation() );
        }
      }
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }

  // ValWord< int32_t > Node::readSigned()
  // {
  // try
  // {
  // if ( mPermission & defs::READ )
  // {
  // if ( mMask == defs::NOMASK )
  // {
  // return mHw->getClient().readSigned ( mAddr );
  // }
  // else
  // {
  // return mHw->getClient().readSigned ( mAddr , mMask );
  // }
  // }
  // else
  // {
  // log ( Error() , "Node permissions denied read access" );
  // ReadAccessDenied().throwFrom ( ThisLocation() );
  // }
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.rethrowFrom ( ThisLocation() );
  // }
  // catch ( const std::exception& aExc )
  // {
  // StdException ( aExc ).throwFrom ( ThisLocation() );
  // }
  // }


  // ValVector< int32_t > Node::readBlockSigned ( const uint32_t& aSize ) //, const defs::BlockReadWriteMode& aMode )
  // {
  // try
  // {
  // if ( ( mMode == defs::SINGLE ) && ( aSize != 1 ) ) //We allow the user to call a bulk access of size=1 to a single register
  // {
  // log ( Error() , "Bulk Transfer requested on single register node" );
  // log ( Error() , "If you were expecting an incremental read, please modify your address file to add the 'mode=",  Quote ( "incremental" ) , "' flags there" );
  // BulkTransferOnSingleRegister().throwFrom ( ThisLocation() );
  // }
  // else
  // {
  // if ( ( mSize != 1 ) && ( aSize>mSize ) )
  // {
  // log ( Error() , "Requested bulk read of greater size than the specified endpoint size" );
  // BulkTransferRequestedTooLarge().throwFrom ( ThisLocation() );
  // }

  // if ( mPermission & defs::READ )
  // {
  // return mHw->getClient().readBlockSigned ( mAddr , aSize , mMode ); //aMode );
  // }
  // else
  // {
  // log ( Error() , "Node permissions denied read access" );
  // ReadAccessDenied().throwFrom ( ThisLocation() );
  // }
  // }
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.rethrowFrom ( ThisLocation() );
  // }
  // catch ( const std::exception& aExc )
  // {
  // StdException ( aExc ).throwFrom ( ThisLocation() );
  // }
  // }




  // ValWord< uint32_t > Node::rmw_bits ( const uint32_t& aANDterm , const uint32_t& aORterm )
  // {
  // try
  // {
  // if ( mPermission == defs::READWRITE )
  // {
  // return mHw->getClient().rmw_bits ( mAddr , aANDterm , aORterm );
  // }
  // else
  // {
  // log ( Error() , "Node permissions denied read/write access" );
  // ReadAccessDenied().throwFrom ( ThisLocation() );
  // }
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.rethrowFrom ( ThisLocation() );
  // }
  // catch ( const std::exception& aExc )
  // {
  // StdException ( aExc ).throwFrom ( ThisLocation() );
  // }
  // }



  // ValWord< uint32_t > Node::rmw_sum ( const int32_t& aAddend )
  // {
  // try
  // {
  // if ( mPermission == defs::READWRITE )
  // {
  // return mHw->getClient().rmw_sum ( mAddr , aAddend );
  // }
  // else
  // {
  // log ( Error() , "Node permissions denied read/write access" );
  // ReadAccessDenied().throwFrom ( ThisLocation() );
  // }
  // }
  // catch ( uhal::exception& aExc )
  // {
  // aExc.rethrowFrom ( ThisLocation() );
  // }
  // catch ( const std::exception& aExc )
  // {
  // StdException ( aExc ).throwFrom ( ThisLocation() );
  // }
  // }

  ClientInterface& Node::getClient() const
  {
    try
    {
      return mHw->getClient();
    }
    catch ( uhal::exception& aExc )
    {
      aExc.rethrowFrom ( ThisLocation() );
    }
    catch ( const std::exception& aExc )
    {
      StdException ( aExc ).throwFrom ( ThisLocation() );
    }
  }

}
