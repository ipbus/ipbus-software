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

#ifndef _uhal_Node_hpp_
#define _uhal_Node_hpp_

#include "uhal/log/exception.hpp"
#include "uhal/definitions.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/Utilities.hpp"
#include "uhal/ClientInterface.hpp"

#include <exception>
#include <utility>
#include <vector>
#include <map>
#include <string>
#include <sstream>

#include "pugixml/pugixml.hpp"



namespace uhal
{
  class HwInterface;
  class NodeTreeBuilder;

  namespace exception
  {
    //! Exception class to handle the case where a write was performed on a register which does not allow write access.
    ExceptionClass ( WriteAccessDenied , "Exception class to handle the case where a write was performed on a register which does not allow write access." );
    //! Exception class to handle the case where a read was performed on a register which does not allow read access.
    ExceptionClass ( ReadAccessDenied , "Exception class to handle the case where a read was performed on a register which does not allow read access." );
    //! Exception class to handle the case where a child ID was requested which does not exist.
    ExceptionClass ( NoBranchFoundWithGivenUID , "Exception class to handle the case where a child ID was requested which does not exist." );
    //! Exception class to handle the case where a bulk read or write was performed on a single register.
    ExceptionClass ( BulkTransferOnSingleRegister , "Exception class to handle the case where a bulk read or write was performed on a single register." );
    //! Exception class to handle the case where requested bulk read or write was too large.
    ExceptionClass ( BulkTransferRequestedTooLarge , "Exception class to handle the case where requested bulk read or write was too large." );
    //! Exception class to handle the case of an attempt to cast a node to the wrong type.
    ExceptionClass ( BadNodeCast , "Exception class to handle the case of an attempt to cast a node to the wrong type." );
  }


  //! A heirarchical node for navigating heirarchical firmwares
  class Node
  {
      friend class HwInterface;
      friend class NodeTreeBuilder;

    protected:
      /**
      	Empty node
      */
      Node ( );

      /**
      	Copy constructor
      	@param aNode a node to copy.
      */
      Node ( const Node& aNode );

      /**
      	Assignment operator
      	@param aNode a Node to copy
      	@return reference to this object for chained assignment
      */
      virtual Node& operator= ( const Node& aNode );

      /**
      	Function to produce a new copy of the current Node
      	@return a new copy of the current Node
      */
      virtual Node* clone() const;

    public:
      /**
      	Destructor
      */
      virtual ~Node();

      /**
      	A function to determine whether two Nodes are identical
      	@param aNode a Node to compare
      	@return whether two Nodes are identical
      */
      bool operator == ( const Node& aNode ) const;


      /**
      	Retrieve the Node given by a full-stop delimeted name path relative, to the current node
      	@param aId a full-stop delimeted name path to a node, relative to the current node
      	@return the Node given by the identifier
      */
      const Node& getNode ( const std::string& aId ) const;


      /**
      	Retrieve the Node given by a full-stop delimeted name path relative, to the current node and cast it to a particular node type
      	@param aId a full-stop delimeted name path to a node, relative to the current node
      	@return the Node given by the identifier
      */
      template< typename T>
      const T& getNode ( const std::string& aId ) const;


      /**
      	Return all node IDs known to this HwInterface
      	@return all node IDs known to this HwInterface
      */
      std::vector<std::string> getNodes() const;


      /**
      	Return all node IDs known to this connection manager which match a (boost) regular expression
      	@param aRegex a string expression which is converted to a (boost) regular expression against which the node IDs are tested
      	@return all node IDs known to this connection manager
      */
      std::vector<std::string> getNodes ( const std::string& aRegex ) const;


      /**
      	Return the unique ID of the current node
      	@return the unique ID of the current node
      */
      const std::string& getId() const;

      /**
      	Return the register address with which this node is associated
      	@return the register address with which this node is associated
      */
      const uint32_t& getAddress() const;

      /**
      	Return the mask to be applied if this node is a sub-field, rather than an entire register
      	@return the mask to be applied if this node is a sub-field, rather than an entire register
      */
      const uint32_t& getMask() const;

      /**
      	Return whether the node represents a single register, a block of registers or a block-read/write port
      	@return whether the node represents a single register, a block of registers or a block-read/write port
      */
      const defs::BlockReadWriteMode& getMode() const;

      /**
      	Return the maximum size available to a block read/write
      	@return the maximum size available to a block read/write
      */
      const uint32_t& getSize() const;

      /**
      	Return the read/write access permissions of this node
      	@return the read/write access permissions of this node
      */
      const defs::NodePermission& getPermission() const;

      /**
      	Return the optional tags string which the user can specify for the current node
      	@return the optional tags string which the user can specify for the current node
      */
      const std::string& getTags() const;

      /**
      	Return the optional description string which the user can specify for the current node
      	@return the optional description string which the user can specify for the current node
      */
      const std::string& getDescription() const;

      /**
        Return the name of the module in which the current node resides
        @return the name of the module in which the current node resides
      */
      const std::string& getModule() const;

      /**
        Return the parameters of the current node
        @return the parameters of the current node
      */
      const boost::unordered_map< std::string, std::string >& getParameters() const;
      
      /**
      	A streaming helper function to create pretty, indented tree diagrams
      	@param aStr a stream to write to
      	@param aIndent size of the indentation
      */
      void stream ( std::ostream& aStr , std::size_t aIndent = 0 ) const;


      /**
      	Write a single, unmasked word to a register
      	@param aValue the value to write to the register
        @return a Validated Header which will contain the returned IPbus header
      */
      ValHeader write ( const uint32_t& aValue ) const;

      /**
      	Write a block of data to a block of registers or a block-write port
      	@param aValues the values to write to the registers or a block-write port
        @return a Validated Header which will contain the returned IPbus header
      */
      ValHeader writeBlock ( const std::vector< uint32_t >& aValues ) const;

      // /**
      // DEPRICATED! Write a block of data to a block of registers or a block-write port
      // @param aValues the values to write to the registers or a block-write port
      // @param aMode whether we are writing to a block of registers (INCREMENTAL) or a block-write port (NON_INCREMENTAL)
      // @return a Validated Header which will contain the returned IPbus header
      // @warning DEPRICATED and will be removed in the next release!
      // **/
      // ValHeader writeBlock ( const std::vector< uint32_t >& aValues , const defs::BlockReadWriteMode& aMode )
      // {
      //   log ( Error() , "THIS METHOD IS DEPRECATED! "
      //         "PLEASE MODIFY YOUR ADDRESS FILE TO ADD THE INCREMENTAL/NON_INCREMENTAL FLAGS THERE "
      //         "AND CHANGE THE FUNCTION CALL TO writeBlock ( const std::vector< uint32_t >& aValues ). "
      //         "I WILL ATTEMPT A HACK TO CALL THIS FUNCTION BUT BE WARNED. "
      //         "THIS METHOD WILL BE REMOVED IN THE NEXT RELEASE!" );
      //   defs::BlockReadWriteMode lMode ( mMode );
      //   mMode = aMode;
      //   ValHeader lReply ( writeBlock ( aValues ) );
      //   mMode = lMode;
      //   return lReply;
      // }


      /**
      	Read a single, unmasked, unsigned word
      	@return a Validated Memory which wraps the location to which the reply data is to be written
      */
      ValWord< uint32_t > read ( ) const;

      /**
      	Read a block of unsigned data from a block of registers or a block-read port
      	@param aSize the number of words to read
      	@return a Validated Memory which wraps the location to which the reply data is to be written
      */
      ValVector< uint32_t > readBlock ( const uint32_t& aSize ) const;

      // /**
      //	DEPRICATED! Read a block of unsigned data from a block of registers or a block-read port
      //	@param aSize the number of words to read
      //	@param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
      //	@return a Validated Memory which wraps the location to which the reply data is to be written
      // @warning DEPRICATED and will be removed in the next release!
      // 
      // ValVector< uint32_t > readBlock ( const uint32_t& aSize , const defs::BlockReadWriteMode& aMode )
      // {
      //   log ( Error() , "THIS METHOD IS DEPRECATED! "
      //         "PLEASE MODIFY YOUR ADDRESS FILE TO ADD THE INCREMENTAL/NON_INCREMENTAL FLAGS THERE "
      //         "AND CHANGE THE FUNCTION CALL TO readBlock ( const uint32_t& aSize ). "
      //         "I WILL ATTEMPT A HACK TO CALL THIS FUNCTION BUT BE WARNED. "
      //         "THIS METHOD WILL BE REMOVED IN THE NEXT RELEASE!" );
      //   defs::BlockReadWriteMode lMode ( mMode );
      //   mMode = aMode;
      //   ValVector< uint32_t > lRet ( readBlock ( aSize ) );
      //   mMode = lMode;
      //   return lRet;
      // }

      // /*
      // Read a single, unmasked word and interpret it as being signed
      // @return a Validated Memory which wraps the location to which the reply data is to be written
      // */
      // ValWord< int32_t > readSigned ( );

      // /*
      // Read a block of data from a block of registers or a block-read port and interpret it as being signed data
      // @param aSize the number of words to read
      // @return a Validated Memory which wraps the location to which the reply data is to be written
      // */
      // ValVector< int32_t > readBlockSigned ( const uint32_t& aSize );


      // /*
      // DEPRICATED! Read a block of data from a block of registers or a block-read port and interpret it as being signed data
      // @param aSize the number of words to read
      // @return a Validated Memory which wraps the location to which the reply data is to be written
      // @param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
      // @warning DEPRICATED and will be removed in the next release!
      // */
      // ValVector< int32_t > readBlockSigned ( const uint32_t& aSize , const defs::BlockReadWriteMode& aMode )
      // {
      // log ( Error() , "THIS METHOD IS DEPRECATED! "
      // "PLEASE MODIFY YOUR ADDRESS FILE TO ADD THE INCREMENTAL/NON_INCREMENTAL FLAGS THERE "
      // "AND CHANGE THE FUNCTION CALL TO readBlockSigned ( const uint32_t& aSize ). "
      // "I WILL ATTEMPT A HACK TO CALL THIS FUNCTION BUT BE WARNED. "
      // "THIS METHOD WILL BE REMOVED IN THE NEXT RELEASE!" );
      // defs::BlockReadWriteMode lMode ( mMode );
      // mMode = aMode;
      // ValVector< int32_t > lRet ( readBlockSigned ( aSize ) );
      // mMode = lMode;
      // return lRet;
      // }


      // /*
      // Read the value of a register, apply the AND-term, apply the OR-term, set the register to this new value and return a copy of the new value to the user
      // @param aANDterm the AND-term to apply to existing value in the target register
      // @param aORterm the OR-term to apply to existing value in the target register
      // @return a Validated Memory which wraps the location to which the reply data is to be written
      // */
      // ValWord< uint32_t > rmw_bits ( const uint32_t& aANDterm , const uint32_t& aORterm );

      // /*
      // Read the value of a register, add the addend, set the register to this new value and return a copy of the new value to the user
      // @param aAddend the addend to add to the existing value in the target register
      // @return a Validated Memory which wraps the location to which the reply data is to be written
      // */
      // ValWord< uint32_t > rmw_sum ( const int32_t& aAddend );


      /**
      	Get the underlying IPbus client
      	@return the IPbus client that will be used to issue a dispatch
      */
      ClientInterface& getClient() const;


    private:

      //! The parent hardware interface of which this node is a child (or rather decendent)
      HwInterface* mHw;

      //! The Unique ID of this node
      std::string mUid;

      //! The register address with which this node is associated
      uint32_t mPartialAddr;
      //! The register address with which this node is associated
      uint32_t mAddr;

      //! The mask to be applied if this node is a sub-field, rather than an entire register
      uint32_t mMask;
      //! The read/write access permissions of this node
      defs::NodePermission mPermission;
      //! Whether the node represents a single register, a block of registers or a block-read/write port
      defs::BlockReadWriteMode mMode;
      //! The maximum size available to a block read/write
      uint32_t mSize;

      //! Optional string which the user can specify
      std::string mTags;

      //! Optional string which the user can specify
      std::string mDescription;

      //! The name of the module in which the current node resides
      std::string mModule;

      //! Additional parameters of the node
      boost::unordered_map< std::string, std::string > mParameters;
      
      //! The direct children of the node
      std::deque< Node* > mChildren;

      //! Helper to assist look-up of a particular child node, given a name
      boost::unordered_map< std::string , Node* > mChildrenMap;



  };

}

std::ostream& operator<< ( std::ostream& aStr ,  const uhal::Node& aNode );

#include "uhal/TemplateDefinitions/Node.hxx"

#endif
