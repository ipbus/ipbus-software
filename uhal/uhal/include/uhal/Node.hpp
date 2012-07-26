/**
	@file
	@author Andrew W. Rose
	@author Marc Magrans De Abril
	@date 2012
*/

#ifndef _uhal_Node_hpp_
#define _uhal_Node_hpp_

#include "uhal/exception.hpp"
#include "uhal/definitions.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/Utilities.hpp"
#include "uhal/ClientInterface.hpp"

#include <boost/spirit/include/qi.hpp>
#include <boost/regex.hpp>
#include <boost/shared_ptr.hpp>

#include <exception>
#include <vector>
#include <map>
#include <string>
#include <sstream>

#include "pugixml/pugixml.hpp"

// forward declaration so that we can declare friends
namespace uhal
{
	class Node;
}


namespace uhal
{
	class HwInterface;
	class NodeTreeBuilder;

	//! Exception class to handle the case where a write was performed on a register which does not allow write access. Uses the base uhal::exception implementation of what()
	class WriteAccessDenied: public uhal::_exception< WriteAccessDenied > {  };
	//! Exception class to handle the case where a read was performed on a register which does not allow read access. Uses the base uhal::exception implementation of what()
	class ReadAccessDenied: public uhal::_exception< ReadAccessDenied > {  };
	//! Exception class to handle the case where creation of a node was attempted without it having a UID. Uses the base uhal::exception implementation of what()
	class NodeMustHaveUID: public uhal::_exception< NodeMustHaveUID > {  };
	//! Exception class to handle the case where a child ID was requested which does not exist. Uses the base uhal::exception implementation of what()
	class NoBranchFoundWithGivenUID: public uhal::_exception< NoBranchFoundWithGivenUID > {  };
	//! Exception class to handle the case where a child node has an address which overlaps with the parent. Uses the base uhal::exception implementation of what()
	class ChildHasAddressOverlap: public uhal::_exception< ChildHasAddressOverlap > {  };
	//! Exception class to handle the case where a child node has an address mask which overlaps with the parent. Uses the base uhal::exception implementation of what()
	class ChildHasAddressMaskOverlap: public uhal::_exception< ChildHasAddressMaskOverlap > {  };
	//! Exception class to handle the case where a bulk read or write was performed on a single register. Uses the base uhal::exception implementation of what()
	class BulkTransferOnSingleRegister: public uhal::_exception< BulkTransferOnSingleRegister > {  };
	//! Exception class to handle the case where requested bulk read or write was too large. Uses the base uhal::exception implementation of what()
	class BulkTransferRequestedTooLarge: public uhal::_exception< BulkTransferRequestedTooLarge > {  };
	//! Exception class to handle the case where an incremental node is specified without a size attribute. Uses the base uhal::exception implementation of what()
	class IncrementalNodeRequiresSizeAttribute: public uhal::_exception< IncrementalNodeRequiresSizeAttribute > {  };

	//! Exception class to handle the case where a memory block has a size which would exceed the available register space. Uses the base uhal::exception implementation of what()
	class ArraySizeExceedsRegisterBound: public uhal::_exception< ArraySizeExceedsRegisterBound > {  };
	//! Exception class to handle the case where two addresses overlap. Uses the base uhal::exception implementation of what()
	class AddressSpaceOverlap: public uhal::_exception< AddressSpaceOverlap > {  };



	//! A heirarchical node for navigating heirarchical firmwares
	class Node
	{
			friend class HwInterface;
			friend class NodeTreeBuilder;

		private:
			/**
				Empty node
			*/
			Node ( );


		public:

			/**
				Construct a node from a PugiXML node
				@param aXmlNode a PugiXML node from which to construct a node
				@param aPath The fully qualified path to the XML file containing this node
				@param aRequireId specify whether an exception should be thrown if the id attribute is not set
			*/
			Node ( const pugi::xml_node& aXmlNode , const boost::filesystem::path& aPath , const bool& aRequireId = true );

			/**
				Lightweight Copy constructor
				@param aNode a node to lightweight copy.
				@note this copy constructor copies the member pointers, not the object they are pointing to. To duplicate the underlying object, rather than the the pointer to it, use the clone() method.
			*/
			Node ( const Node& aNode );

			/**
				Destructor
			*/
			virtual ~Node();

		private:

			/**
				Function to create a true clone of the full hierarchical Node tree
				@return a true clone of the current node tree
			*/
			virtual Node clone() const;

		public:
			/**
				A function to determine whether two Nodes are identical
				@param aNode a Node to compare
				@return whether two Nodes are identical
			*/
			bool operator == ( const Node& aNode );


			/**
				Retrieve the Node given by a full-stop delimeted name path relative, to the current node
				@param aId a full-stop delimeted name path to a node, relative to the current node
				@return the Node given by the identifier
			*/
			Node& getNode ( const std::string& aId );


			/**
				Retrieve the Node given by a full-stop delimeted name path relative, to the current node and cast it to a particular node type
				@param aId a full-stop delimeted name path to a node, relative to the current node
				@return the Node given by the identifier
			*/
			template< typename T>
			T& getNode ( const std::string& aId );


			/**
				Return all node IDs known to this HwInterface
				@return all node IDs known to this HwInterface
			*/
			std::vector<std::string> getNodes();


			/**
				Return all node IDs known to this connection manager which match a (boost) regular expression
				@param aRegex a string expression which is converted to a (boost) regular expression against which the node IDs are tested
				@return all node IDs known to this connection manager
			*/
			std::vector<std::string> getNodes ( const std::string& aRegex );


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
				Return the optional tags string which the user can specify the current node
				@return the optional tags string which the user can specify the current node
			*/
			const std::string& getTags() const;

			/**
				A streaming helper function to create pretty, indented tree diagrams
				@param aStream a stream to write to
				@param aIndent size of the indentation
			*/
			void stream ( std::ostream& aStream , std::size_t aIndent = 0 ) const;


			/**
				Write a single, unmasked word to a register
				@param aValue the value to write to the register
			*/
			ValHeader write ( const uint32_t& aValue );

			/**
				Write a block of data to a block of registers or a block-write port
				@param aValues the values to write to the registers or a block-write port
			*/
			ValHeader writeBlock ( const std::vector< uint32_t >& aValues );

			/**
				DEPRICATED! Write a block of data to a block of registers or a block-write port
				@param aValues the values to write to the registers or a block-write port
				@param aMode whether we are writing to a block of registers (INCREMENTAL) or a block-write port (NON_INCREMENTAL)
				@warning DEPRICATED and will be removed in the next release!
			*/
			ValHeader writeBlock ( const std::vector< uint32_t >& aValues , const defs::BlockReadWriteMode& aMode )
			{
				log ( Error() , "THIS METHOD IS DEPRECATED! "
					  "PLEASE MODIFY YOUR ADDRESS FILE TO ADD THE INCREMENTAL/NON_INCREMENTAL FLAGS THERE "
					  "AND CHANGE THE FUNCTION CALL TO writeBlock ( const std::vector< uint32_t >& aValues ). "
					  "I WILL ATTEMPT A HACK TO CALL THIS FUNCTION BUT BE WARNED. "
					  "THIS METHOD WILL BE REMOVED IN THE NEXT RELEASE!" );
				defs::BlockReadWriteMode lMode ( mMode );
				mMode = aMode;
				ValHeader lReply ( writeBlock ( aValues ) );
				mMode = lMode;
				return lReply;
			}

			/**
				Read a single, unmasked, unsigned word
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			ValWord< uint32_t > read ( );

			/**
				Read a block of unsigned data from a block of registers or a block-read port
				@param aSize the number of words to read
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			ValVector< uint32_t > readBlock ( const uint32_t& aSize );

			/**
				DEPRICATED! Read a block of unsigned data from a block of registers or a block-read port
				@param aSize the number of words to read
				@param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
				@return a Validated Memory which wraps the location to which the reply data is to be written
				@warning DEPRICATED and will be removed in the next release!
			*/
			ValVector< uint32_t > readBlock ( const uint32_t& aSize , const defs::BlockReadWriteMode& aMode )
			{
				log ( Error() , "THIS METHOD IS DEPRECATED! "
					  "PLEASE MODIFY YOUR ADDRESS FILE TO ADD THE INCREMENTAL/NON_INCREMENTAL FLAGS THERE "
					  "AND CHANGE THE FUNCTION CALL TO readBlock ( const uint32_t& aSize ). "
					  "I WILL ATTEMPT A HACK TO CALL THIS FUNCTION BUT BE WARNED. "
					  "THIS METHOD WILL BE REMOVED IN THE NEXT RELEASE!" );
				defs::BlockReadWriteMode lMode ( mMode );
				mMode = aMode;
				ValVector< uint32_t > lRet ( readBlock ( aSize ) );
				mMode = lMode;
				return lRet;
			}

			// /**
			// Read a single, unmasked word and interpret it as being signed
			// @return a Validated Memory which wraps the location to which the reply data is to be written
			// */
			// ValWord< int32_t > readSigned ( );

			// /**
			// Read a block of data from a block of registers or a block-read port and interpret it as being signed data
			// @param aSize the number of words to read
			// @return a Validated Memory which wraps the location to which the reply data is to be written
			// */
			// ValVector< int32_t > readBlockSigned ( const uint32_t& aSize );


			// /**
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


			/**
				Read the value of a register, apply the AND-term, apply the OR-term, set the register to this new value and return a copy of the new value to the user
				@param aANDterm the AND-term to apply to existing value in the target register
				@param aORterm the OR-term to apply to existing value in the target register
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			ValWord< uint32_t > rmw_bits ( const uint32_t& aANDterm , const uint32_t& aORterm );

			/**
				Read the value of a register, add the addend, set the register to this new value and return a copy of the new value to the user
				@param aAddend the addend to add to the existing value in the target register
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			ValWord< uint32_t > rmw_sum ( const int32_t& aAddend );


			/**
				Get the underlying IPbus client
				@return the IPbus client that will be used to issue a dispatch
			*/
			boost::shared_ptr<ClientInterface> getClient();


		private:

			/**
				Propagate the partial addresses down through the hierarchical structure and make a record of all used addresses for collision detection
				@param aAddr the full address of the current branch which will be applied to the current children
				@param aTopLevelNode the top-level node which contains the hash-map of all known children, against which we will check the address for overlaps
			*/
			void calculateHierarchicalAddresses ( const uint32_t& aAddr , const Node& aTopLevelNode );

			//! The parent hardware interface of which this node is a child (or rather decendent)
			HwInterface* mHw;
			// std::string mFullId;
			//! The Unique ID of this node
			std::string mUid;

			//! The register address with which this node is associated
			uint32_t mAddr;
			//! Mark whether the address is fully formed
			bool mAddrValid;

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

			//! The direct children of the child node
			boost::shared_ptr< std::deque< Node > > mChildren;

			//! Helper to assist look-up of a particular child node, given a name
			boost::shared_ptr< std::hash_map< std::string , Node* > > mChildrenMap;

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

#include "uhal/TemplateDefinitions/Node.hxx"

#endif
