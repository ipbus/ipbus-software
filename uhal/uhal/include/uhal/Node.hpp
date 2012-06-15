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
#include <string>
#include <sstream>

#include "pugixml/pugixml.hpp"

// forward declaration so that we can declare friends
namespace uhal
{
	class Node;
}

/**
	A streaming operator to format the node for display
	@param aStream a stream to output the time onto
	@param aNode the node to be displayed
	@return a stream for further appending
*/
std::ostream& operator<< ( std::ostream& aStream , const uhal::Node& aNode );


namespace uhal
{
	class HwInterface;

	//! Exception class to handle the case where a write was performed on a register which does not allow write access. Uses the base uhal::exception implementation of what()
	class WriteAccessDenied: public uhal::exception {  };
	//! Exception class to handle the case where a read was performed on a register which does not allow read access. Uses the base uhal::exception implementation of what()
	class ReadAccessDenied: public uhal::exception {  };
	//! Exception class to handle the case where creation of a node was attempted without it having a UID. Uses the base uhal::exception implementation of what()
	class NodeMustHaveUID: public uhal::exception {  };
	//! Exception class to handle the case where a child ID was requested which does not exist. Uses the base uhal::exception implementation of what()
	class NoBranchFoundWithGivenUID: public uhal::exception {  };
	//! Exception class to handle the case where a child node has an address which overlaps with the parent. Uses the base uhal::exception implementation of what()
	class ChildHasAddressOverlap: public uhal::exception {  };
	//! Exception class to handle the case where a child node has an address mask which overlaps with the parent. Uses the base uhal::exception implementation of what()
	class ChildHasAddressMaskOverlap: public uhal::exception {  };
	//! Exception class to handle the case where a bulk read or write was performed on a single register. Uses the base uhal::exception implementation of what()
	class BulkTransferOnSingleRegister: public uhal::exception {  };


	//! A heirarchical node for navigating heirarchical firmwares
	class Node
	{
			friend class HwInterface;

			/**
				A streaming operator to format the node for display
				@param aStream a stream to output the time onto
				@param aNode the node to be displayed
				@return a stream for further appending
			*/
			friend std::ostream& ( ::operator<< ) ( std::ostream& aStream , const Node& aNode );


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
			bool operator == ( const Node& aNode );


			/**
				Retrieve the Node given by a full-stop delimeted name path relative, to the current node
				@param aId a full-stop delimeted name path to a node, relative to the current node
				@return the Node given by the identifier
			*/
			Node& getNode ( const std::string& aId );

			/**
				Return all node IDs known to this HwInterface
				@return all node IDs known to this HwInterface
			*/
			std::vector<std::string> getNodes();

			/**
				Return all node IDs known to this connection manager which match a (boost) regular expression
				@param aRegex a (boost) regular expression against which the node IDs are tested
				@return all node IDs known to this connection manager
			*/
			std::vector<std::string> getNodes ( const boost::regex& aRegex );
			/**
				Return all node IDs known to this connection manager which match a (boost) regular expression
				@param aRegex a const char* expression which is converted to a (boost) regular expression against which the node IDs are tested
				@return all node IDs known to this connection manager
			*/
			std::vector<std::string> getNodes ( const char* aRegex );
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
			std::string getId() const;

			/**
				Return the register address with which this node is associated
				@return the register address with which this node is associated
			*/
			uint32_t getAddress() const;

			/**
				Return the mask to be applied if this node is a sub-field, rather than an entire register
				@return the mask to be applied if this node is a sub-field, rather than an entire register
			*/
			uint32_t getMask() const;

			/**
				Return the read/write access permissions of this node
				@return the read/write access permissions of this node
			*/
			defs::NodePermission getPermission() const;

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
			void write ( const uint32_t& aValue );

			/**
				Write a block of data to a block of registers or a block-write port
				@param aValues the values to write to the registers or a block-write port
			*/
			//@param aMode whether we are writing to a block of registers (INCREMENTAL) or a block-write port (NON_INCREMENTAL)
			void writeBlock ( const std::vector< uint32_t >& aValues ); //, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

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
			//@param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
			ValVector< uint32_t > readBlock ( const uint32_t& aSize ); //, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

			/**
				Read a single, unmasked word and interpret it as being signed
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			ValWord< int32_t > readSigned ( );

			/**
				Read a block of data from a block of registers or a block-read port and interpret it as being signed data
				@param aSize the number of words to read
				@return a Validated Memory which wraps the location to which the reply data is to be written
			*/
			//@param aMode whether we are reading from a block of registers (INCREMENTAL) or a block-read port (NON_INCREMENTAL)
			ValVector< int32_t > readBlockSigned ( const uint32_t& aSize ); //, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

			
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
			ValWord< int32_t > rmw_sum ( const int32_t& aAddend );
			

			/**
				Get the underlying IPbus client
				@return the IPbus client that will be used to issue a dispatch
			*/
			boost::shared_ptr<ClientInterface> getClient();


			//private:
			//Node ( HwInterface* aHwInterface, const std::string& aFullid );

			/**
				Construct a node from a PugiXML node
				@param aXmlNode a PugiXML node from which to construct a node
			*/
			Node ( const pugi::xml_node& aXmlNode , const uint32_t& aParentAddr = 0x00000000 , const uint32_t& aParentMask = 0xFFFFFFFF );

			/**
				Copy constructor
				Need to clone the children, not copy the pointer!
				@param aNode a node to copy
			*/
			Node ( const Node& aNode );

		private:
			//! The parent hardware interface of which this node is a child (or rather decendent)
			HwInterface* mHw;
			// std::string mFullId;
			//! The Unique ID of this node
			std::string mUid;
			//! The register address with which this node is associated
			uint32_t mAddr;

			//! The register address with which this node is associated
			uint32_t mAddrMask;

			//! The mask to be applied if this node is a sub-field, rather than an entire register
			uint32_t mMask;
			//! The read/write access permissions of this node
			defs::NodePermission mPermission;
			
			//! Whether the node represents a single register, a block of registers or a block-read/write port
			defs::BlockReadWriteMode mMode;
			
			//! The children of the current node
			boost::shared_ptr< std::vector< Node > > mChildren;
			//! Helper to assist look-up of a particular child node, given a name
			boost::shared_ptr< std::hash_map< std::string , Node* > > mChildrenMap;	//ok as long as the member mChildren cannot be modified after this is constructed, otherwise reallocation can cause a problem

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

#endif
