#ifndef _uhal_Node_hpp_
#define _uhal_Node_hpp_

#include "uhal/exception.hpp"
#include "uhal/definitions.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/Utilities.hpp"

#include <boost/spirit/include/qi.hpp>
#include <boost/regex.hpp>

#include <exception>
#include <vector>
#include <string>
#include <sstream>

#include "pugixml/pugixml.hpp"


namespace uhal
{
	class HwInterface;

	class WriteAccessDenied: public uhal::exception {  };
	class ReadAccessDenied: public uhal::exception {  };
	class NodeMustHaveUID: public uhal::exception {  };
	class NoBranchFoundWithGivenUID: public uhal::exception {  };


	class Node
	{
			friend class HwInterface;

			friend std::ostream& operator<< ( std::ostream& aStream , const Node& aNode )
			{
				try
				{
					aNode.stream ( aStream );
					return aStream;
				}
				catch ( const std::exception& aExc )
				{
					pantheios::log_EXCEPTION ( aExc );
					throw uhal::exception ( aExc );
				}
			}


		public:

			virtual ~Node();

			bool operator == ( const Node& aNode );


			/**
			 * Retrieve node with the relative id. If the node does not exist throws
			 */
			Node& getNode ( const std::string& aId );

			std::vector<std::string> getNodes();

			std::vector<std::string> getNodes ( const boost::regex& aRegex );
			std::vector<std::string> getNodes ( const char* aRegex );
			std::vector<std::string> getNodes ( const std::string& aRegex );


			std::string getId() const;

			uint32_t getAddress() const;

			uint32_t getMask() const;

			defs::NodePermission getPermission() const;

			void stream ( std::ostream& aStream , std::size_t indent = 0 ) const;



			/**
			 * Queues the corresponding operation. Id the permissions are insuficient or the node is not an end node, then it throws
			 */
			void write ( const uint32_t& aValue );
			void writeBlock ( const std::vector< uint32_t >& aValues , const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );

			ValWord< uint32_t > read();
			ValVector< uint32_t > readBlock ( const uint32_t& aSize, const defs::BlockReadWriteMode& aMode=defs::INCREMENTAL );



			/*ValWord< int32_t > readSigned ( );
			ValWord< int32_t > readSigned ( const uint32_t& aMask );
			ValVector< int32_t > readBlockSigned ( const uint32_t& size, const defs::BlockReadWriteMode aMode=defs::INCREMENTAL );

			ValWord< uint32_t > rmw_bits ( const uint32_t& aANDterm , const uint32_t& aORterm );

			ValWord< int32_t > rmw_sum ( const int32_t& aAddend );*/


			//private:
			//Node ( HwInterface* aHwInterface, const std::string& aFullid );
			Node ( const pugi::xml_node& aXmlNode );



		private:
			HwInterface* mHw;
			// std::string mFullId;
			std::string mUid;
			uint32_t mAddr;
			uint32_t mMask;
			defs::NodePermission mPermission;

			boost::shared_ptr< std::vector< Node > > mChildren;
			boost::shared_ptr< std::hash_map< std::string , Node* > > mChildrenMap;	//ok as long as the member mChildren cannot be modified after this is constructed, otherwise reallocation can cause a problem


			static const struct permissions_lut : boost::spirit::qi::symbols<char, defs::NodePermission>
			{
				permissions_lut();
			} mPermissionsLut;

	};

}

#endif
