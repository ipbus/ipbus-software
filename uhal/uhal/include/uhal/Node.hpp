#ifndef _uhal_Node_hpp_
#define _uhal_Node_hpp_

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

	class WriteAccessDenied: public std::exception {  };
	class ReadAccessDenied: public std::exception {  };
	class NodeMustHaveUID: public std::exception {  };
	class NoBranchFoundWithGivenUID: public std::exception {  };
	
	
	class Node
	{
		friend class HwInterface;
		
		friend std::ostream& operator<< ( std::ostream& aStream , const Node& aNode ){
			aNode.stream( aStream );
			return aStream;
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

			void stream( std::ostream& aStream , std::size_t indent = 0 ) const;
						
			
			/**
			 * Queues the corresponding operation. Id the permissions are insuficient or the node is not an end node, then it throws
			 */
/*			template< typename T >
			void writeBlock ( const std::vector< T >& aValues , const defs::BlockReadWriteMode aMode=defs::INCREMENTAL )
			{
				if ( mPermission & defs::WRITE ) {
					mHw->getClient().writeBlock< T >( mAddr , aValues , aMode );
				} else {
					throw WriteAccessDenied();
				}
			}
	
			template< typename T >
			void write ( const T& aValue )
			{
				if ( mPermission & defs::WRITE ) {
					if ( mMask == defs::NOMASK ) {
						mHw->getClient().write< T >( mAddr , aValue );
					} else {
						mHw->getClient().write< T >( mAddr , aValue , mMask );
					}
				} else {
					throw WriteAccessDenied();
				}
			}
			
			template< typename T >
			ValVector<T> readBlock ( const uint32_t& aSize, const defs::BlockReadWriteMode aMode=defs::INCREMENTAL )
			{
				if ( mPermission & defs::READ ) {
					return mHw->getClient().readBlock< T >( aSize , aMode );
				} else {
					throw ReadAccessDenied();
				}
			}			

			template< typename T >
			ValWord<T> read()
			{
				if ( mPermission & defs::READ ) {
					if ( mMask == defs::NOMASK ) {
						return mHw->getClient().read< T >( mAddr );
					} else {
						return mHw->getClient().read< T >( mAddr , mMask );
					}
				} else {
					throw ReadAccessDenied();
				}
			}		*/	
			
		//private:
			//Node ( HwInterface* aHwInterface, const std::string& aFullid );
			Node( const pugi::xml_node& aXmlNode  );
			
			
			
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
