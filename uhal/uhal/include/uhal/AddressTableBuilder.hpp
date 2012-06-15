/**
	@file
	@author Andrew W. Rose
	@author Marc Magrans De Abril
	@date 2012
*/

#ifndef _uhal_AddressTableBuilder_hpp_
#define _uhal_AddressTableBuilder_hpp_

#include "uhal/exception.hpp"
#include "uhal/definitions.hpp"
#include "uhal/Node.hpp"

#include <boost/utility.hpp>
#include <boost/filesystem.hpp>

#include "pugixml/pugixml.hpp"

#include <map>

namespace uhal
{
	//! Exception class to handle the case where too many or two few address files are specified. Uses the base uhal::exception implementation of what()
	class IncorrectAddressTableFileCount: public uhal::exception {  };
	//! Exception class to handle the case where the address file failed to open. Uses the base uhal::exception implementation of what()
	class FailedToOpenAddressTableFile: public uhal::exception {  };

	//! A class to build a node tree from an Address table file
	class AddressTableBuilder: private boost::noncopyable
	{
		private:
			/**
				Default constructor
				This is private since only a single instance is to be created, using the getInstance method
			*/
			AddressTableBuilder () {}

			/**
				Destructor
			*/
			virtual ~AddressTableBuilder () {}


		public:
			/**
				Static method to retrieve the single instance of the class
				@return the single instance of the class
			*/
			static AddressTableBuilder& getInstance();

			/**
				Construct a node tree from file whose name is specified
				@param aFilenameExpr a Filename Expression
				@return a shared_ptr to a const node tree, such that which must be copied by the final user
			*/
			boost::shared_ptr< const Node > getAddressTable ( const std::string& aFilenameExpr , const uint32_t& aAddr = 0x00000000 , const uint32_t& aAddrMask = 0xFFFFFFFF );

			/**
				Method called once the file specified in the call to getAddressTable( aFilenameExpr ) has been opened
				@param aProtocol The protocol by which the file was loaded
				@param aPath The fully qualified path to the file which has been opened
				@param aFile A byte vector containing the content of the opened file. Done like this since the routine handles local and http files identically
				@param aAddressTable The address table constructed from the file
			*/
			void CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile , const uint32_t& aAddr , const uint32_t& aAddrMask , std::vector< boost::shared_ptr< const Node > >& aAddressTable );

		private:
			//! The single instance of the class
			static AddressTableBuilder* mInstance;
			//! Hash map associating a Node tree with a file name
			std::hash_map< std::string , boost::shared_ptr< const Node > > mNodes;

	};
}

#endif
