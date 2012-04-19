/**
	@file
	@author Andrew W. Rose
	@author Marc Magrans De Abril
	@date 2012
*/

#ifndef _uhal_ConnectionManager_hpp_
#define _uhal_ConnectionManager_hpp_

#include "uhal/exception.hpp"
#include "uhal/HwInterface.hpp"
#include "uhal/Utilities.hpp"

#include <boost/filesystem.hpp>
#include <boost/regex.hpp>

#include "pugixml/pugixml.hpp"

#include <vector>
#include <set>
#include <map>

namespace uhal
{
	//! Exception class to handle the case where the supposedly unique ID is duplicated. Uses the base uhal::exception implementation of what()
	class DuplicatedUID: public uhal::exception {  };
	//! Exception class to handle the case where the UID requested does not exists in the map of connections. Uses the base uhal::exception implementation of what()
	class ConnectionUIDDoesNotExist: public uhal::exception {  };

	//! A class to open and manage XML connection files and wrap up the interfaces to the AddressTableBuilder and the ClientFactory
	class ConnectionManager: private boost::noncopyable
	{
		public:
			//! A struct to hold the fields of each entry in the XML connections file
			struct ConnectionDescriptor
			{
				/**
					Constructor
					@param aNode a PugiXML node from which to extract the details of the connection
					@param aSuccess return whether all the necessary values were extracted from the PugiXML node
				*/
				ConnectionDescriptor ( const pugi::xml_node& aNode , bool& aSuccess );

				/**
					Comparison operation
					@param aConnectionDescriptor another ConnectionDescriptor to compare with
					@return whether the two ConnectionDescriptors are identical
				*/
				bool operator== ( const ConnectionDescriptor& aConnectionDescriptor ) const;

				//! An identifier for an individual
				std::string id;
				//! The full uri for making the connection
				std::string uri;
				//! The address table for building the node tree
				std::string address_table;
			};




		public:
			/**
				Default constructor
				Given a semi-colon delimeted list of glob expressions, parse all the files matching it (e.g. $BUILD/config/c*.xml). If one parsing fails throw an exception and return filename and line number
				@param aFilenameExpr a semi-colon delimeted list of glob expressions specifying one or more XML connection files to load
			*/
			ConnectionManager ( const std::string& aFilenameExpr );

			/**
				Destructor
			*/
			virtual ~ConnectionManager ();

			/**
				Retrieves protocol, host, and port from the connection file to create an IPbus Client
				Retrieves the address table file from the connection file to create the Node tree
				Puts the two together to create a full HwInterface
				@param aId the unique identifier for the connection
				@return a HwInterface which encapsulates the Node tree and the IPbus Client
			 */
			HwInterface getDevice ( const std::string& aId );

			/**
				Return all device IDs known to this connection manager
				@return all device IDs known to this connection manager
			*/
			std::vector<std::string> getDevices ( );

			/**
				Return all device IDs known to this connection manager which match a (boost) regular expression
				@param aRegex a (boost) regular expression against which the device IDs are tested
				@return all device IDs known to this connection manager
			*/
			std::vector<std::string> getDevices ( const boost::regex& aRegex );
			/**
				Return all device IDs known to this connection manager which match a (boost) regular expression
				@param aRegex a const char* expression which is converted to a (boost) regular expression against which the device IDs are tested
				@return all device IDs known to this connection manager
			*/
			std::vector<std::string> getDevices ( const char* aRegex );
			/**
				Return all device IDs known to this connection manager which match a (boost) regular expression
				@param aRegex a string expression which is converted to a (boost) regular expression against which the device IDs are tested
				@return all device IDs known to this connection manager
			*/
			std::vector<std::string> getDevices ( const std::string& aRegex );

		private:
			/**
				Method called once the file specified in the constructor has been opened
				@param aProtocol The protocol by which the file was loaded
				@param aPath The fully qualified path to the file which has been opened
				@param aFile A byte vector containing the content of the opened file. Done like this since the routine handles local and http files identically
			*/
			void CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile );

		private:
			//! All the connections found matching the semi-colon delimeted list of glob expressions specified in the constructor
			std::vector< std::pair<std::string, std::string> >  mConnectionFiles;	//protocol, filename
			//! A map of connection identifiers to stucts containing details of the parsed XML node
			std::map< std::string, ConnectionDescriptor >  mConnectionDescriptors;	//connection identifier, parsed descriptor (also contains the connection identifier)
			//! A set of previously opened filenames, so that the same file is not parsed multiple times
			std::set< std::string > mPreviouslyOpenedFiles;							//previously opened file names

	};


}

#endif

