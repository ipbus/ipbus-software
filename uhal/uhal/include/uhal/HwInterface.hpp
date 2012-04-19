/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_HwInterface_hpp_
#define _uhal_HwInterface_hpp_

#include "uhal/Node.hpp"
#include "uhal/ClientInterface.hpp"

#include <boost/regex.hpp>


namespace uhal
{
	//! A class which bundles a node tree and an IPbus client interface together providing everything you need to navigate and perform hardware access
	class HwInterface
	{
		public:
			/**
				Constructor
			*/
			HwInterface ( const boost::shared_ptr<ClientInterface>& aClientInterface , const Node& aNode );

			/**
				Get the underlying IPbus client
			*/
			boost::shared_ptr<ClientInterface> getClient();

			/**
				Make the IPbus client issue a dispatch
			*/
			void dispatch ();

			/**
				Retrieve the Node given by a full-stop delimeted name path relative, to the top-level node
				@param aId a full-stop delimeted name path to a node, relative to the top-level node
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

			// Node& getNode();

			/**
				Get the target device's reserved address information
				@return a Validated Memory which wraps the location to which the reserved address information will be written
			*/
			ValVector< uint32_t > readReservedAddressInfo ();

		private:
			/**
				A function which sets the HwInterface pointer in the Node to point to this HwInterface
			*/
			void claimNode ( Node& aNode );

			//! A shared pointer to the IPbus client through which the transactions will be sent
			boost::shared_ptr<ClientInterface> mClientInterface;

			//! A node tree
			Node mNode;

	};


}

#endif
