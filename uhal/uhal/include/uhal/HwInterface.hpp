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
				@param aClientInterface a shared pointer to a client interface which performs the transport
				@param aNode a shared pointer to a >>freshly cloned<< node representing the >>full<< endpoint structure
			*/
			HwInterface ( const boost::shared_ptr<ClientInterface>& aClientInterface , const boost::shared_ptr< Node >& aNode );

			/**
				Destructor
			*/
			virtual ~HwInterface();

			/**
				Get the underlying IPbus client
				@return the underlying IPbus client
			*/
			ClientInterface& getClient();

			/**
				Return the url of the target for this client
				@return the url of the target for this client
			*/
			std::string uri();

			/**
				Return the identifier of the target for this client
				@return the identifier of the target for this client
			*/
			const std::string& id();


			/**
				Make the IPbus client issue a dispatch
			*/
			void dispatch ();

			/**
				A method to modify the timeout period for any pending or future transactions
				@param aTimeoutPeriod the desired timeout period in milliseconds
			*/
			void setTimeoutPeriod ( const uint32_t& aTimeoutPeriod );

			/**
				A method to retrieve the timeout period currently being used
				@return the timeout period currently being used in milliseconds
			*/
			uint32_t getTimeoutPeriod();

			// /**
			// Ping the target for this client
			// */
			// void ping();

			/**
				Retrieve the top-level node
				@return the top-level node
			*/
			Node& getNode ();

			/**
				Retrieve the Node given by a full-stop delimeted name path relative, to the top-level node
				@param aId a full-stop delimeted name path to a node, relative to the top-level node
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

			// /**
			// Get the target device's reserved address information
			// @return a Validated Memory which wraps the location to which the reserved address information will be written
			// */
			// ValVector< uint32_t > readReservedAddressInfo ();

		private:
			/**
				A function which sets the HwInterface pointer in the Node to point to this HwInterface
				@param aNode a Node that is to be claimed
			*/
			void claimNode ( Node& aNode );

			//! A shared pointer to the IPbus client through which the transactions will be sent
			boost::shared_ptr<ClientInterface> mClientInterface;

			//! A node tree
			boost::shared_ptr<Node> mNode;

	};

}

#include "uhal/TemplateDefinitions/HwInterface.hxx"

#endif
