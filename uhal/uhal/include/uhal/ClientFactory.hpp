/**
	@file
	@author Andrew W. Rose
	@author Marc Magrans De Abril
	@date 2012
*/

#ifndef _uhal_ClientFactory_hpp_
#define _uhal_ClientFactory_hpp_

#include "uhal/exception.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/Utilities.hpp"
#include "uhal/log/log.hpp"

//#include "boost/utility.hpp"
#include <boost/shared_ptr.hpp>

#include <map>

namespace uhal
{
  // //! Exception class to handle the case where a protocol already exists in the creator map. Uses the base uhal::exception implementation of what()
  // class ProtocolAlreadyExist: public uhal::_exception< ProtocolAlreadyExist > { };
  //! Exception class to handle the case where the protocol requested does not exists in the creator map. Uses the base uhal::exception implementation of what()
  class ProtocolDoesNotExist: public uhal::_exception< ProtocolDoesNotExist > { };

  //! A class to construct an IPbus client based on the protocol identifier specified
  class ClientFactory: private boost::noncopyable
  {

    private:
      /**
      	Default constructor
      	This is private since only a single instance is to be created, using the getInstance method
      */
      ClientFactory();

      /**
      	Destructor
      */
      virtual ~ClientFactory();

    public:
      /**
      	Static method to retrieve the single instance of the class
      	@return the single instance of the class
      */
      static ClientFactory& getInstance();

      /**
      	Method to create an associate between a protocol identifier and a Creator of a particular type
      	@param aProtocol the protocol identifier
      	@param aDescription an optional description of the protocol
      */
      template <class T>
      void add ( const std::string& aProtocol , const std::string& aDescription = "" );

      /**
      	Construct an IPbus client based on the protocol identifier specified
      	@param aId the uinique identifier that the client will be given.
      	@param aUri a string containing the full URI of the target. This string is parsed to extract the protocol, and it is this which is used to identify the relevent creator which is then used to create the client.
      	@return a shared pointer to the newly created client
      */
      boost::shared_ptr<ClientInterface> getClient ( const std::string& aId , const std::string& aUri );

    private:
      //! An abstract base class for defining the interface to the creators
      class CreatorInterface
      {
        public:
          /**
          	Default constructor
          */
          CreatorInterface() {}
          /**
          	Destructor
          */
          virtual ~CreatorInterface() {}
          /**
          	Interface to a function which create a new IPbus client based on the protocol identifier specified
          	@param aId the uinique identifier that the client will be given.
          	@param aUri a string containing the full URI of the target. This string is parsed to extract the protocol, and it is this which is used to identify the relevent creator which is then used to create the client.
          	@return a shared pointer to the newly created client
          */
          virtual boost::shared_ptr<ClientInterface> create ( const std::string& aId , const URI& aUri ) = 0;
      };

      //! Templated concrete implementation with a CreatorInterface interface
      template <class T>
      class Creator: public CreatorInterface
      {
        public:

          /**
          	Default constructor
          */
          Creator() {}
          /**
          	Destructor
          */
          virtual ~Creator() {}
          /**
          	Concrete function which creates a new IPbus client based on the protocol identifier specified
          	@param aId the uinique identifier that the client will be given.
          	@param aUri a string containing the full URI of the target. This string is parsed to extract the protocol, and it is this which is used to identify the relevent creator which is then used to create the client.
          	@return a shared pointer to the newly created client
          */
          boost::shared_ptr<ClientInterface> create ( const std::string& aId , const URI& aUri );
      };


    private:
      //! The single instance of the class
      static ClientFactory* mInstance;
      //! Hash map associating a creator for a particular protocol with a file name
      std::hash_map< std::string , boost::shared_ptr< CreatorInterface > > mCreators; //map string name of each protocol to a creator for that protocol
      //! Store the description of the factory product
      std::map< std::string , std::string > mProductDescriptions;

  };

}

#include "uhal/TemplateDefinitions/ClientFactory.hxx"

#endif
