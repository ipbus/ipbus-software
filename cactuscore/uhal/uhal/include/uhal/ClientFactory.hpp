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

#ifndef _uhal_ClientFactory_hpp_
#define _uhal_ClientFactory_hpp_

#include "uhal/log/exception.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/log/log.hpp"

#include <boost/shared_ptr.hpp>

#include <map>


namespace uhal
{
  namespace exception
  {
    // //! Exception class to handle the case where a protocol already exists in the creator map. Uses the base uhal::exception implementation of what()
    // class ProtocolAlreadyExist : public exception {};
    //! Exception class to handle the case where the protocol requested does not exists in the creator map.
    ExceptionClass ( ProtocolDoesNotExist , "Exception class to handle the case where the protocol requested does not exists in the creator map." )

    //! Exception class to handle the case where the factory failed to parse URI.
    ExceptionClass ( FailedToParseURI , "Exception class to handle the case where the factory failed to parse URI." )
  }

  /**
  A class to construct an IPbus client based on the protocol identifier specified
  NOTE! This is a factory method and must be Mutex protected if it is used in multithreaded environments!
  */

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
          CreatorInterface()
          {
          }
          /**
          	Destructor
          */
          virtual ~CreatorInterface()
          {
          }
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
          Creator()
          {
          }
          /**
          	Destructor
          */
          virtual ~Creator()
          {
          }
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
      boost::unordered_map< std::string , boost::shared_ptr< CreatorInterface > > mCreators; //map string name of each protocol to a creator for that protocol
      //! Store the description of the factory product
      std::map< std::string , std::string > mProductDescriptions;

  };

}

#include "uhal/TemplateDefinitions/ClientFactory.hxx"

#endif
