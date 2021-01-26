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

#include "uhal/ConnectionManager.hpp"


#include <mutex>

#include <boost/filesystem/operations.hpp>
#include <boost/regex.hpp>
#include <boost/spirit/include/qi.hpp>

#include "uhal/Node.hpp"
#include "uhal/NodeTreeBuilder.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/ClientFactory.hpp"
#include "uhal/utilities/files.hpp"
#include "uhal/utilities/xml.hpp"

#include "uhal/log/log.hpp"


// Resolve std bind placeholders (_1, _2, ...)
namespace arg = std::placeholders;


namespace uhal
{

  ConnectionManager::ConnectionDescriptor::ConnectionDescriptor ( const pugi::xml_node& aNode , const boost::filesystem::path& aConnectionFile , bool& aSuccess ) :
    connection_file ( aConnectionFile )
  {
    aSuccess=false;

    if ( ! uhal::utilities::GetXMLattribute<true> ( aNode , "id" , id ) )
    {
      return;
    }

    if ( ! uhal::utilities::GetXMLattribute<true> ( aNode , "uri" , uri ) )
    {
      return;
    }

    if ( ! uhal::utilities::GetXMLattribute<true> ( aNode , "address_table" , address_table ) )
    {
      return;
    }

    aSuccess=true;
  }


  bool ConnectionManager::ConnectionDescriptor::operator== ( const ConnectionDescriptor& aConnectionDescriptor ) const
  {
    if ( id != aConnectionDescriptor.id )
    {
      return false;
    }

    if ( uri != aConnectionDescriptor.uri )
    {
      return false;
    }

    if ( address_table != aConnectionDescriptor.address_table )
    {
      return false;
    }

    return true;
  }




  ConnectionManager::ConnectionManager ( const std::string& aFilenameExpr )
  {
    //Mutex lock here to be on the safe side
    std::lock_guard<std::mutex> lLock ( mMutex );
    std::vector< std::pair<std::string, std::string> >  lConnectionFiles;	//protocol, filename
    uhal::utilities::ParseSemicolonDelimitedUriList ( aFilenameExpr , lConnectionFiles );

    for (const auto& x: lConnectionFiles)
      uhal::utilities::OpenFile ( x.first , x.second , boost::filesystem::current_path() , std::bind ( &ConnectionManager::CallBack, this , arg::_1 , arg::_2 , arg::_3 ) );
  }


  ConnectionManager::ConnectionManager ( const std::string& aFilenameExpr , const std::vector<std::string>& aUserClientActivationList ) :
    mUserClientActivationList(aUserClientActivationList)
  {
    //Mutex lock here to be on the safe side
    std::lock_guard<std::mutex> lLock ( mMutex );
    std::vector< std::pair<std::string, std::string> >  lConnectionFiles; //protocol, filename
    uhal::utilities::ParseSemicolonDelimitedUriList ( aFilenameExpr , lConnectionFiles );

    for (const auto& x: lConnectionFiles)
      uhal::utilities::OpenFile ( x.first , x.second , boost::filesystem::current_path() , std::bind ( &ConnectionManager::CallBack, this , arg::_1 , arg::_2 , arg::_3 ) );
  }


  ConnectionManager::~ConnectionManager ()
  {
  }


  HwInterface ConnectionManager::getDevice ( const std::string& aId )
  {
    //We need a mutex lock here to protect access to the NodeTreeBuilder and the ClientFactory
    std::lock_guard<std::mutex> lLock ( mMutex );

    if ( mConnectionDescriptors.size() == 0 )
    {
      exception::ConnectionUIDDoesNotExist lExc;
      log ( lExc , "Connection map contains no entries" );
      throw lExc;
    }

    std::map< std::string, ConnectionDescriptor >::iterator lIt = mConnectionDescriptors.find ( aId );

    if ( lIt == mConnectionDescriptors.end() )
    {
      exception::ConnectionUIDDoesNotExist lExc;
      log ( lExc , "Device ID , " , Quote ( aId ) , ", does not exist in connection map" );
      throw lExc;
    }

    //The node tree builder returns a newly created Node which we can safely wrap as a shared_ptr
    std::shared_ptr< Node > lNode ( NodeTreeBuilder::getInstance().getNodeTree ( lIt->second.address_table , lIt->second.connection_file ) );
    log ( Info() , "ConnectionManager created node tree: " , *lNode );
    std::shared_ptr<ClientInterface> lClientInterface ( ClientFactory::getInstance().getClient ( lIt->second.id , lIt->second.uri , mUserClientActivationList ) );
    return HwInterface ( lClientInterface , lNode );
  }


  HwInterface ConnectionManager::getDevice ( const std::string& aId , const std::string& aUri , const std::string& aAddressFileExpr )
  {
    //We need a mutex lock here to protect access to the TodeTreeBuilder and the ClientFactory
    std::lock_guard<std::mutex> lLock ( mMutex );
    std::shared_ptr< Node > lNode ( NodeTreeBuilder::getInstance().getNodeTree ( aAddressFileExpr , boost::filesystem::current_path() / "." ) );
    log ( Info() , "ConnectionManager created node tree: " , *lNode );
    std::shared_ptr<ClientInterface> lClientInterface ( ClientFactory::getInstance().getClient ( aId , aUri ) );
    return HwInterface ( lClientInterface , lNode );
  }


  HwInterface ConnectionManager::getDevice ( const std::string& aId , const std::string& aUri , const std::string& aAddressFileExpr, const std::vector<std::string>& aUserClientActivationList )
  {
    //We need a mutex lock here to protect access to the TodeTreeBuilder and the ClientFactory
    std::lock_guard<std::mutex> lLock ( mMutex );
    std::shared_ptr< Node > lNode ( NodeTreeBuilder::getInstance().getNodeTree ( aAddressFileExpr , boost::filesystem::current_path() / "." ) );
    log ( Info() , "ConnectionManager created node tree: " , *lNode );
    std::shared_ptr<ClientInterface> lClientInterface ( ClientFactory::getInstance().getClient ( aId , aUri , aUserClientActivationList ) );
    return HwInterface ( lClientInterface , lNode );
  }


  std::vector<std::string> ConnectionManager::getDevices ( ) const
  {
    std::vector<std::string> lDevices;
    lDevices.reserve ( mConnectionDescriptors.size() ); //prevent reallocations

    for ( std::map< std::string, ConnectionDescriptor >::const_iterator lIt = mConnectionDescriptors.begin() ; lIt != mConnectionDescriptors.end() ; ++lIt )
    {
      lDevices.push_back ( lIt->first );
    }

    return lDevices;
  }


  std::vector<std::string> ConnectionManager::getDevices ( const std::string& aRegex ) const
  {
    std::vector<std::string> lDevices;
    lDevices.reserve ( mConnectionDescriptors.size() ); //prevent reallocations

    for ( std::map< std::string, ConnectionDescriptor >::const_iterator lIt = mConnectionDescriptors.begin() ; lIt != mConnectionDescriptors.end() ; ++lIt )
    {
      boost::cmatch lMatch;

      if ( boost::regex_match ( lIt->first.c_str() , lMatch ,  boost::regex ( aRegex ) ) ) //to allow partial match, add  boost::match_default|boost::match_partial  as fourth argument
      {
        lDevices.push_back ( lIt->first );
      }
    }

    return lDevices;
  }


  void ConnectionManager::clearAddressFileCache()
  {
    // Need a mutex lock here to protect access to NodeTreeBuilder
    std::lock_guard<std::mutex> lLock ( mMutex );
    log( Info(), "ConnectionManager is clearing the address filename -> Node tree cache");
    NodeTreeBuilder::getInstance().clearAddressFileCache();
  }


  void ConnectionManager::CallBack ( const std::string& aProtocol , const boost::filesystem::path& aPath , std::vector<uint8_t>& aFile )
  {
    std::pair< std::set< std::string >::iterator , bool > lInsert = mPreviouslyOpenedFiles.insert ( aProtocol+ ( aPath.string() ) );

    if ( ! lInsert.second )
    {
      log ( Info() , "File " ,  Quote ( aProtocol+ ( aPath.string() ) ) , " has already been parsed. I am not reparsing and will continue with next document for now but be aware!" );
      return;
    }

    pugi::xml_document lXmlDocument;
    pugi::xml_parse_result lLoadResult = lXmlDocument.load_buffer_inplace ( & ( aFile[0] ) , aFile.size() );

    if ( !lLoadResult )
    {
      //Mark says to throw on this condition, I will leave it continuing for now...
      uhal::utilities::PugiXMLParseResultPrettifier ( lLoadResult , aPath , aFile );
      return;
    }

    pugi::xpath_node_set lConnections = lXmlDocument.select_nodes ( "/connections/connection" );

    for ( pugi::xpath_node_set::const_iterator lConnectionIt = lConnections.begin(); lConnectionIt != lConnections.end(); ++lConnectionIt )
    {
      bool lSuccess;
      ConnectionDescriptor lDescriptor ( lConnectionIt->node() , aPath , lSuccess );

      if ( lSuccess )
      {
        std::pair< std::map< std::string, ConnectionDescriptor >::iterator , bool > lInsert = mConnectionDescriptors.insert ( std::make_pair ( lDescriptor.id , lDescriptor ) );

        if ( !lInsert.second )
        {
          if ( lInsert.first->second == lDescriptor )
          {
            log ( Info() , "Duplicate connection entry found:"
                  "\n > id = " , lDescriptor.id ,
                  "\n > uri = " , lDescriptor.uri ,
                  "\n > address_table = " , lDescriptor.address_table ,
                  "\n Continuing for now but be aware!" );
          }
          else
          {
            exception::DuplicatedUID lExc;
            log ( lExc , "Duplicate connection ID " , Quote ( lDescriptor.id ) , " found in connections file " ,
                  Quote ( aProtocol+ ( aPath.string() ) ) , " but parameters do not match! Bailing!" );
            throw lExc;
          }
        }
      }
      else
      {
        log ( Error() , "Construction of Connection Descriptor failed. Continuing with next Connection Descriptor for now but be aware!" );
      }
    }
  }


  std::mutex ConnectionManager::mMutex;

}

