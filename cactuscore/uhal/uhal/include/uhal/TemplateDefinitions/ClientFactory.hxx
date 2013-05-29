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


namespace uhal
{

  template <class T>
  void ClientFactory::add ( const std::string& aProtocol , const std::string& aDescription )
  {
    boost::unordered_map<std::string , boost::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( aProtocol ) ;

    if ( lIt != mCreators.end() )
    {
      //
      //ProtocolAlreadyExist().throwFrom( ThisLocation() );
      log ( Warning() , "Protocol \"" , aProtocol , "\" already exists in map of creators. Continuing for now, but be warned." );
      return;
    }

    mCreators[aProtocol] =  boost::shared_ptr<CreatorInterface> ( new Creator<T>() );
    mProductDescriptions.insert ( std::make_pair ( aProtocol , aDescription ) );
  }


  template <class T>
  boost::shared_ptr<ClientInterface> ClientFactory::Creator<T>::create ( const std::string& aId , const URI& aUri )
  {
    return boost::shared_ptr<ClientInterface> ( new T ( aId , aUri ) );
  }


}
