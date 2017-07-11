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

#include "uhal/grammars/URI.hpp"


//#include "uhal/IPbusInspector.hpp"
// #include "uhal/logo.hpp"

#include <sys/time.h>
#include <iostream>

namespace uhal
{

  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  template < typename InnerProtocol >
  PCIe< InnerProtocol >::PCIe ( const std::string& aId, const URI& aUri ) :
    InnerProtocol ( aId , aUri )
  {
    std::string lPCIePath( '/' + aUri.mPath );

    std::cout << "Id: " << aId << ", Path: " << lPCIePath << std::endl;
  }



  template < typename InnerProtocol >
  PCIe< InnerProtocol >::PCIe ( const PCIe< InnerProtocol >& aPCIe )
  {}


  template < typename InnerProtocol >
  PCIe< InnerProtocol >& PCIe< InnerProtocol >::operator= ( const PCIe< InnerProtocol >& aPCIe )
  {}

  template < typename InnerProtocol >
  PCIe< InnerProtocol >::~PCIe()
  {}



  template < typename InnerProtocol >
  void PCIe< InnerProtocol >::implementDispatch ( boost::shared_ptr< Buffers > aBuffers )
  {

    // I split the write and read processes into separate functions for clarity only here,
    // For multi-threaded code it becomes more important (and far more confusing)

    mDispatchBuffers = aBuffers;
    write ( );
  }


  template < typename InnerProtocol >
  void PCIe< InnerProtocol >::write ( )
  {

    if ( !mDispatchBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mDispatchBuffers' was NULL" );
      return;
    }

    // DO WRITE OPERATION HERE
    // mDispatchBuffers->getSendBuffer()
    // mDispatchBuffers->sendCounter()

    mReplyBuffers = mDispatchBuffers;
    mDispatchBuffers.reset();
    read ( );
  }


  template < typename InnerProtocol >
  void PCIe< InnerProtocol >::read ( )
  {
    if ( !mReplyBuffers )
    {
      log ( Error() , __PRETTY_FUNCTION__ , " called when 'mReplyBuffers' was NULL" );
      return;
    }

    // DO READ OPERATION HERE
    // mReplyBuffers->getReplyBuffer()

    if ( uhal::exception::exception* lExc = ClientInterface::validate ( mReplyBuffers ) ) //Control of the pointer has been passed back to the client interface
    {
      throw lExc;
    }
    mReplyBuffers.reset();
  }


  template < typename InnerProtocol >
  void PCIe< InnerProtocol >::Flush( )
  {
  }

  template < typename InnerProtocol >
  void PCIe< InnerProtocol >::dispatchExceptionHandler()
  {
    InnerProtocol::dispatchExceptionHandler();
  }

}

