/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library aod programming framework
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

#include "uhal/DerivedNodeFactory.hpp"


#include <memory>
#include <stddef.h>                          // for NULL
#include <unordered_map>

#include "uhal/log/LogLevels.hpp"            // for Warning
#include "uhal/log/log_inserters.quote.hpp"  // for Quote, _Quote
#include "uhal/log/log.hpp"
#include "uhal/Node.hpp"


namespace uhal
{

  std::shared_ptr<DerivedNodeFactory> DerivedNodeFactory::mInstance;


  DerivedNodeFactory::DerivedNodeFactory ()
  {
  }


  DerivedNodeFactory::~DerivedNodeFactory ()
  {
  }


  DerivedNodeFactory& DerivedNodeFactory::getInstance()
  {
    if ( ! mInstance )
    {
      mInstance.reset(new DerivedNodeFactory());
    }

    return *mInstance;
  }


  Node* DerivedNodeFactory::convertToClassType ( Node* aNode )
  {
    std::unordered_map< std::string , std::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.find ( aNode->mClassName );

    if ( lIt == mCreators.end() )
    {
      log ( Warning , "Class " , Quote ( aNode->mClassName ) , " is unknown to the NodeTreeBuilder class factory. A plain node will be returned instead." );

      if ( mCreators.size() )
      {
        log ( Warning , "Known types are:" );

        for ( std::unordered_map< std::string , std::shared_ptr<CreatorInterface> >::const_iterator lIt = mCreators.begin() ; lIt != mCreators.end() ; ++lIt )
        {
          log ( Warning , "    > " , lIt->first );
        }
      }
      else
      {
        log ( Warning , "No class types have been defined" );
      }

      return aNode;
    }
    else
    {
      Node* lClassNode ( lIt->second->create ( *aNode ) );
      delete aNode;
      return lClassNode;
    }
  }

}
