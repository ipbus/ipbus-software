
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

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

---------------------------------------------------------------------------
*/

#include "uhal/detail/utilities.hpp"


#include "boost/lexical_cast.hpp"
#include "boost/shared_ptr.hpp"

#include "uhal/Node.hpp"


namespace uhal {
  namespace detail {

    std::string getAddressDescription(const Node& aNode, const uint32_t aAddress)
    {
      std::vector<const Node*> lMatches;
      for ( Node::const_iterator lIt = aNode.begin(); lIt != aNode.end(); lIt++ ) {

        if ( lIt->getMode() == uhal::defs::INCREMENTAL ) {
          if ( ( aAddress >= lIt->getAddress() ) and ( aAddress < ( lIt->getAddress() + lIt->getSize() ) ) )
            lMatches.push_back(&*lIt);
        }
        else if ( lIt->getMode() != uhal::defs::HIERARCHICAL ) {
          if ( lIt->getAddress() == aAddress )
          	lMatches.push_back(&*lIt);
        }
      }

      if ( lMatches.empty() )
        return "no matching nodes";
      else if ( lMatches.size() == 1 )
        return "node \"" + lMatches.front()->getPath() + "\"";

      const Node* lCommonAncestor = lMatches.front();
      std::vector<const Node*> lCommonAncestorLineage = lCommonAncestor->getLineage(aNode);

      for ( std::vector<const Node*>::const_iterator lIt = lMatches.begin()+1; lIt != lMatches.end(); lIt++ ) {
        std::vector<const Node*> lLineage = (*lIt)->getLineage(aNode);
        lLineage.push_back(*lIt);

        size_t i = 0;
        for ( ; i < std::min(lLineage.size(), lCommonAncestorLineage.size()); i++ ) {
          if ( lCommonAncestorLineage.at(i) != lLineage.at(i) )
            break;

        	lCommonAncestor = lLineage.at(i);
        }
        lCommonAncestorLineage.assign(lLineage.begin(), lLineage.begin() + i + 1);
      }

    	return boost::lexical_cast<std::string>(lMatches.size()) + " descendants of node \"" + lCommonAncestor->getPath() + "\" match";
    }


    std::string getAddressDescription(const ClientInterface& aClient, const uint32_t aAddress)
    {
      if ( boost::shared_ptr<Node> lNode = aClient.mNode.lock() )
        return getAddressDescription(*lNode, aAddress);
      else
        return "";
    }
  }
}