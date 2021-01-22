
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


#include <fstream>
#include <iomanip>
#include <memory>

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>

#include "uhal/Node.hpp"


namespace uhal {
  namespace detail {

    std::string getAddressDescription(const Node& aNode, const uint32_t aAddress, const size_t& aMaxListSize)
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
        const std::vector<const Node*> lLineage = (*lIt)->getLineage(aNode);

        size_t i = 0;
        for ( ; i < std::min(lLineage.size(), lCommonAncestorLineage.size()); i++ ) {
          if ( lCommonAncestorLineage.at(i) != lLineage.at(i) )
            break;
        }
        lCommonAncestor = lLineage.at(i - 1);
        lCommonAncestorLineage.assign(lLineage.begin(), lLineage.begin() + i);
      }

      if ( (aMaxListSize != 0) and (lMatches.size() > aMaxListSize) )
      {
        if ( lCommonAncestor == &aNode )
          return std::to_string(lMatches.size()) + " nodes match";
        else
          return std::to_string(lMatches.size()) + " nodes under \"" + lCommonAncestor->getPath() + "\" match";
      }
      else
      {
        const std::string lCommonAncestorPath(lCommonAncestor->getPath());
        const size_t lCommonPathLength(lCommonAncestor == &aNode ? 0 : lCommonAncestorPath.size() + 1);
        std::ostringstream lOSS;
        lOSS << "nodes \"" << lMatches.front()->getPath().substr(lCommonPathLength) << "\"";
        for (auto lIt = lMatches.begin() + 1; lIt < lMatches.end(); lIt++)
          lOSS << ", \"" << (*lIt)->getPath().substr(lCommonPathLength) << "\"";

        if (lCommonAncestor != &aNode)
          lOSS << " under \"" << lCommonAncestorPath << "\"";

        return lOSS.str();
      }
    }


    std::string getAddressDescription(const ClientInterface& aClient, const uint32_t aAddress, const size_t& aMaxListSize)
    {
      if ( std::shared_ptr<Node> lNode = aClient.mNode.lock() )
        return getAddressDescription(*lNode, aAddress, aMaxListSize);
      else
        return "";
    }


    bool compareNodeAddr ( const Node* aNodeL, const Node* aNodeR )
    {
      return ( aNodeL->getAddress() < aNodeR->getAddress() );
    }


    std::vector<std::pair<const Node*, const Node*> > getAddressOverlaps(const Node& aNode)
    {
      std::vector<const Node*> lNodes;
      for (Node::const_iterator lIt = aNode.begin() ; lIt.next() ; )
        lNodes.push_back(&*lIt);
      std::stable_sort ( lNodes.begin() , lNodes.end() , compareNodeAddr );

      std::vector<std::pair<const Node*, const Node*> > lOverlappingNodes;
      if (lNodes.size() < 2)
        return lOverlappingNodes;

      for (std::vector<const Node*>::const_iterator lIt1 = lNodes.begin() ; lIt1 != (lNodes.end() - 1); lIt1++)
      {
        const Node& lNode1 = **lIt1;
        if (lNode1.getMode() == defs::HIERARCHICAL)
          continue;

        const uint32_t lMaxAddr1(lNode1.getMode() == defs::INCREMENTAL ? lNode1.getAddress() + lNode1.getSize() - 1 : lNode1.getAddress());

        for (std::vector<const Node*>::const_iterator lIt2(lIt1 + 1) ; (lIt2 != lNodes.end()) and (*lIt2)->getAddress() <= lMaxAddr1; lIt2++)
        {
          if ((*lIt2)->getMode() == defs::HIERARCHICAL)
            continue;

          const Node& lNode2 = **lIt2;

          if (lNode1.getMode() != defs::SINGLE or lNode2.getMode() != defs::SINGLE)
            lOverlappingNodes.push_back( std::make_pair(&lNode1, &lNode2) );

          else if (lNode1.getMask() & lNode2.getMask())
          {
            if (lNode1.getMask() == defs::NOMASK and lNode2.isChildOf(lNode1))
            {
              // Node 2 is masked child of node 1: No overlap
            }
            else if (lNode2.getMask() == defs::NOMASK and lNode1.isChildOf(lNode2))
            {
              // Node 1 is masked child of node 2: No overlap
            }
            else
              lOverlappingNodes.push_back( std::make_pair(&lNode1, &lNode2) );
          }
        }
      }

      return lOverlappingNodes;
    }


    void printNodeOverlapDescription(std::ostream& aStream, const Node& aNode1, const Node& aNode2)
    {
      std::ios_base::fmtflags lInitialFlags = aStream.flags();
      char lInitialFillChar = aStream.fill('0');
      aStream << std::hex;

      aStream << "Node '" << aNode1.getPath() << "' [";
      if (aNode1.getMode() == defs::INCREMENTAL)
        aStream << "addresses 0x"  << std::setw ( 8 ) << aNode1.getAddress() << " - 0x"  << std::setw ( 8 ) << aNode1.getAddress() + aNode1.getSize() - 1;
      else if ( aNode2.getMode() == defs::INCREMENTAL )
        aStream << "address 0x" << std::setw ( 8 ) << aNode1.getAddress();
      else
        aStream << "address 0x" << std::setw ( 8 ) << aNode1.getAddress() << ", mask 0x" << std::setw ( 8 ) << aNode1.getMask();

      aStream << "] overlaps with '" << aNode2.getPath() << "' [";
      if (aNode2.getMode() == defs::INCREMENTAL)
        aStream << "addresses 0x"  << std::setw ( 8 ) << aNode2.getAddress() << " - 0x"  << std::setw ( 8 ) << aNode2.getAddress() + aNode2.getSize() - 1;
      else if ( aNode1.getMode() == defs::INCREMENTAL )
        aStream << "address 0x" << std::setw ( 8 ) << aNode2.getAddress();
      else
        aStream << "address 0x" << std::setw ( 8 ) << aNode2.getAddress() << ", mask 0x" << std::setw ( 8 ) << aNode2.getMask();
      aStream << "].";

      aStream.flags(lInitialFlags);
      aStream.fill(lInitialFillChar);
    }


    bool writeNodeOverlapReport(const std::string& aFilePath, const std::vector<std::pair<const Node*, const Node*> >& aNodes, const std::string& aHeader)
    {
      // 1. Create main contents of report
      std::stringstream lReport;
      lReport << std::hex << std::setfill ( '0' );

      for (const auto& x: aNodes)
      {
        printNodeOverlapDescription(lReport, *x.first, *x.second);
        lReport << std::endl;
      }

      // 2. Write report to file
      const bool lNewlyCreatedFile = not boost::filesystem::is_regular_file( boost::filesystem::path(aFilePath) );
      std::ofstream lReportFile ( aFilePath.c_str() );

      if ( lReportFile.is_open() )
      {
        lReportFile << aHeader << std::endl;
        lReportFile << "Written at " << boost::posix_time::microsec_clock::local_time() << "." << std::endl;
        lReportFile << std::endl;
        lReportFile << lReport.rdbuf();
        lReportFile.close();

        if ( lNewlyCreatedFile )
        {
          boost::filesystem::permissions( aFilePath , boost::filesystem::perms( 0666 ) );
        }

        return true;
      }
      else
        return false;
    }

  }
}