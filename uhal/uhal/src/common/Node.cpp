#include "uhal/Node.hpp"

#include "uhal/HwInterface.hpp"
#include "uhal/ValMem.hpp"
#include "uhal/AddressTable.hpp"

namespace uhal
{
    Node::Node( HwInterface* aHwInterface, const std::string& aFullId )
        :mHw( aHwInterface ),
         mFullId( aFullId ),
         mAddr( aHwInterface->getAddressTable().getAddress( aFullId ) ),
         mMask( aHwInterface->getAddressTable().getMask( aFullId ) ),
         mPermission( aHwInterface->getAddressTable().getPermission( aFullId ) )
    {}

    Node Node::getNode( const std::string& aId )
    {
        return Node( mHw , mFullId + "." + aId );
    }

    std::vector<std::string> Node::getNodes()
    {
        return mHw->getAddressTable().getChildren( mFullId );
    }


}
