
#include "uhal/AddressTable.hpp"


namespace uhal
{
	AddressTable::AddressTable ( )
	{	}

	AddressTable::~AddressTable () { }
	
	uint32_t AddressTable::getAddress ( const std::string& aFullId )
	{
		return 0x32;
	}

	uint32_t AddressTable::getMask ( const std::string& aFullId )
	{
		return defs::NOMASK;
	}

	defs::NodePermission AddressTable::getPermission ( const std::string& aFullId )
	{
		return defs::READWRITE;
	}

	std::vector<std::string> AddressTable::getChildren ( const std::string& aId )
	{
		std::vector<std::string> lResult;
		lResult.push_back ( aId + "." + "REG1" );
		lResult.push_back ( aId + "." + "REG2" );
		lResult.push_back ( aId + "." + "REG3" );
		return lResult;
	}

}
