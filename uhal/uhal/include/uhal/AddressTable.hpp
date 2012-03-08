#ifndef _uhal_AddressTable_hpp_
#define _uhal_AddressTable_hpp_

#include "uhal/definitions.hpp"

#include "boost/utility.hpp"

#include <map>

namespace uhal
{
	class AddressTable
	{
		public:
			AddressTable ( const std::string& aFilename )
				:mFilename ( aFilename )
			{}

			uint32_t getAddress ( const std::string& aFullId )
			{
				return 0x32;
			}

			uint32_t getMask ( const std::string& aFullId )
			{
				return defs::NOMASK;
			}

			defs::NodePermission getPermission ( const std::string& aFullId )
			{
				return defs::READWRITE;
			}

			std::vector<std::string> getChildren ( const std::string& aId )
			{
				std::vector<std::string> lResult;
				lResult.push_back ( aId + "." + "REG1" );
				lResult.push_back ( aId + "." + "REG2" );
				lResult.push_back ( aId + "." + "REG3" );
				return lResult;
			}

		private:
			std::string mFilename;
	};
}

#endif
