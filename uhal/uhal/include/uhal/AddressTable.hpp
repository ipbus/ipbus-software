#ifndef _uhal_AddressTable_hpp_
#define _uhal_AddressTable_hpp_

#include "uhal/definitions.hpp"

#include <string>
#include <vector>

namespace uhal
{



	class AddressTable
	{
		public:
			AddressTable ( );

			virtual ~AddressTable () ;
			
			uint32_t getAddress ( const std::string& aFullId );

			uint32_t getMask ( const std::string& aFullId );

			defs::NodePermission getPermission ( const std::string& aFullId );

			std::vector<std::string> getChildren ( const std::string& aId );

		private:


	};
}

#endif
