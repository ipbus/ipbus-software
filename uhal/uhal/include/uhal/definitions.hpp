/**
	@file
	@author Andrew W. Rose
	@author Marc Magrans De Abril
	@date 2012
*/

#ifndef _uhal_definitions_hpp_
#define _uhal_definitions_hpp_

#include <stdint.h>

namespace uhal
{
  namespace defs
  {
    //! define Read and Write permissions of a uhal Node
    enum NodePermission {READ=0x1,WRITE=0x2,READWRITE=0x3};

    //! define whether transactions target a single register, a block of registers, a block-read/write port or whether node is purely hierarchical
    enum BlockReadWriteMode {SINGLE,INCREMENTAL,NON_INCREMENTAL,HIERARCHICAL};

    //! define what it means to have no mask
    const uint32_t NOMASK = 0xFFFFFFFF;
  }
}

#endif

