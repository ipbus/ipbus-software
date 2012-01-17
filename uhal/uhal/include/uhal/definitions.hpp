#ifndef _uhal_definitions_hpp_
#define _uhal_definitions_hpp_

#include <stdint.h>

namespace uhal {
  namespace defs {
    enum NodePermission {READ=0x1,WRITE=0x2,READWRITE=0x3};
  
    enum BlockReadWriteMode {INCREMENTAL,NON_INCREMENTAL};

    enum VerificationMode {VERIFY,NO_VERIFY};

    const uint32_t NOMASK = 0xFFFFFFFF;
  }
}

#endif 

