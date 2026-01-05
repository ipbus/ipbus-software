#ifndef _uhal_pycohal_version_hpp_
#define _uhal_pycohal_version_hpp_


namespace pybind11 {
class module_;
}

namespace pycohal
{
  void wrap_version_and_build_info(pybind11::module_&);
}


#endif

