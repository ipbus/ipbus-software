
#ifndef _uhal_detail_SharedObject_hpp_
#define _uhal_detail_SharedObject_hpp_

#include <string>

#include <boost/interprocess/managed_shared_memory.hpp>


namespace uhal {
namespace detail {

//! Wrapper for C++ object that's placed in shared memory
template <class T>
class SharedObject {
public:

  SharedObject(const SharedObject<T>&) = delete;
  SharedObject<T>& operator=(const SharedObject<T>&) = delete;

  SharedObject(const std::string& aName);
  ~SharedObject();

  T* operator->();

  T& operator*();

private:
  std::string mName;
  boost::interprocess::managed_shared_memory mSharedMem;
  T* mObj;
};

}
}

#endif
