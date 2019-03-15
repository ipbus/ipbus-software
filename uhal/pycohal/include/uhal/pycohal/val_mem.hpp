#ifndef _uhal_pycohal_val_mem_hpp_
#define _uhal_pycohal_val_mem_hpp_


#include <string>

#include <boost/python/slice.hpp>

#include "uhal/ValMem.hpp"


namespace pycohal
{
  template <typename T>
  std::string getHexString (const uhal::ValWord<T>& aValWord);

  template <typename T>
  std::string getString( const uhal::ValWord<T>& aValWord );

  template <typename T>
  std::string getString ( const uhal::ValVector<T>& aValVec );


  /// Struct containing wrapper functions for list-like indexing of ValVector<uint32_t> in python
  template <class T>
  struct ValVectorIndexingSuite
  {
    static void raiseIndexError();

    static const T& getItem ( const uhal::ValVector<T>& valVec, int i );
    
    static std::vector<T> getSlice( const uhal::ValVector<T>& valVec, const boost::python::slice aSlice );
  };

  void wrapValHeader();

  template <typename T>
  void wrapValWord(const std::string& aNameSuffix);

  template <typename T>
  void wrapValVector(const std::string& aNameSuffix);

  void wrapValMem();
}

#endif
