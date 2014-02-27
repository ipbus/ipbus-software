#ifndef _uhal_pycohal_converters_exceptions_hpp_
#define _uhal_pycohal_converters_exceptions_hpp_

#include <iostream>
#include <vector>

#include "uhal/log/exception.hpp"
#include "uhal/log/log.hpp"

// The following python boost patch is required to compile on apple
#include "uhal/pycohal/boost_python.hpp"

#include "boost/python/converter/rvalue_from_python_data.hpp"
#include "boost/unordered_map.hpp"

namespace pycohal {

  struct Converter_vec_uint32_from_list {
    // Default CTOR. Registers this converter to boost::python

    Converter_vec_uint32_from_list() {
      boost::python::converter::registry::push_back(&convertible, &construct, boost::python::type_id< std::vector<uint32_t> >());
    }

    // Determine if obj_ptr can be converted to vector<uint32_t>
    static void* convertible(PyObject* obj_ptr);

    // Convert obj_ptr to a C++ vector<uint32_t>
    static void construct(PyObject* obj_ptr, boost::python::converter::rvalue_from_python_stage1_data* data);
  };

  template <class T>
  struct Converter_std_vector_to_list {
    static PyObject* convert(const std::vector<T>& v);
  };

  /**
   Templated converter from boost::unordered_map to dict
   */
  template <class U, class T>
  struct Converter_boost_unorderedmap_to_dict {
    static PyObject * convert(const boost::unordered_map<U, T>& m);
  };

  // Registers to boost::python all of the pycohal to-python and from-python converters
  void register_converters();


  // EXCEPTIONS //

  ExceptionClass(PycohalLogLevelEnumError, "Exception class to handle errors in translating logging levels");

  PyObject* create_exception_class(const std::string& excName, PyObject* baseTypeObj = PyExc_Exception);

  /// Functor for exception translation.
  /// Written as functor (rather than function) to avoid storing the PyObject* for python versions of exception classes as global variables (member exception_pyType_ used here instead)

  template<class ExceptionType>
  class ExceptionTranslator {
  public:
    ExceptionTranslator(PyObject* exception_pyType);

    /// Translation function called at the C-python boundary
    void operator() (const ExceptionType& e) const;

  private:
    PyObject* exception_pyType_; ///< Pointer to PyObject corresponding to C++ exception class ExceptionType
  };

  template<class ExceptionType>
  void wrap_derived_exception(const std::string& exceptionName, PyObject* base_exception_pyType) {
    PyObject* derived_exception_pyType = pycohal::create_exception_class(exceptionName, base_exception_pyType);
    boost::python::register_exception_translator<ExceptionType> (pycohal::ExceptionTranslator<ExceptionType> (derived_exception_pyType));
  }

  void wrap_exceptions();
}



//----------------------------------------//
// ---  Converter_std_vector_to_list  --- //
//----------------------------------------//

template <class T>
PyObject* pycohal::Converter_std_vector_to_list<T>::convert(const std::vector<T>& vec) {
  namespace bpy = boost::python;
  bpy::list theList;

  for (typename std::vector<T>::const_iterator it = vec.begin(); it != vec.end(); it++) {
    theList.append(bpy::object(*it));
  }

  return bpy::incref(theList.ptr());
}

//-----------------------------------------------//
// --- Converter_boost_unorderedmap_to_dict --- //
//----------------------------------------------//
template <class U, class T>
PyObject* pycohal::Converter_boost_unorderedmap_to_dict<U,T>::convert(const boost::unordered_map<U, T>& m) {
  namespace bpy = boost::python;
  bpy::dict theDict;

  for (typename boost::unordered_map<U, T>::const_iterator it = m.begin(); it != m.end(); it++) {
    theDict[it->first] = bpy::object(it->second);
  }

  return bpy::incref(theDict.ptr());
}

//-------------------------------//
// ---  ExceptionTranslator  --- //
//-------------------------------//

template <class ExceptionType>
pycohal::ExceptionTranslator<ExceptionType>::ExceptionTranslator(PyObject* exception_pyType) :
exception_pyType_(exception_pyType) {
}

template <class ExceptionType>
void pycohal::ExceptionTranslator<ExceptionType>::operator() (const ExceptionType& e) const {
  namespace bpy = boost::python;
  bpy::object pyException(bpy::handle<> (bpy::borrowed(exception_pyType_)));
  pyException.attr("what") = e.what();
  PyErr_SetObject(exception_pyType_, pyException.ptr());
}


#endif
