
#ifndef UHAL_PYCOHAL_EXCEPTIONS_HXX
#define UHAL_PYCOHAL_EXCEPTIONS_HXX


#include "boost/python/object.hpp"
#include "boost/python/scope.hpp"



template<class ExceptionType>
pycohal::ExceptionTranslator<ExceptionType>::ExceptionTranslator(PyObject* aExceptionPyType) : 
 mExceptionPyType(aExceptionPyType) { 
}


template<class ExceptionType>
void pycohal::ExceptionTranslator<ExceptionType>::operator() (const ExceptionType& e) const {
  namespace bpy = boost::python;
  bpy::object pyException(bpy::handle<> (bpy::borrowed(mExceptionPyType)));

  // Add exception::what() string as ".what" attribute of Python exception
  pyException.attr("what") = e.what();

  PyErr_SetObject(mExceptionPyType, pyException.ptr());
}


template<class ExceptionType>
PyObject* pycohal::wrap_exception_class(const std::string& aExceptionName, PyObject* aBaseExceptionPyType)
{
  PyObject* exceptionPyType = pycohal::create_exception_class(aExceptionName, aBaseExceptionPyType);
  boost::python::register_exception_translator<ExceptionType>(pycohal::ExceptionTranslator<ExceptionType>(exceptionPyType));

  return exceptionPyType;
}


#endif
