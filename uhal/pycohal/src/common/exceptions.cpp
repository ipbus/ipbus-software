
#include "uhal/pycohal/exceptions.hpp"

// boost includes
#include "boost/python/extract.hpp"

// uhal includes
#include "uhal/log/exception.hpp"
#include "uhal/Node.hpp"
#include "uhal/ProtocolTCP.hpp"
#include "uhal/ProtocolUDP.hpp"
#include "uhal/ValMem.hpp"

using namespace pycohal;
namespace bpy = boost::python ;


PyObject* pycohal::create_exception_class(const std::string& aExceptionName, PyObject* aBaseExceptionPyType)
{
  std::string scopeName = bpy::extract<std::string> ( bpy::scope().attr ( "__name__" ) );
  std::string qualifiedExcName = scopeName + "." + aExceptionName;
  PyObject* typeObj = PyErr_NewException ( const_cast<char*> ( qualifiedExcName.c_str() ) , aBaseExceptionPyType, 0 );

  if ( !typeObj )
  {
    bpy::throw_error_already_set();
  }

  bpy::scope().attr ( aExceptionName.c_str() ) = bpy::handle<> ( bpy::borrowed ( typeObj ) );
  return typeObj;
}



void pycohal::wrap_exceptions() {
  // Base uHAL exception (fallback for derived exceptions not wrapped)
  PyObject* baseExceptionPyType = wrap_exception_class<uhal::exception::exception>("exception", PyExc_Exception);

  // Derived uHAL exceptions
  wrap_exception_class<uhal::exception::NonValidatedMemory> ( "NonValidatedMemory", baseExceptionPyType );
  wrap_exception_class<uhal::exception::BulkTransferRequestedTooLarge> ( "BulkTransferRequestedTooLarge", baseExceptionPyType );
  wrap_exception_class<uhal::exception::WriteAccessDenied> ( "WriteAccessDenied", baseExceptionPyType );
  wrap_exception_class<uhal::exception::ReadAccessDenied> ( "ReadAccessDenied",  baseExceptionPyType );
  wrap_exception_class<uhal::exception::BitsSetWhichAreForbiddenByBitMask> ( "BitsSetWhichAreForbiddenByBitMask", baseExceptionPyType );
  wrap_exception_class<uhal::exception::ValidationError> ( "ValidationError", baseExceptionPyType );
  wrap_exception_class<uhal::exception::TcpTimeout> ( "TcpTimeout", baseExceptionPyType );
  wrap_exception_class<uhal::exception::UdpTimeout> ( "UdpTimeout", baseExceptionPyType );
  wrap_exception_class<pycohal::PycohalLogLevelEnumError> ( "PycohalLogLevelEnumError", baseExceptionPyType );
}
