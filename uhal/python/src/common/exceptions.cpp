
#include "uhal/pycohal/exceptions.hpp"

#include "pybind11/pybind11.h"

// uhal includes
#include "uhal/log/exception.hpp"
#include "uhal/Node.hpp"
#include "uhal/ProtocolTCP.hpp"
#include "uhal/ProtocolUDP.hpp"
#include "uhal/ValMem.hpp"


namespace py = pybind11;


void pycohal::wrap_exceptions(pybind11::module_& aModule)
{
  // Base uHAL exception (fallback for derived exceptions not wrapped)
  auto baseException = py::register_exception<uhal::exception::exception>(aModule, "exception", PyExc_Exception);

  // Derived uHAL exceptions
  py::register_exception<uhal::exception::NonValidatedMemory> ( aModule, "NonValidatedMemory", baseException );
  py::register_exception<uhal::exception::BulkTransferRequestedTooLarge> ( aModule, "BulkTransferRequestedTooLarge", baseException );
  py::register_exception<uhal::exception::WriteAccessDenied> ( aModule, "WriteAccessDenied", baseException );
  py::register_exception<uhal::exception::ReadAccessDenied> ( aModule, "ReadAccessDenied",  baseException );
  py::register_exception<uhal::exception::BitsSetWhichAreForbiddenByBitMask> ( aModule, "BitsSetWhichAreForbiddenByBitMask", baseException );
  py::register_exception<uhal::exception::ValidationError> ( aModule, "ValidationError", baseException );
  py::register_exception<uhal::exception::TcpTimeout> ( aModule, "TcpTimeout", baseException );
  py::register_exception<uhal::exception::UdpTimeout> ( aModule, "UdpTimeout", baseException );
  py::register_exception<pycohal::PycohalLogLevelEnumError> ( aModule, "PycohalLogLevelEnumError", baseException );
}
