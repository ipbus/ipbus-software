
#include "uhal/pycohal/converters_exceptions.hpp"

#include "uhal/ClientInterface.hpp"
#include "uhal/Node.hpp"
#include "uhal/ProtocolTCP.hpp"
#include "uhal/ProtocolUDP.hpp"
#include "uhal/ValMem.hpp"


namespace bpy = boost::python ;

//-----------------------------------------//
// ---  Converter_vec_uint32_from_list  ---//
//-----------------------------------------//


void* pycohal::Converter_vec_uint32_from_list::convertible ( PyObject* obj_ptr )
{
  if ( !PyList_Check ( obj_ptr ) )
  {
    return 0;
  }
  else
  {
    return obj_ptr;
  }
}


void pycohal::Converter_vec_uint32_from_list::construct ( PyObject* obj_ptr, bpy::converter::rvalue_from_python_stage1_data* data )
{
  // Grab pointer to memory in which to construct the new vector
  void* storage = ( ( bpy::converter::rvalue_from_python_storage< std::vector<uint32_t> >* ) data )->storage.bytes;
  // Grab list object from obj_ptr
  bpy::list py_list ( bpy::handle<> ( bpy::borrowed ( obj_ptr ) ) );
  // Construct vector in requested location, and set element values.
  // boost::python::extract will throw appropriate exception if can't convert to type T ; boost::python will then call delete itself as well.
  size_t nItems = bpy::len ( py_list );
  std::vector<uint32_t>* vec_ptr = new ( storage ) std::vector<uint32_t> ( nItems );

  for ( size_t i=0; i<nItems; i++ )
  {
    vec_ptr->at ( i ) = bpy::extract<uint32_t> ( py_list[i] );
  }

  // If successful, then register memory chunk pointer for later use by boost.python
  data->convertible = storage;
}


void pycohal::register_converters()
{
  pycohal::Converter_vec_uint32_from_list();
  bpy::to_python_converter< std::vector<std::string>, pycohal::Converter_std_vector_to_list<std::string> >();
  bpy::to_python_converter< std::vector<uint32_t>, pycohal::Converter_std_vector_to_list<uint32_t> >();
  bpy::to_python_converter< boost::unordered_map<std::string, std::string>, pycohal::Converter_boost_unorderedmap_to_dict<std::string,std::string> >(); 

}


PyObject* pycohal::create_exception_class ( const std::string& excName, PyObject* baseTypeObj )
{
  std::string scopeName = bpy::extract<std::string> ( bpy::scope().attr ( "__name__" ) );
  std::string qualifiedExcName = scopeName + "." + excName;
  PyObject* typeObj = PyErr_NewException ( const_cast<char*> ( qualifiedExcName.c_str() ) , baseTypeObj, 0 );

  if ( !typeObj )
  {
    bpy::throw_error_already_set();
  }

  bpy::scope().attr ( excName.c_str() ) = bpy::handle<> ( bpy::borrowed ( typeObj ) );
  return typeObj;
}


void pycohal::wrap_exceptions()
{
  // Base uhal exception (fallback for derived exceptions not wrapped)
  PyObject* base_uhal_exception_pyType = pycohal::create_exception_class ( "exception" );
  bpy::register_exception_translator<uhal::exception::exception> ( pycohal::ExceptionTranslator<uhal::exception::exception> ( base_uhal_exception_pyType ) );
  // Derived uhal exceptions
  pycohal::wrap_derived_exception<uhal::exception::NonValidatedMemory> ( "NonValidatedMemory", base_uhal_exception_pyType );
  pycohal::wrap_derived_exception<uhal::exception::BulkTransferRequestedTooLarge> ( "BulkTransferRequestedTooLarge", base_uhal_exception_pyType );
  pycohal::wrap_derived_exception<uhal::exception::WriteAccessDenied> ( "WriteAccessDenied", base_uhal_exception_pyType );
  pycohal::wrap_derived_exception<uhal::exception::ReadAccessDenied> ( "ReadAccessDenied",  base_uhal_exception_pyType );
  pycohal::wrap_derived_exception<uhal::exception::BitsSetWhichAreForbiddenByBitMask> ( "BitsSetWhichAreForbiddenByBitMask", base_uhal_exception_pyType );
  pycohal::wrap_derived_exception<uhal::exception::TcpTimeout> ( "TcpTimeout", base_uhal_exception_pyType );
  pycohal::wrap_derived_exception<uhal::exception::UdpTimeout> ( "UdpTimeout", base_uhal_exception_pyType );
  pycohal::wrap_derived_exception<pycohal::PycohalLogLevelEnumError> ( "PycohalLogLevelEnumError", base_uhal_exception_pyType );
}


