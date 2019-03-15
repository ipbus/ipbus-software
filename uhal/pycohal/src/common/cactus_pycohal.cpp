// The following include is required to compile on apple
#include "uhal/pycohal/boost_python.hpp"


// Boost includes
#include "boost/lexical_cast.hpp"
#include "boost/algorithm/string.hpp"
#include "boost/python/module.hpp"
#include "boost/python/def.hpp"
#include "boost/python/suite/indexing/vector_indexing_suite.hpp"
#include "boost/python/wrapper.hpp"
#include "boost/python/slice.hpp"

// uhal includes
#include "uhal/ClientFactory.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/ConnectionManager.hpp"
#include "uhal/ProtocolControlHub.hpp"
#include "uhal/ProtocolIPbus.hpp"
#include "uhal/ProtocolTCP.hpp"
#include "uhal/ProtocolUDP.hpp"
#include "uhal/Node.hpp"
#include "uhal/tests/tools.hpp"

// pycohal includes
#include "uhal/pycohal/converters.hpp"
#include "uhal/pycohal/enums_logging.hpp"
#include "uhal/pycohal/exceptions.hpp"
#include "uhal/pycohal/val_mem.hpp"


using namespace boost::python;

namespace pycohal
{
  uint32_t defs_NOMASK()
  {
    return uhal::defs::NOMASK;
  }

  uint64_t defs_NOMASK64()
  {
    return uhal::defs::NOMASK64;
  }

  /// Trivial function that returns copy of vector argument in order to test vector to/from list converters. [Function solely used in unit-tests]
  template<class T>
  std::vector<T> copy_vector ( const std::vector<T>& aVec )
  {
    return std::vector<T> ( aVec );
  }

  /// Returns vector, generated from splitting string argument using "," as delimeter, and converting string to type via lexical cast. [Function solely used in unit-tests]
  template < class T >
  std::vector<T> convert_string_to_vector ( const std::string& aString )
  {
    std::vector<std::string> sVec;
    std::vector<T> result;

    if ( ! aString.empty() )
    {
      boost::algorithm::split ( sVec, aString, boost::algorithm::is_any_of ( "," ) );
      result.reserve ( sVec.size() );

      for ( std::vector<std::string>::const_iterator it = sVec.begin(); it != sVec.end(); it++ )
      {
        result.push_back ( boost::lexical_cast<T> ( *it ) );
      }
    }

    return result;
  }

  template <>
  std::vector<std::string> convert_string_to_vector ( const std::string& aString )
  {
    std::vector<std::string> result;

    if ( ! aString.empty() )
    {
      boost::algorithm::split ( result, aString, boost::algorithm::is_any_of ( "," ) );
    }

    return result;
  }

  /// Returns whether uint32 and string arguments represent the same nubmer. Used in tests to check that uint32 arguments don't get altered at python-to-C boundary.
  bool test_check_uint32_argument ( uint32_t aUInt, const std::string& uIntString )
  {
    std::string stringFromUInt ( boost::lexical_cast<std::string> ( aUInt ) );
    return ( stringFromUInt==uIntString );
  }

  /// Converts string argument to uint32_t, and returns this value. Used in tests to check that uint32 return values don't get altered at C-to-python boundary.
  uint32_t test_convert_str_to_uint32 ( const std::string& uIntString )
  {
    return boost::lexical_cast<uint32_t> ( uIntString );
  }

  /// Returns dummy ValWord instance for testing.
  uhal::ValWord<uint32_t> get_dummy_ValWord(const uint32_t aValue, const bool aValid)
  {
    uhal::ValWord<uint32_t> valWord;
    valWord.value( aValue );
    valWord.valid( aValid );
    return valWord;
  }

  /// Wraps functions that are only sed in unti tests. Puts them in "tests" sub-module.
  void wrap_test_functions()
  {
    namespace bpy = boost::python;
    bpy::scope packageScope;
    std::string testModuleName ( bpy::extract<const char*> ( packageScope.attr ( "__name__" ) ) );
    testModuleName += ".tests";
    char* testModuleCstr = new char [testModuleName.size() +1];
    strcpy ( testModuleCstr, testModuleName.c_str() );
    // Make test sub-module ...
    bpy::object testModule ( bpy::handle<> ( bpy::borrowed ( PyImport_AddModule ( testModuleCstr ) ) ) ); //< Enables "from mypackage.tests import <whatever>"
    packageScope.attr ( "tests" ) = testModule; //< Enables "from mypackage import tests"
    // Change to sub-module scope ...
    bpy::scope testScope = testModule;

    // Wrap the test functions ...
    bpy::def ( "measureReadLatency", static_cast<double (*) ( uhal::ClientInterface&, uint32_t, uint32_t, size_t, bool, bool ) > ( &uhal::tests::measureReadLatency ) );
    bpy::def ( "measureWriteLatency", static_cast<double (*) ( uhal::ClientInterface&, uint32_t, uint32_t, size_t, bool, bool ) > ( &uhal::tests::measureWriteLatency ) );
    bpy::def ( "check_uint32_argument", pycohal::test_check_uint32_argument );
    bpy::def ( "convert_str_to_uint32", pycohal::test_convert_str_to_uint32 );
    bpy::def ( "convert_str_to_vec_str", pycohal::convert_string_to_vector<std::string> );
    bpy::def ( "convert_str_to_vec_uint32", pycohal::convert_string_to_vector<uint32_t> );
    bpy::def ( "copy_vec_uint32", pycohal::copy_vector<uint32_t> );
    bpy::def ( "get_dummy_ValWord", pycohal::get_dummy_ValWord );
  }


  /* Typedefs for common return value policies */
  /// Default return value policy for const references returned attribute 'getter' methods
  typedef return_value_policy<copy_const_reference> const_ref_return_policy ;
  /// Return value policy for internal references.
  /// For member functions, it causes python garbage collector to keep 'self' (i.e. this) alive behind-the-scences as long as the returned object still exists.
  /// N.B: This return value policy is a safe option, but not necessarily the most optimal.
  typedef return_internal_reference<> norm_ref_return_policy ;

  /// Constructs a ClientInterface using the ClientFactory
  boost::shared_ptr<uhal::ClientInterface> buildClient(const std::string& aId, const std::string& aURI)
  {
    return uhal::ClientFactory::getInstance().getClient(aId, aURI);
  }

}//namespace pycohal



inline object const& pass_through(object const& o)
{ return o; }

const uhal::Node& NextNodeConstIterator( uhal::Node::const_iterator& aIt )
{
  if ( aIt.next() ) {
    return *aIt;
  }

  PyErr_SetString(PyExc_StopIteration, "No more data.");
  boost::python::throw_error_already_set();  
}


BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_Node_getNodes_overloads, getNodes, 0, 1 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_write_overloads,        write,        2, 3 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_write32_overloads,      write32,      2, 3 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_write64_overloads,      write64,      2, 3 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_writeBlock_overloads,   writeBlock,   2, 3 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_writeBlock32_overloads, writeBlock32, 2, 3 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_writeBlock64_overloads, writeBlock64, 2, 3 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_read_overloads,         read,         1, 2 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_read32_overloads,       read32,       1, 2 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_read64_overloads,       read64,       1, 2 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_readBlock_overloads,    readBlock,    2, 3 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_readBlock32_overloads,  readBlock32,  2, 3 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_ClientInterface_readBlock64_overloads,  readBlock64,  2, 3 )
BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS ( uhal_IPbusCore_readConfigurationSpace_overloads, readConfigurationSpace, 1, 2 )


// *** N.B: The argument of this BOOST_PYTHON_MODULE macro MUST be the same as the name of the library created, i.e. if creating library file my_py_binds_module.so , imported in python as:
//                import my_py_binds_module
//          then would have to put
//                BOOST_PYTHON_MODULE(my_py_binds_module)
//          Otherwise, will get the error message "ImportError: dynamic module does not define init function (initmy_py_binds_module)
BOOST_PYTHON_MODULE ( _core )
{
  def ( "NOMASK", pycohal::defs_NOMASK );
  def ( "NOMASK64", pycohal::defs_NOMASK64 );
  // ENUMS
  pycohal::wrap_enums();
  // LOGGING FUNCTIONS
  pycohal::wrap_logging_functions();
  // CONVERTERS
  pycohal::register_converters();
  // EXCEPTIONS
  pycohal::wrap_exceptions();
  // TEST FUNCTIONS
  pycohal::wrap_test_functions();

  pycohal::wrapValMem();

  // Wrap uhal::Node
  class_<uhal::Node, boost::noncopyable /*since no copy CTOR*/ > ( "Node", no_init )
  .def ( "getNode",           static_cast< const uhal::Node& ( uhal::Node::* ) ( const std::string& ) const > ( &uhal::Node::getNode ), pycohal::norm_ref_return_policy() )
  .def ( "getNodes",          ( std::vector<std::string> ( uhal::Node::* ) ( const std::string& ) ) 0, uhal_Node_getNodes_overloads() )
  .def ( "getId",             &uhal::Node::getId,         pycohal::const_ref_return_policy() )
  .def ( "getPath",           &uhal::Node::getPath )
  .def ( "getParameters",     &uhal::Node::getParameters, pycohal::const_ref_return_policy() )
  .def ( "getFirmwareInfo",   &uhal::Node::getFirmwareInfo, pycohal::const_ref_return_policy() ) 
  .def ( "getAddress",        &uhal::Node::getAddress,    pycohal::const_ref_return_policy() )
  .def ( "getMask",           &uhal::Node::getMask,       pycohal::const_ref_return_policy() )
  .def ( "getMode",           &uhal::Node::getMode,       pycohal::const_ref_return_policy() )
  .def ( "getSize",           &uhal::Node::getSize,       pycohal::const_ref_return_policy() )
  .def ( "getPermission",     &uhal::Node::getPermission, pycohal::const_ref_return_policy() )
  .def ( "getTags",           &uhal::Node::getTags,       pycohal::const_ref_return_policy() )
  .def ( "getDescription",    &uhal::Node::getDescription, pycohal::const_ref_return_policy() )
  .def ( "getModule",         &uhal::Node::getModule,     pycohal::const_ref_return_policy() )
  .def ( "write",             &uhal::Node::write )
  .def ( "write32",           &uhal::Node::write32 )
  .def ( "write64",           &uhal::Node::write64 )
  .def ( "writeBlock",        &uhal::Node::writeBlock )
  .def ( "writeBlock32",      &uhal::Node::writeBlock32 )
  .def ( "writeBlock64",      &uhal::Node::writeBlock64 )
  .def ( "writeBlockOffset",  &uhal::Node::writeBlockOffset )
  .def ( "writeBlockOffset32",&uhal::Node::writeBlockOffset32 )
  .def ( "writeBlockOffset64",&uhal::Node::writeBlockOffset64 )
  .def ( "read",              &uhal::Node::read )
  .def ( "read32",            &uhal::Node::read32 )
  .def ( "read64",            &uhal::Node::read64 )
  .def ( "readBlock",         &uhal::Node::readBlock )
  .def ( "readBlock32",       &uhal::Node::readBlock32 )
  .def ( "readBlock64",       &uhal::Node::readBlock64 )
  .def ( "readBlockOffset",   &uhal::Node::readBlockOffset )
  .def ( "readBlockOffset32", &uhal::Node::readBlockOffset32 )
  .def ( "readBlockOffset64", &uhal::Node::readBlockOffset64 )
  .def ( "getClient",         &uhal::Node::getClient,     pycohal::norm_ref_return_policy() )
  .def ( "__iter__"   ,       range< pycohal::norm_ref_return_policy >(&uhal::Node::begin, &uhal::Node::end) )
  .def ( "__str__", &uhal::Node::getId, pycohal::const_ref_return_policy() )
  ;

  class_< uhal::Node::const_iterator >( "NodeConstIterator" , no_init )
  .def("next" , NextNodeConstIterator , pycohal::norm_ref_return_policy() )
  .def("__iter__" , pass_through , pycohal::norm_ref_return_policy() )
  ;

  // Wrap uhal::ClientInterface
  class_<uhal::ClientInterface, boost::noncopyable /* no to-python converter (would require a copy CTOR) */,
         boost::shared_ptr<uhal::ClientInterface> /* all instances are held within boost::shared_ptr */> ( "ClientInterface", no_init /* no CTORs */ )
         .def ( "id",     &uhal::ClientInterface::id,   pycohal::const_ref_return_policy() )
         .def ( "uri",    &uhal::ClientInterface::uri )
         .def ( "write",   ( uhal::ValHeader ( uhal::ClientInterface::* ) ( const uint32_t&, const uint32_t&, const uint32_t& ) ) 0, uhal_ClientInterface_write_overloads() )
         .def ( "write32", ( uhal::ValHeader ( uhal::ClientInterface::* ) ( const uint32_t&, const uint32_t&, const uint32_t& ) ) 0, uhal_ClientInterface_write32_overloads() )
         .def ( "write64", ( uhal::ValHeader ( uhal::ClientInterface::* ) ( const uint32_t&, const uint64_t&, const uint64_t& ) ) 0, uhal_ClientInterface_write64_overloads() )
         .def ( "read",   ( uhal::ValWord<uint32_t> ( uhal::ClientInterface::* ) ( const uint32_t&, const uint32_t& ) ) 0,           uhal_ClientInterface_read_overloads() )
         .def ( "read32", ( uhal::ValWord<uint32_t> ( uhal::ClientInterface::* ) ( const uint32_t&, const uint32_t& ) ) 0,           uhal_ClientInterface_read32_overloads() )
         .def ( "read64", ( uhal::ValWord<uint64_t> ( uhal::ClientInterface::* ) ( const uint32_t&, const uint64_t& ) ) 0,           uhal_ClientInterface_read64_overloads() )
         .def ( "writeBlock",   &uhal::ClientInterface::writeBlock,   uhal_ClientInterface_writeBlock_overloads() )
         .def ( "writeBlock32", &uhal::ClientInterface::writeBlock32, uhal_ClientInterface_writeBlock32_overloads() )
         .def ( "writeBlock64", &uhal::ClientInterface::writeBlock64, uhal_ClientInterface_writeBlock64_overloads() )
         .def ( "readBlock",    &uhal::ClientInterface::readBlock,    uhal_ClientInterface_readBlock_overloads() )
         .def ( "readBlock32",  &uhal::ClientInterface::readBlock32,  uhal_ClientInterface_readBlock32_overloads() )
         .def ( "readBlock64",  &uhal::ClientInterface::readBlock64,  uhal_ClientInterface_readBlock64_overloads() )
         .def ( "rmw_bits",   &uhal::ClientInterface::rmw_bits )
         .def ( "rmw_bits32", &uhal::ClientInterface::rmw_bits32 )
         .def ( "rmw_bits64", &uhal::ClientInterface::rmw_bits64 )
         .def ( "rmw_sum",   &uhal::ClientInterface::rmw_sum )
         .def ( "rmw_sum32", &uhal::ClientInterface::rmw_sum32 )
         .def ( "rmw_sum64", &uhal::ClientInterface::rmw_sum64 )
         .def ( "dispatch", &uhal::ClientInterface::dispatch )
         .def ( "setTimeoutPeriod", &uhal::ClientInterface::setTimeoutPeriod )
         .def ( "getTimeoutPeriod", &uhal::ClientInterface::getTimeoutPeriod )
         .def ( "__str__", &uhal::ClientInterface::id, pycohal::const_ref_return_policy() )
         ;

  class_<uhal::IPbusCore, bases<uhal::ClientInterface>, boost::noncopyable /* no to-python converter (would require a copy CTOR) */,
         boost::shared_ptr<uhal::IPbusCore> /* all instances are held within boost::shared_ptr */ >("IPbusCore", no_init /* no CTORs */)
         .def ( "readConfigurationSpace", ( uhal::ValWord<uint32_t> ( uhal::IPbusCore::* ) ( const uint32_t&, const uint32_t& ) ) 0, uhal_IPbusCore_readConfigurationSpace_overloads() )
         ;

  class_<uhal::UDP<uhal::IPbus<1, 3> >, bases<uhal::IPbusCore>,
         boost::noncopyable, /* no to-python converter (would require a copy CTOR) */
         boost::shared_ptr< uhal::UDP<uhal::IPbus<1, 3> > > // /* all instances are held within boost::shared_ptr */
          >("_UDP_IPbus_1_3", no_init /* no CTORs */);

  class_<uhal::UDP<uhal::IPbus<2, 0> >, bases<uhal::IPbusCore>,
         boost::noncopyable, /* no to-python converter (would require a copy CTOR) */
         boost::shared_ptr< uhal::UDP<uhal::IPbus<2, 0> > > // /* all instances are held within boost::shared_ptr */
          >("_UDP_IPbus_2_0", no_init /* no CTORs */);

  class_<uhal::UDP<uhal::IPbus<3, 0> >, bases<uhal::IPbusCore>,
         boost::noncopyable, /* no to-python converter (would require a copy CTOR) */
         boost::shared_ptr< uhal::UDP<uhal::IPbus<3, 0> > > // /* all instances are held within boost::shared_ptr */
          >("_UDP_IPbus_3_0", no_init /* no CTORs */);

  class_<uhal::TCP<uhal::IPbus<1, 3>, 1>, bases<uhal::IPbusCore>,
         boost::noncopyable, /* no to-python converter (would require a copy CTOR) */
         boost::shared_ptr< uhal::TCP<uhal::IPbus<1, 3>, 1> > // /* all instances are held within boost::shared_ptr */
          >("_TCP_IPbus_1_3", no_init /* no CTORs */);

  class_<uhal::TCP<uhal::IPbus<2, 0>, 1>, bases<uhal::IPbusCore>,
         boost::noncopyable, /* no to-python converter (would require a copy CTOR) */
         boost::shared_ptr< uhal::TCP<uhal::IPbus<2, 0>, 1> > // /* all instances are held within boost::shared_ptr */
          >("_TCP_IPbus_2_0", no_init /* no CTORs */);

  class_<uhal::TCP<uhal::ControlHub<uhal::IPbus<1, 3> >, 3>, bases<uhal::IPbusCore>,
         boost::noncopyable, /* no to-python converter (would require a copy CTOR) */
         boost::shared_ptr< uhal::TCP<uhal::ControlHub<uhal::IPbus<1, 3> >, 3> > // /* all instances are held within boost::shared_ptr */
          >("_TCP_ControlHub_IPbus_1_3", no_init /* no CTORs */);

  class_<uhal::TCP<uhal::ControlHub<uhal::IPbus<2, 0> >, 3>, bases<uhal::IPbusCore>,
         boost::noncopyable, /* no to-python converter (would require a copy CTOR) */
         boost::shared_ptr< uhal::TCP<uhal::ControlHub<uhal::IPbus<2, 0> >, 3> > // /* all instances are held within boost::shared_ptr */
          >("_TCP_ControlHub_IPbus_2_0", no_init /* no CTORs */);

  def ( "buildClient", pycohal::buildClient );

  // Wrap uhal::HwInterface
  class_<uhal::HwInterface> ( "HwInterface", init<const uhal::HwInterface&>() )
  .def ( "getClient", &uhal::HwInterface::getClient, pycohal::norm_ref_return_policy() )
  .def ( "uri", &uhal::HwInterface::uri )
  .def ( "id",  &uhal::HwInterface::id, pycohal::const_ref_return_policy() )
  .def ( "dispatch", &uhal::HwInterface::dispatch )
  .def ( "setTimeoutPeriod", &uhal::HwInterface::setTimeoutPeriod )
  .def ( "getTimeoutPeriod", &uhal::HwInterface::getTimeoutPeriod )
  .def ( "getNode", static_cast< const uhal::Node& ( uhal::HwInterface::* ) () const > ( &uhal::HwInterface::getNode ), pycohal::norm_ref_return_policy() )
  .def ( "getNode", static_cast< const uhal::Node& ( uhal::HwInterface::* ) ( const std::string& ) const > ( &uhal::HwInterface::getNode ), pycohal::norm_ref_return_policy() )
  .def ( "getNodes", static_cast< std::vector<std::string> ( uhal::HwInterface::* ) () const > ( &uhal::HwInterface::getNodes ) )
  .def ( "getNodes", static_cast< std::vector<std::string> ( uhal::HwInterface::* ) ( const std::string& ) const > ( &uhal::HwInterface::getNodes ) )
  .def ( "__str__", &uhal::HwInterface::id, pycohal::const_ref_return_policy() )
  ;
  // Wrap uhal::ConnectionManager
  class_< uhal::ConnectionManager, boost::noncopyable > ( "ConnectionManager", init<const std::string&>() )
  .def ( "getDevice",  static_cast< uhal::HwInterface ( uhal::ConnectionManager::* ) ( const std::string& ) > ( &uhal::ConnectionManager::getDevice ) )
  .def ( "getDevices", static_cast< std::vector<std::string> ( uhal::ConnectionManager::* ) ()                   const > ( &uhal::ConnectionManager::getDevices ) )
  .def ( "getDevices", static_cast< std::vector<std::string> ( uhal::ConnectionManager::* ) ( const std::string& ) const > ( &uhal::ConnectionManager::getDevices ) )
  .def ( "clearAddressFileCache", &uhal::ConnectionManager::clearAddressFileCache )
  .staticmethod("clearAddressFileCache");
  def ( "getDevice", static_cast<uhal::HwInterface (* ) ( const std::string&, const std::string&, const std::string& ) > ( &uhal::ConnectionManager::getDevice ) );
}

