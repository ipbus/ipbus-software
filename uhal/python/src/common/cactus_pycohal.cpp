
// Boost includes
#include "boost/lexical_cast.hpp"
#include "boost/algorithm/string.hpp"

// pybind11
#include "pybind11/stl.h" // Automatically adds converters for STL collection/map classes
#include "pybind11/pybind11.h"

// uhal includes
#include "uhal/ClientFactory.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/ConnectionManager.hpp"
#include "uhal/ProtocolControlHub.hpp"
#include "uhal/ProtocolIPbus.hpp"
#include "uhal/ProtocolTCP.hpp"
#include "uhal/ProtocolUDP.hpp"
#include "uhal/SigBusGuard.hpp"
#include "uhal/Node.hpp"
#include "uhal/tests/tools.hpp"

// pycohal includes
#include "uhal/pycohal/enums_logging.hpp"
#include "uhal/pycohal/exceptions.hpp"


namespace py = pybind11;


namespace pycohal
{
  uint32_t defs_NOMASK()
  {
    return uhal::defs::NOMASK;
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

      for ( const auto& x: sVec )
      {
        result.push_back ( boost::lexical_cast<T> ( x ) );
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
#if PY_VERSION_HEX >= 0x03000000
  bool test_check_uint32_argument ( uint32_t aUInt, const std::string& uIntString )
#else
  bool test_check_uint32_argument ( uint32_t aUInt, const py::bytes& uIntString )
#endif
  {
    std::string stringFromUInt ( boost::lexical_cast<std::string> ( aUInt ) );
    return ( stringFromUInt == std::string(uIntString) );
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

  // Internally creates instance of SigBusGuard class - used to verify that SIGBUS is automatically blocked when uhal loaded
  void create_sigbus_buard()
  {
    const uhal::SigBusGuard lGuard;
  }

  /// Wraps functions that are only sed in unti tests. Puts them in "tests" sub-module.
  void wrap_test_functions(py::module_& aModule)
  {
    py::module_ lSubModule = aModule.def_submodule("tests");

    lSubModule.def ( "measureReadLatency", static_cast<double (*) ( uhal::ClientInterface&, uint32_t, uint32_t, size_t, bool, bool ) > ( &uhal::tests::measureReadLatency ) );
    lSubModule.def ( "measureWriteLatency", static_cast<double (*) ( uhal::ClientInterface&, uint32_t, uint32_t, size_t, bool, bool ) > ( &uhal::tests::measureWriteLatency ) );
    lSubModule.def ( "measureFileReadLatency", uhal::tests::measureFileReadLatency );
    lSubModule.def ( "measureFileWriteLatency", uhal::tests::measureFileWriteLatency );
    lSubModule.def ( "check_uint32_argument", pycohal::test_check_uint32_argument );
    lSubModule.def ( "convert_str_to_uint32", pycohal::test_convert_str_to_uint32 );
    lSubModule.def ( "convert_str_to_vec_str", pycohal::convert_string_to_vector<std::string> );
    lSubModule.def ( "convert_str_to_vec_uint32", pycohal::convert_string_to_vector<uint32_t> );
    lSubModule.def ( "copy_vec_uint32", pycohal::copy_vector<uint32_t> );
    lSubModule.def ( "get_dummy_ValWord", pycohal::get_dummy_ValWord );

    lSubModule.def ( "create_sigbus_guard", pycohal::create_sigbus_buard );
  }


  /* Typedefs for common return value policies */
  /// Default return value policy for const references returned attribute 'getter' methods
  const auto const_ref_return_policy = py::return_value_policy::copy;
  /// Return value policy for internal references.
  /// For member functions, it causes python garbage collector to keep 'self' (i.e. this) alive behind-the-scences as long as the returned object still exists.
  /// N.B: This return value policy is a safe option, but not necessarily the most optimal.
  const auto norm_ref_return_policy = py::return_value_policy::reference_internal;

  /// Constructs a ClientInterface using the ClientFactory
  std::shared_ptr<uhal::ClientInterface> buildClient(const std::string& aId, const std::string& aURI)
  {
    return uhal::ClientFactory::getInstance().getClient(aId, aURI);
  }

  /// Returns hex string for ValWord<uint32_t> value
  py::bytes hex_string ( const uhal::ValWord<uint32_t>& valWord )
  {
    std::ostringstream osstream;
    osstream << "0x" << std::hex << valWord.value();
    return osstream.str();
  }

  /// Struct containing wrapper functions for list-like indexing of ValVector<uint32_t> in python
  template <class T>
  struct ValVectorIndexingSuite
  {
    static const T& getItem ( const uhal::ValVector<T>& valVec, int i)
    {
      if (i >= int(valVec.size()))
        throw py::index_error();

      if (i < 0)
        i += valVec.size();

      if (i < 0)
        throw py::index_error();

      return valVec.at(i);
    }
    
    static std::vector<T> getSlice( const uhal::ValVector<T>& valVec, const py::slice aSlice ) 
    {
      size_t start, stop, step, slicelength;
      if (!aSlice.compute(valVec.size(), &start, &stop, &step, &slicelength))
        throw py::error_already_set();

      std::vector<T> lSliced(slicelength);
      for (size_t i = 0; i < slicelength; ++i) {
        lSliced.at(i) = valVec.at(start);
        start += step;
      }
      return lSliced;
    }
  };

  std::string convert_to_string( const uhal::ValWord<uint32_t>& valWord )
  {
    return boost::lexical_cast<std::string>(valWord.value());
  }

  std::string convert_to_string ( const uhal::ValVector<uint32_t>& valVec )
  {
    std::ostringstream lStream;

    lStream << "[";

    for ( size_t  i=0; i<valVec.size(); i++ )
    {
      lStream << valVec.at ( i );

      if ( i!= ( valVec.size()-1 ) )
      {
        lStream << ", ";
      }
    }

    lStream << "]";
    return lStream.str();
  }

}//namespace pycohal



inline py::object const& pass_through(py::object const& o)
{
  return o;
}

const uhal::Node& NextNodeConstIterator( uhal::Node::const_iterator& aIt )
{
  if ( aIt.next() )
    return *aIt;

  throw pybind11::stop_iteration();
}


// *** N.B: The argument of this BOOST_PYTHON_MODULE macro MUST be the same as the name of the library created, i.e. if creating library file my_py_binds_module.so , imported in python as:
//                import my_py_binds_module
//          then would have to put
//                BOOST_PYTHON_MODULE(my_py_binds_module)
//          Otherwise, will get the error message "ImportError: dynamic module does not define init function (initmy_py_binds_module)
PYBIND11_MODULE(_core, m)
{
  // Block SIGBUS, since required by SigBusGuard (used by mmap-based client)
  uhal::SigBusGuard::blockSIGBUS();

  m.def("NOMASK", pycohal::defs_NOMASK);

  // ENUMS
  pycohal::wrap_enums(m);

  // LOGGING FUNCTIONS
  pycohal::wrap_logging_functions(m);

  // EXCEPTIONS
  pycohal::wrap_exceptions(m);

  // TEST FUNCTIONS
  pycohal::wrap_test_functions(m);

  // Wrap uhal::ValHeader
  py::class_<uhal::ValHeader>(m, "ValHeader")
    .def (py::init< const uhal::ValHeader& >())
    .def ( "valid", static_cast< bool ( uhal::ValHeader::* ) () > ( &uhal::ValHeader::valid ) )
    ;

  // Wrap uhal::ValWord<uint32_t>
  py::class_<uhal::ValWord<uint32_t>>( m, "ValWord_uint32")
    .def ( py::init<>() )
    .def ( py::init< const uhal::ValWord<uint32_t>& >() )
    .def ( "valid", static_cast< bool ( uhal::ValWord<uint32_t>::* ) () > ( &uhal::ValWord<uint32_t>::valid ) )
    .def ( "value", static_cast< uint32_t ( uhal::ValWord<uint32_t>::* ) () const > ( &uhal::ValWord<uint32_t>::value ) )
    .def ( "mask",  static_cast< const uint32_t& ( uhal::ValWord<uint32_t>::* ) () const > ( &uhal::ValWord<uint32_t>::mask ) , pycohal::const_ref_return_policy )
    .def ( "__str__", static_cast< std::string (*) ( const uhal::ValWord<uint32_t>& ) > ( &pycohal::convert_to_string ) )
    .def ( "__int__", static_cast< uint32_t ( uhal::ValWord<uint32_t>::* ) () const> ( &uhal::ValWord<uint32_t>::value ) )
#if PY_VERSION_HEX >= 0x03000000
    .def ( "__index__", static_cast< uint32_t ( uhal::ValWord<uint32_t>::* ) () const> ( &uhal::ValWord<uint32_t>::value ) )
#else
    .def ( "__hex__", pycohal::hex_string )
#endif
    ;

  // Wrap uhal::ValVector<uint32_t>
  py::class_<uhal::ValVector<uint32_t>>(m, "ValVector_uint32", py::buffer_protocol())
    .def ( py::init<>() )
    .def ( py::init< const uhal::ValVector<uint32_t>& >() )
    .def ( "valid", static_cast< bool ( uhal::ValVector<uint32_t>::* ) () > ( &uhal::ValVector<uint32_t>::valid ) )
    .def ( "value", static_cast< std::vector<uint32_t> ( uhal::ValVector<uint32_t>::* ) () const > ( &uhal::ValVector<uint32_t>::value ) )
    .def ( "size",  &uhal::ValVector<uint32_t>::size )
    .def ( "at", &uhal::ValVector<uint32_t>::at, pycohal::const_ref_return_policy )
    .def ( "__str__", static_cast< std::string (*) ( const uhal::ValVector<uint32_t>& ) > ( &pycohal::convert_to_string ) )
    .def ( "__len__", &uhal::ValVector<uint32_t>::size )
    .def ( "__getitem__", &pycohal::ValVectorIndexingSuite<uint32_t>::getItem , pycohal::const_ref_return_policy )
    .def ( "__getitem__", &pycohal::ValVectorIndexingSuite<uint32_t>::getSlice )
    .def ( "__iter__", [](const uhal::ValVector<uint32_t>& v) { return py::make_iterator ( v.begin() , v.end() ); })
    .def_buffer([](uhal::ValVector<uint32_t> &v) -> py::buffer_info {
      const uint32_t* lData(v.valid() ? v.data() : nullptr);
      if (lData == nullptr) {
        return py::buffer_info();
      }
      return py::buffer_info(
        lData,     /* Pointer to buffer */
        v.size(),  /* Buffer size */
        true       /* Read-only */
      );
    })
    ;

  // Wrap uhal::Node
  py::class_<uhal::Node>(m, "Node")
    .def ( "getNode",         static_cast<const uhal::Node& ( uhal::Node::* ) ( const std::string& ) const>( &uhal::Node::getNode ), pycohal::norm_ref_return_policy )
    .def ( "getNodes",        static_cast<std::vector<std::string> ( uhal::Node::* ) ( const std::string& ) const>(&uhal::Node::getNodes) )
    .def ( "getNodes",        static_cast<std::vector<std::string> ( uhal::Node::* ) () const>(&uhal::Node::getNodes) )
    .def ( "getId",           &uhal::Node::getId,         pycohal::const_ref_return_policy )
    .def ( "getPath",         &uhal::Node::getPath )
    .def ( "getParameters",   &uhal::Node::getParameters, pycohal::const_ref_return_policy )
    .def ( "getFirmwareInfo", &uhal::Node::getFirmwareInfo, pycohal::const_ref_return_policy ) 
    .def ( "getAddress",      &uhal::Node::getAddress,    pycohal::const_ref_return_policy )
    .def ( "getMask",         &uhal::Node::getMask,       pycohal::const_ref_return_policy )
    .def ( "getMode",         &uhal::Node::getMode,       pycohal::const_ref_return_policy )
    .def ( "getSize",         &uhal::Node::getSize,       pycohal::const_ref_return_policy )
    .def ( "getPermission",   &uhal::Node::getPermission, pycohal::const_ref_return_policy )
    .def ( "getTags",         &uhal::Node::getTags,       pycohal::const_ref_return_policy )
    .def ( "getDescription",  &uhal::Node::getDescription, pycohal::const_ref_return_policy )
    .def ( "getModule",       &uhal::Node::getModule,     pycohal::const_ref_return_policy )
    .def ( "write",           &uhal::Node::write )
    .def ( "writeBlock",      &uhal::Node::writeBlock )
    .def ( "writeBlockOffset",&uhal::Node::writeBlockOffset )
    .def ( "read",            &uhal::Node::read )
    .def ( "readBlock",       &uhal::Node::readBlock )
    .def ( "readBlockOffset", &uhal::Node::readBlockOffset )
    .def ( "getClient",       &uhal::Node::getClient,     pycohal::norm_ref_return_policy )
    .def ( "__iter__", [](uhal::Node& n) { return py::make_iterator(n.begin(), n.end()); }, py::keep_alive<0, 1>())
    .def ( "__str__", &uhal::Node::getId, pycohal::const_ref_return_policy )
    ;

  py::class_< uhal::Node::const_iterator >(m, "NodeConstIterator")
    .def("next" , NextNodeConstIterator , pycohal::norm_ref_return_policy )
    .def("__iter__" , pass_through , pycohal::norm_ref_return_policy )
    ;

  // Wrap uhal::ClientInterface
  py::class_<uhal::ClientInterface, std::shared_ptr<uhal::ClientInterface>>(m, "ClientInterface")
    .def ( "id",     &uhal::ClientInterface::id, pycohal::const_ref_return_policy )
    .def ( "uri",    &uhal::ClientInterface::uri, pycohal::const_ref_return_policy )
    .def ( "write", static_cast<uhal::ValHeader ( uhal::ClientInterface::* ) ( const uint32_t&, const uint32_t& )> ( &uhal::ClientInterface::write ) )
    .def ( "write", static_cast<uhal::ValHeader ( uhal::ClientInterface::* ) ( const uint32_t&, const uint32_t&, const uint32_t& )> ( &uhal::ClientInterface::write ) )
    .def ( "read", static_cast<uhal::ValWord<uint32_t> ( uhal::ClientInterface::* ) ( const uint32_t& )> ( &uhal::ClientInterface::read) )
    .def ( "read", static_cast<uhal::ValWord<uint32_t> ( uhal::ClientInterface::* ) ( const uint32_t&, const uint32_t& )> ( &uhal::ClientInterface::read ) )
    .def ( "writeBlock", [](uhal::ClientInterface& c, const uint32_t& a, const std::vector< uint32_t >& v) { return c.writeBlock(a, v); } )
    .def ( "writeBlock", static_cast<uhal::ValHeader ( uhal::ClientInterface::* ) ( const uint32_t&, const std::vector< uint32_t >& , const uhal::defs::BlockReadWriteMode&)>( &uhal::ClientInterface::writeBlock ) )
    .def ( "readBlock", [](uhal::ClientInterface& c, const uint32_t& a, const uint32_t x) { return c.readBlock(a, x); } )
    .def ( "readBlock", static_cast<uhal::ValVector<uint32_t> ( uhal::ClientInterface::* ) ( const uint32_t&, const uint32_t&, const uhal::defs::BlockReadWriteMode& )>( &uhal::ClientInterface::readBlock ) )
    .def ( "rmw_bits", &uhal::ClientInterface::rmw_bits )
    .def ( "rmw_sum", &uhal::ClientInterface::rmw_sum )
    .def ( "dispatch", &uhal::ClientInterface::dispatch )
    .def ( "setTimeoutPeriod", &uhal::ClientInterface::setTimeoutPeriod )
    .def ( "getTimeoutPeriod", &uhal::ClientInterface::getTimeoutPeriod )
    .def ( "__str__", &uhal::ClientInterface::id, pycohal::const_ref_return_policy )
    ;

  py::class_<uhal::IPbusCore, uhal::ClientInterface, std::shared_ptr<uhal::IPbusCore>>(m, "IPbusCore")
    .def ( "readConfigurationSpace", static_cast<uhal::ValWord<uint32_t> ( uhal::IPbusCore::* ) ( const uint32_t& )>(&uhal::IPbusCore::readConfigurationSpace))
    .def ( "readConfigurationSpace", static_cast<uhal::ValWord<uint32_t> ( uhal::IPbusCore::* ) ( const uint32_t&, const uint32_t& )>(&uhal::IPbusCore::readConfigurationSpace))
    ;

  py::class_<uhal::UDP<uhal::IPbus<1, 3>>, uhal::IPbusCore, std::shared_ptr<uhal::UDP<uhal::IPbus<1, 3>>>>(m, "_UDP_IPbus_1_3");
  py::class_<uhal::UDP<uhal::IPbus<2, 0>>, uhal::IPbusCore, std::shared_ptr<uhal::UDP<uhal::IPbus<2, 0>>>>(m, "_UDP_IPbus_2_0");

  py::class_<uhal::TCP<uhal::IPbus<1, 3>, 1>, uhal::IPbusCore, std::shared_ptr<uhal::TCP<uhal::IPbus<1, 3>, 1>>>(m, "_TCP_IPbus_1_3");
  py::class_<uhal::TCP<uhal::IPbus<2, 0>, 1>, uhal::IPbusCore, std::shared_ptr<uhal::TCP<uhal::IPbus<2, 0>, 1>>>(m, "_TCP_IPbus_2_0");

  py::class_<uhal::TCP<uhal::ControlHub<uhal::IPbus<1, 3> >, 3>, uhal::IPbusCore, std::shared_ptr<uhal::TCP<uhal::ControlHub<uhal::IPbus<1, 3> >, 3>>>(m, "_TCP_ControlHub_IPbus_1_3");
  py::class_<uhal::TCP<uhal::ControlHub<uhal::IPbus<2, 0>>, 3>, uhal::IPbusCore, std::shared_ptr<uhal::TCP<uhal::ControlHub<uhal::IPbus<2, 0> >, 3>>>(m, "_TCP_ControlHub_IPbus_2_0");

  m.def ( "buildClient", pycohal::buildClient );

  // Wrap uhal::HwInterface
  py::class_<uhal::HwInterface> (m, "HwInterface")
    .def ( py::init<const uhal::HwInterface&>() )
    .def ( "getClient", &uhal::HwInterface::getClient, pycohal::norm_ref_return_policy )
    .def ( "uri", &uhal::HwInterface::uri, pycohal::const_ref_return_policy )
    .def ( "id",  &uhal::HwInterface::id, pycohal::const_ref_return_policy )
    .def ( "dispatch", &uhal::HwInterface::dispatch )
    .def ( "setTimeoutPeriod", &uhal::HwInterface::setTimeoutPeriod )
    .def ( "getTimeoutPeriod", &uhal::HwInterface::getTimeoutPeriod )
    .def ( "getNode", static_cast< const uhal::Node& ( uhal::HwInterface::* ) () const > ( &uhal::HwInterface::getNode ), pycohal::norm_ref_return_policy )
    .def ( "getNode", static_cast< const uhal::Node& ( uhal::HwInterface::* ) ( const std::string& ) const > ( &uhal::HwInterface::getNode ), pycohal::norm_ref_return_policy )
    .def ( "getNodes", static_cast< std::vector<std::string> ( uhal::HwInterface::* ) () const > ( &uhal::HwInterface::getNodes ) )
    .def ( "getNodes", static_cast< std::vector<std::string> ( uhal::HwInterface::* ) ( const std::string& ) const > ( &uhal::HwInterface::getNodes ) )
    .def ( "__str__", &uhal::HwInterface::id, pycohal::const_ref_return_policy )
    ;

  // Wrap uhal::ConnectionManager
  py::class_< uhal::ConnectionManager > (m, "ConnectionManager")
    .def ( py::init<const std::string&>() )
    .def ( py::init<const std::string&, const std::vector<std::string>&>() )
    .def ( "getDevice", static_cast< uhal::HwInterface ( uhal::ConnectionManager::* ) ( const std::string& ) > ( &uhal::ConnectionManager::getDevice ) )
    .def ( "getDevices", static_cast< std::vector<std::string> ( uhal::ConnectionManager::* ) ()                   const > ( &uhal::ConnectionManager::getDevices ) )
    .def ( "getDevices", static_cast< std::vector<std::string> ( uhal::ConnectionManager::* ) ( const std::string& ) const > ( &uhal::ConnectionManager::getDevices ) )
    .def_static ( "clearAddressFileCache", &uhal::ConnectionManager::clearAddressFileCache )
    ;

  m.def ( "getDevice", static_cast<uhal::HwInterface (* ) ( const std::string&, const std::string&, const std::string& ) > ( &uhal::ConnectionManager::getDevice ) );
  m.def ( "getDevice", static_cast<uhal::HwInterface (* ) ( const std::string&, const std::string&, const std::string&, const std::vector<std::string>& ) > ( &uhal::ConnectionManager::getDevice ) );
}

