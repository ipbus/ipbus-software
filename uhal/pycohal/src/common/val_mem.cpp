
#include "uhal/pycohal/val_mem.hpp"


// The following python boost patch is required to compile on apple
#include "uhal/pycohal/boost_python.hpp"

#include <boost/python/class.hpp>
#include <boost/python/return_value_policy.hpp>
#include <boost/python/return_internal_reference.hpp>


namespace pycohal
{
  /* Typedefs for common return value policies */
  /// Default return value policy for const references returned attribute 'getter' methods
  typedef boost::python::return_value_policy<boost::python::copy_const_reference> const_ref_return_policy ;
  /// Return value policy for internal references.
  /// For member functions, it causes python garbage collector to keep 'self' (i.e. this) alive behind-the-scences as long as the returned object still exists.
  /// N.B: This return value policy is a safe option, but not necessarily the most optimal.
  typedef boost::python::return_internal_reference<> norm_ref_return_policy ;


  template <typename T>
  std::string getHexString ( const uhal::ValWord<T>& aValWord )
  {
    std::ostringstream osstream;
    osstream << "0x" << std::hex << aValWord.value();
    return osstream.str();
  }


  template <typename T>
  std::string getString( const uhal::ValWord<T>& aValWord )
  {
    return boost::lexical_cast<std::string>(aValWord.value());
  }


  template <typename T>
  std::string getString ( const uhal::ValVector<T>& aValVec )
  {
    std::ostringstream lStream;

    lStream << "[";

    for ( size_t  i=0; i<aValVec.size(); i++ )
    {
      lStream << aValVec.at ( i );

      if ( i!= ( aValVec.size()-1 ) )
      {
        lStream << ", ";
      }
    }

    lStream << "]";
    return lStream.str();
  }


  template <class T>
  void ValVectorIndexingSuite<T>::raiseIndexError()
  {
    PyErr_SetString ( PyExc_IndexError, "Index out of range" );
    boost::python::throw_error_already_set();
  }


  template <class T>
  const T& ValVectorIndexingSuite<T>::getItem ( const uhal::ValVector<T>& valVec, int i )
  {
    if ( i<0 )
    {
      i += valVec.size();
    }

    if ( i<0 || i>=int ( valVec.size() ) )
    {
      raiseIndexError();
    }

    return valVec.at ( i );
  }


  template <class T>
  std::vector<T> ValVectorIndexingSuite<T>::getSlice( const uhal::ValVector<T>& valVec, const boost::python::slice aSlice ) 
  {
    boost::python::slice::range<typename std::vector<T>::const_iterator> bounds;
    try {
      bounds = aSlice.get_indicies<>(valVec.begin(), valVec.end());
    }
    catch (std::invalid_argument) {
      return std::vector<T>();
    }

    std::vector<T> lSliced;
    while (bounds.start != bounds.stop) {
      lSliced.push_back(*bounds.start);
      std::advance( bounds.start, bounds.step);
    }
    lSliced.push_back(*bounds.start);
    return lSliced;
  }


  void wrapValHeader()
  {
    boost::python::class_< uhal::ValHeader > ( "ValHeader", boost::python::init< const uhal::ValHeader& >() )
    .def ( "valid", static_cast< bool ( uhal::ValHeader::* ) () > ( &uhal::ValHeader::valid ) )
    ;
  }


  template <typename T>
  void wrapValWord(const std::string& aNameSuffix)
  {
    boost::python::class_< uhal::ValWord<T> > ( ("ValWord_" + aNameSuffix).c_str(), boost::python::init< const uhal::ValWord<T>& >() )
    .def ( boost::python::init<>() )
    .def ( "valid", static_cast< bool ( uhal::ValWord<T>::* ) () > ( &uhal::ValWord<T>::valid ) )
    .def ( "value", static_cast< T ( uhal::ValWord<T>::* ) () const > ( &uhal::ValWord<T>::value ) )
    .def ( "mask",  static_cast< const T& ( uhal::ValWord<T>::* ) () const > ( &uhal::ValWord<T>::mask ) , const_ref_return_policy() )
    .def ( "__str__", static_cast< std::string (*) ( const uhal::ValWord<T>& ) > ( &pycohal::getString<T> ) )
    .def ( "__int__", static_cast< T ( uhal::ValWord<T>::* ) () const> ( &uhal::ValWord<T>::value ) )
#if PY_VERSION_HEX >= 0x03000000
    .def ( "__index__", static_cast< T ( uhal::ValWord<T>::* ) () const> ( &uhal::ValWord<T>::value ) )
#else
    .def ( "__hex__", getHexString<T> )
#endif
    ;
  }


  template <typename T>
  void wrapValVector(const std::string& aNameSuffix)
  {
    boost::python::class_<uhal::ValVector<T> > ( ("ValVector_" + aNameSuffix).c_str(), boost::python::init< const uhal::ValVector<T>& >() )
    .def ( boost::python::init<>() )
    .def ( "valid", static_cast< bool ( uhal::ValVector<T>::* ) () > ( &uhal::ValVector<T>::valid ) )
    .def ( "value", static_cast< std::vector<T> ( uhal::ValVector<T>::* ) () const > ( &uhal::ValVector<T>::value ) )
    .def ( "size",  &uhal::ValVector<T>::size )
    .def ( "at", &uhal::ValVector<T>::at, const_ref_return_policy() )
    .def ( "__str__", static_cast< std::string (*) ( const uhal::ValVector<T>& ) > ( &pycohal::getString<T> ) )
    .def ( "__len__", &uhal::ValVector<T>::size )
    .def ( "__getitem__", &pycohal::ValVectorIndexingSuite<T>::getItem , const_ref_return_policy() )
    .def ( "__getitem__", &pycohal::ValVectorIndexingSuite<T>::getSlice )
    .def ( "__iter__", boost::python::range ( &uhal::ValVector<T>::begin , &uhal::ValVector<T>::end ) )
    ;
  }


  void wrapValMem()
  {
    wrapValHeader();
    wrapValWord<uint32_t>("uint32");
    wrapValWord<uint64_t>("uint64");
    wrapValVector<uint32_t>("uint32");
    wrapValVector<uint64_t>("uint64");
  }
}