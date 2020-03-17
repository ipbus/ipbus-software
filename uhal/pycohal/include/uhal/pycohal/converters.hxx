#ifndef _uhal_pycohal_converters_hxx_
#define _uhal_pycohal_converters_hxx_

namespace pycohal {

//------------------------------------------//
// ---  Converter_std_vector_from_list  --- //
//------------------------------------------//

//---
template<class T>
Converter_std_vector_from_list<T>::Converter_std_vector_from_list() {
    boost::python::converter::registry::push_back ( &convertible, &construct, boost::python::type_id< std::vector<T> >() );
} 


//---
template <class T>
void* 
Converter_std_vector_from_list<T>::convertible ( PyObject* obj_ptr ) {
  if ( !PyList_Check ( obj_ptr ) ) {
    return 0;
  } else {
    return obj_ptr;
  }
}


//---
template <class T>
void 
Converter_std_vector_from_list<T>::construct ( PyObject* obj_ptr, boost::python::converter::rvalue_from_python_stage1_data* data ) {
  namespace bpy = boost::python;
  // Grab pointer to memory in which to construct the new vector
  void* storage = ( ( bpy::converter::rvalue_from_python_storage< std::vector<T> >* ) data )->storage.bytes;
  // Grab list object from obj_ptr
  bpy::list py_list ( bpy::handle<> ( bpy::borrowed ( obj_ptr ) ) );
  // Construct vector in requested location, and set element values.
  // boost::python::extract will throw appropriate exception if can't convert to type T ; boost::python will then call delete itself as well.
  size_t nItems = bpy::len ( py_list );
  std::vector<T>* vec_ptr = new ( storage ) std::vector<T> ( nItems );

  for ( size_t i = 0; i < nItems; i++ ) {
    vec_ptr->at ( i ) = bpy::extract<T> ( py_list[i] );
  }

  // If successful, then register memory chunk pointer for later use by boost.python
  data->convertible = storage;
}

}

#endif /* _uhal_pycohal_converters_hxx_ */

