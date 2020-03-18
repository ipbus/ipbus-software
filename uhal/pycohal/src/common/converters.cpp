
#include "uhal/pycohal/converters.hpp"

#include "uhal/ClientInterface.hpp"
#include "uhal/Node.hpp"
#include "uhal/ValMem.hpp"


namespace bpy = boost::python ;

void pycohal::register_converters()
{
  pycohal::Converter_std_vector_from_list<uint32_t>();
  pycohal::Converter_std_vector_from_list<std::string>();
  bpy::to_python_converter< std::vector<std::string>, pycohal::Converter_std_vector_to_list<std::string> >();
  bpy::to_python_converter< std::vector<uint32_t>, pycohal::Converter_std_vector_to_list<uint32_t> >();
  bpy::to_python_converter< boost::unordered_map<std::string, std::string>, pycohal::Converter_boost_unorderedmap_to_dict<std::string,std::string> >(); 

}



