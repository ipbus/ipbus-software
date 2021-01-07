
#include "uhal/pycohal/converters.hpp"

#include "uhal/ClientInterface.hpp"
#include "uhal/Node.hpp"
#include "uhal/ValMem.hpp"


namespace bpy = boost::python ;

void pycohal::register_converters()
{
  const bpy::converter::registration* reg;

  reg = bpy::converter::registry::query(bpy::type_id< std::vector<uint32_t> >());
  if (reg == NULL or (*reg).m_to_python == NULL)
  {
    pycohal::Converter_std_vector_from_list<uint32_t>();
    bpy::to_python_converter< std::vector<uint32_t>, pycohal::Converter_std_vector_to_list<uint32_t> >();
  }

  reg = bpy::converter::registry::query(bpy::type_id< std::vector<std::string> >());
  if (reg == NULL or (*reg).m_to_python == NULL)
  {
    pycohal::Converter_std_vector_from_list<std::string>();
    bpy::to_python_converter< std::vector<std::string>, pycohal::Converter_std_vector_to_list<std::string> >();
  }

  reg = bpy::converter::registry::query(bpy::type_id< std::unordered_map<std::string, std::string> >());
  if (reg == NULL or (*reg).m_to_python == NULL)
    bpy::to_python_converter< std::unordered_map<std::string, std::string>, pycohal::Converter_std_unorderedmap_to_dict<std::string,std::string> >();
}



