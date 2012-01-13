#ifndef _uhal_test_tools_hpp_
#define _uhal_test_tools_hpp_

#include <boost/test/unit_test.hpp>
#include <boost/program_options.hpp>

#include <string>

namespace uhal {

  namespace test {
    
    template <class T> 
    T getParam(const std::string& param, const T& def_val) {
      
      T value(def_val);
      
      
      try {
	boost::program_options::options_description argDescriptions("Allowed options");
	argDescriptions.add_options()
	  (param.c_str(), boost::program_options::value<T>(&value)->default_value(value) );
	
	boost::program_options::parsed_options parsed = 
	  boost::program_options::command_line_parser(boost::unit_test::framework::master_test_suite().argc, boost::unit_test::framework::master_test_suite().argv)
	  .options(argDescriptions)
	  .allow_unregistered()       //unknown options do not throw
	  .run();

	boost::program_options::variables_map vm;
	boost::program_options::store(parsed,vm);
	boost::program_options::notify(vm);    

      } catch(std::exception& e) { 
	//Default value should be used instead
      }
      
      return value;
    };
  }
}
#endif 
