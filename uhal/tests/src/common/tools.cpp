#include "uhal/tests/tools.hpp"

#include <boost/program_options.hpp>

namespace po = boost::program_options;

long uhal::tests::usdiff(const timeval& end, const timeval& start)  {
  long usec;
  usec=static_cast<long>((end.tv_sec-start.tv_sec)*1e6);
  usec+=(end.tv_usec-start.tv_usec);
  return usec;
}
    
std::map<std::string,std::string> uhal::tests::default_arg_parsing(int argc,char* argv[]) {
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
    ("connection_file,c", po::value<std::string>()->default_value("", "Connection file"))
    ("device_id,d", po::value<std::string>()->default_value("", "Device identifier"))
    ;
  
  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);    
  
  if (vm.count("help")) {
    std::cout << desc << std::endl;
    exit(0);
  }
  
  std::map<std::string,std::string> result;
  result["connection_file"] = vm["connection_file"].as<std::string>();
  result["device_id"] = vm["device_id"].as<std::string>();

  return result;

}

