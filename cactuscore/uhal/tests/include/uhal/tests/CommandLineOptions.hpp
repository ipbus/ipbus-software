namespace uhal {
  namespace tests {
  
    //! Struct to store the command line options
    struct CommandLineOptions
    {
      //! The delay in seconds between the request and response of the first IPbus transaction
      uint32_t delay;
      //! The port used by the dummy hardware
      uint16_t port;
      //! Whether we use the big-endian hack
      bool bigendian;
      //! IPbus version number - 1 or 2
      uint32_t version;
    };
  
    /**
      Function to parse the command line arguments into a struct containing the information
      @param argc the number of command line arguments
      @param argv array of c-strings containing the command line arguments
    */
    CommandLineOptions ParseCommandLineOptions ( int argc,char* argv[] )
    {
      // Declare the supported options.
      boost::program_options::options_description desc ( "Allowed options" );
      desc.add_options()
      ( "help,h", "Produce this help message" )
      ( "delay,d", boost::program_options::value<uint32_t>()->default_value ( 0 ) , "Reply delay for first packet (in seconds) - optional" )
      ( "port,p", boost::program_options::value<uint16_t>() , "Port number to listen on - required" )
      ( "big-endian,b", "Include the big-endian hack (version 2 only)" )
      ( "version,v", boost::program_options::value<uint32_t>() , "IPbus Major version (1 or 2) - required" )
      ( "verbose,V", "Produce verbose output" )
      ;
      boost::program_options::variables_map vm;
  
      try
      {
        boost::program_options::store ( boost::program_options::parse_command_line ( argc, argv, desc ), vm );
        boost::program_options::notify ( vm );
  
        if ( vm.count ( "help" ) )
        {
          std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
          std::cout << desc << std::endl;
          exit ( 0 );
        }
  
        CommandLineOptions lResult;
        lResult.delay = vm["delay"].as<uint32_t>();
        lResult.port = vm["port"].as<uint16_t>();
        lResult.version = vm["version"].as<uint32_t>();
        lResult.bigendian = bool ( vm.count ( "big-endian" ) );
  
        if ( ( lResult.version == 1 ) && ( lResult.bigendian ) )
        {
          log ( Error , "-big-endian flag does nothing with version set to 1" );
        }
  
        if ( vm.count ( "verbose" ) )
        {
          setLogLevelTo ( Debug() );
        }
        else
        {
          setLogLevelTo ( Notice() );
        }
  
        return lResult;
      }
      catch ( std::exception& e )
      {
        std::cerr << "ERROR: " << e.what() << std::endl << std::endl;
        std::cout << "Usage: " << argv[0] << " [OPTIONS]" << std::endl;
        std::cout << desc << std::endl;
        exit ( 1 );
      }
    }
  }
}