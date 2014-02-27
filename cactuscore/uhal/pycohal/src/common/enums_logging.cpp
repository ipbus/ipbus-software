
#include "uhal/pycohal/enums_logging.hpp"

// The following python boost patch is required to compile on apple
#include "uhal/pycohal/boost_python.hpp"

#include "uhal/definitions.hpp"
#include "uhal/log/log.hpp"
#include "uhal/pycohal/converters_exceptions.hpp"


namespace bpy = boost::python ;


void pycohal::setLogLevelTo ( const pycohal::LogLevel& logLevel )
{
  switch ( logLevel )
  {
    case pycohal::DEBUG :
      uhal::setLogLevelTo ( uhal::Debug() );
      break;
    case pycohal::INFO :
      uhal::setLogLevelTo ( uhal::Info() );
      break;
    case pycohal::NOTICE :
      uhal::setLogLevelTo ( uhal::Notice() );
      break;
    case pycohal::WARNING :
      uhal::setLogLevelTo ( uhal::Warning() );
      break;
    case pycohal::ERROR :
      uhal::setLogLevelTo ( uhal::Error() );
      break;
    case pycohal::FATAL :
      uhal::setLogLevelTo ( uhal::Fatal() );
      break;
    default :
      throw pycohal::PycohalLogLevelEnumError();
  }
}


const bool& pycohal::LoggingIncludes ( const pycohal::LogLevel& logLevel )
{
  switch ( logLevel )
  {
    case pycohal::DEBUG :
      return  uhal::LoggingIncludes ( uhal::Debug() );
    case pycohal::INFO :
      return uhal::LoggingIncludes ( uhal::Info() );
    case pycohal::NOTICE :
      return uhal::LoggingIncludes ( uhal::Notice() );
    case pycohal::WARNING :
      return uhal::LoggingIncludes ( uhal::Warning() );
    case pycohal::ERROR :
      return uhal::LoggingIncludes ( uhal::Error() );
    case pycohal::FATAL :
      return uhal::LoggingIncludes ( uhal::Fatal() );
    default :
      throw PycohalLogLevelEnumError();
  }
}


void pycohal::wrap_enums()
{
  bpy::enum_<uhal::defs::NodePermission> ( "NodePermission" )
  .value ( "READ", uhal::defs::READ )
  .value ( "WRITE", uhal::defs::WRITE )
  .value ( "READWRITE", uhal::defs::READWRITE )
  ;
  bpy::enum_<uhal::defs::BlockReadWriteMode> ( "BlockReadWriteMode" )
  .value ( "SINGLE", uhal::defs::SINGLE )
  .value ( "INCREMENTAL", uhal::defs::INCREMENTAL )
  .value ( "NON_INCREMENTAL", uhal::defs::NON_INCREMENTAL )
  .value ( "HIERARCHICAL", uhal::defs::HIERARCHICAL )
  ;
  bpy::enum_<pycohal::LogLevel> ( "LogLevel" )
  .value ( "FATAL",   pycohal::FATAL )
  .value ( "ERROR",   pycohal::ERROR )
  .value ( "WARNING", pycohal::WARNING )
  .value ( "NOTICE",  pycohal::NOTICE )
  .value ( "INFO",    pycohal::INFO )
  .value ( "DEBUG",   pycohal::DEBUG )
  ;
}

void pycohal::wrap_logging_functions()
{
  bpy::def ( "setLogLevelFromEnvironment", uhal::setLogLevelFromEnvironment );
  bpy::def ( "disableLogging", uhal::disableLogging );
  bpy::def ( "setLogLevelTo", pycohal::setLogLevelTo );
  bpy::def ( "LoggingIncludes", pycohal::LoggingIncludes, bpy::return_value_policy<bpy::copy_const_reference>() );
}


