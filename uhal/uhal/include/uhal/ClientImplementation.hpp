#ifndef _uhal_ClientImplementation_hpp_
#define _uhal_ClientImplementation_hpp_

#include "uhal/ClientInterface.hpp"

namespace uhal
{
	class IPBusUDPClient: public ClientInterface
	{
		public:
			IPBusUDPClient ( const std::string& aId,const std::string& aUri )
				:ClientInterface ( aId,aUri )
			{};
	};

	class IPBusTCPClient: public ClientInterface
	{
		public:
			IPBusTCPClient ( const std::string& aId,const std::string& aUri )
				:ClientInterface ( aId,aUri )
			{};
	};

	class ControlHubClient: public ClientInterface
	{
		public:
			ControlHubClient ( const std::string& aId,const std::string& aUri )
				:ClientInterface ( aId,aUri )
			{};
	};

	class DummyClient: public ClientInterface
	{
		public:
			DummyClient ( const std::string& aId,const std::string& aUri )
				:ClientInterface ( aId,aUri )
			{};
	};



}

#endif
