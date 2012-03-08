#include "uhal/ClientImplementation.hpp"

#include "uhal/ClientFactory.hpp"

namespace uhal
{

    ClientFactory* ClientFactory::mInstance = 0;

    ClientFactory& ClientFactory::getInstance()
    {
        if ( mInstance == 0 ) {
            mInstance = new ClientFactory();

            mInstance->add<uhal::IPBusUDPClient>( "ipbusudp" );

            mInstance->add<uhal::IPBusTCPClient>( "ipbustcp" );

            mInstance->add<uhal::ControlHubClient>( "controlhuptcp" );

            mInstance->add<uhal::DummyClient>( "dummy" );


        }

        return *mInstance;

    }
}
