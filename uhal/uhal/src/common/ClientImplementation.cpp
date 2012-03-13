#include "uhal/ClientImplementation.hpp"

#include "uhal/ClientFactory.hpp"

namespace uhal
{

    ClientFactory* ClientFactory::mInstance = NULL;

    ClientFactory& ClientFactory::getInstance()
    {
        if ( mInstance == NULL ) {
            mInstance = new ClientFactory();

            mInstance->add<uhal::IPBusUDPClient>( "ipbusudp" );

            mInstance->add<uhal::IPBusTCPClient>( "ipbustcp" );

            mInstance->add<uhal::ControlHubClient>( "controlhuptcp" );

            mInstance->add<uhal::DummyClient>( "dummy" );


        }

        return *mInstance;

    }
}
