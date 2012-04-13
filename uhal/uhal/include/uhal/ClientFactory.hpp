#ifndef _uhal_ClientFactory_hpp_
#define _uhal_ClientFactory_hpp_

#include "uhal/exception.hpp"
#include "uhal/ClientInterface.hpp"
#include "uhal/Utilities.hpp"
#include "uhal/log.hpp"

#include "boost/utility.hpp"

#include <map>

namespace uhal
{
	class ProtocolAlreadyExist: public uhal::exception { };
	class ProtocolDoesNotExist: public uhal::exception { };

	class ClientFactory: private boost::noncopyable
	{

		public:
			static ClientFactory& getInstance();

			template <class T>
			void add ( const std::string& aProtocol );

			boost::shared_ptr<ClientInterface> getClient ( const std::string& aId , const std::string& aUri );


		private:
			ClientFactory();
			virtual ~ClientFactory();


		private:
			class CreatorInterface
			{
				public:
					CreatorInterface() {}
					virtual ~CreatorInterface() {}
					virtual boost::shared_ptr<ClientInterface> create ( const std::string& aId , const URI& aUri ) = 0;
			};

			template <class T>
			class Creator: public CreatorInterface
			{
				public:

					Creator() {}
					virtual ~Creator() {}
					boost::shared_ptr<ClientInterface> create ( const std::string& aId , const URI& aUri )
					{
						try
						{
							return boost::shared_ptr<ClientInterface> ( new T ( aId , aUri ) );
						}
						catch ( const std::exception& aExc )
						{
							pantheios::log_EXCEPTION ( aExc );
							throw uhal::exception ( aExc );
						}
					}
			};


		private:
			static ClientFactory* mInstance;
			std::hash_map< std::string , CreatorInterface* > mCreators; //map string name of each protocol to a creator for that protocol

	};



	template <class T>
	void ClientFactory::add ( const std::string& aProtocol )
	{
		try
		{
			std::hash_map<std::string , CreatorInterface*>::const_iterator lIt = mCreators.find ( aProtocol ) ;

			if ( lIt != mCreators.end() )
			{
				//pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
				//throw ProtocolAlreadyExist();
				pantheios::log_WARNING ( "Protocol \"" , aProtocol , "\" already exists in map of creators. Continuing for now, but be warned." );
				return;
			}

			mCreators[aProtocol] = new Creator<T>();
		}
		catch ( const std::exception& aExc )
		{
			pantheios::log_EXCEPTION ( aExc );
			throw uhal::exception ( aExc );
		}
	}


}

#endif
