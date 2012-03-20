#ifndef _uhal_ClientFactory_hpp_
#define _uhal_ClientFactory_hpp_

#include "uhal/ClientInterface.hpp"

#include "boost/utility.hpp"

#include <map>

namespace uhal
{
	class ProtocolAlreadyExist: public std::exception {  };
	class ProtocolDoesNotExist: public std::exception {  };

	class ClientFactory: private boost::noncopyable
	{
		public:
			static ClientFactory& getInstance();

			template <class T>
			void add ( const std::string& aProtocol )
			{
				std::map<std::string,CreatorInterface*>::const_iterator i ( mCreators.find ( aProtocol ) );

				if ( i != mCreators.end() )
				{
					throw ProtocolAlreadyExist();
				}

				mCreators[aProtocol] = new Creator<T>();
			}

			ClientInterface getClient ( const std::string& aId , const std::string& aUri )
			{
				std::string lProtocol = getProtocol ( aUri );
				std::map<std::string,CreatorInterface*>::const_iterator i ( mCreators.find ( lProtocol ) );

				if ( i == mCreators.end() )
				{
					throw ProtocolDoesNotExist();
				}

				return i->second->create ( aId , aUri );
			}

		private:
			ClientFactory() {}
			virtual ~ClientFactory() {}
			
			std::string getProtocol ( const std::string& aUri )
			{
				return "ipbusudp";
			}

		private:
			class CreatorInterface
			{
				public:
					virtual ~CreatorInterface()
					{
						;
					}

					virtual ClientInterface create ( const std::string& aId,const std::string& aUri ) = 0;
			};

			template <class T>
			class Creator: public CreatorInterface
			{
				public:

					Creator() {};
					ClientInterface create ( const std::string& aId,const std::string& aUri )
					{
						return T ( aId , aUri );
					}
			};

		private:
			static ClientFactory* mInstance;
			std::map<std::string,CreatorInterface*> mCreators;
	};
}

#endif
