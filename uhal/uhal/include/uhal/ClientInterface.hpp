#ifndef _uhal_ClientInterface_hpp_
#define _uhal_ClientInterface_hpp_

#include "uhal/definitions.hpp"
#include "uhal/ValMem.hpp"

#include <vector>
#include <iostream>

namespace uhal
{
	class AtomicTransactionSize: public std::exception {};

	class ClientInterface
	{
		public:
			ClientInterface ( const std::string& aId, const std::string& aUri )
				:mId ( aId )
			{}
			virtual ~ClientInterface() {}

			std::string id()
			{
				return mId;
			}
			virtual bool ping()
			{
				return true;
			}
			virtual std::string url()
			{
				return "not implemented";
			}

			virtual void write ( const uint32_t aAddr, const uint32_t& aValue )
			{
				ValWord< uint32_t > lValWord ( aValue );
				mValwords.push_back ( lValWord );
			}

			virtual void write ( const uint32_t& aAddr, const uint32_t& aValue, const uint32_t& aMask )
			{
				ValWord< uint32_t > lValWord ( ( aValue << trailing_right_bits ( aMask ) ) && aMask );
				mValwords.push_back ( lValWord );
			}

			virtual void writeBlock ( const uint32_t& aAddr, const std::vector< uint32_t >& values, const defs::BlockReadWriteMode aMode=defs::INCREMENTAL )
			{
				ValVector< uint32_t > lValVector;
				lValVector.assign ( values.begin(),values.end() );
				mValblocks.push_back ( lValVector );
			}

			virtual ValWord< uint32_t > read ( const uint32_t& aAddr )
			{
				ValWord< uint32_t > lValWord ( rand() );
				mValwords.push_back ( lValWord );
				return lValWord;
			}

			virtual ValWord< uint32_t > read ( const uint32_t& aAddr, const uint32_t& aMask )
			{
				uint32_t lValue = rand();
				lValue = ( lValue & aMask ) >> trailing_right_bits ( aMask );
				ValWord< uint32_t > lValWord ( lValue );
				mValwords.push_back ( lValWord );
				return lValWord;
			}

			virtual ValVector< uint32_t > readBlock ( const uint32_t& aAddr, const uint32_t& size, const defs::BlockReadWriteMode aMode=defs::INCREMENTAL )
			{
				std::vector<uint32_t> lValues;

				for ( uint32_t i ( 0 ); i!= size; ++i )
				{
					lValues.push_back ( rand() );
				}

				ValVector< uint32_t > lValVector ( lValues );
				mValblocks.push_back ( lValVector );
				return lValVector;
			}

			//validation has to be moved to the descendants
			virtual void dispatch ( defs::DispatchMode aMode = defs::NON_ATOMIC )
			{
				if ( aMode == defs::ATOMIC &&
						( mValblocks.size() +mValwords.size() ) > MAX_REQUEST_PER_PACKET )
				{
					throw AtomicTransactionSize();
				}

				try
				{
					for ( std::vector< ValWord< uint32_t > >::iterator lIt = mValwords.begin() ; lIt != mValwords.end() ; ++lIt )
					{
						lIt->valid ( true );
					}

					mValwords.clear();

					for ( std::vector< ValVector< uint32_t > >::iterator lIt = mValblocks.begin() ; lIt != mValblocks.end() ; ++lIt )
					{
						lIt->valid ( true );
					}

					mValblocks.clear();
				}
				catch ( std::exception& e )
				{
					mValwords.clear();
					mValblocks.clear();
					throw;
				}
			}
		private:
			unsigned int trailing_right_bits ( uint32_t aValue )
			{
				unsigned int lReturn = sizeof ( aValue ) * 8; // lReturn will be the number of zero bits on the right
				aValue &= -signed ( aValue );

				if ( aValue )
				{
					lReturn--;
				}

				if ( aValue & 0x0000FFFF )
				{
					lReturn -= 16;
				}

				if ( aValue & 0x00FF00FF )
				{
					lReturn -= 8;
				}

				if ( aValue & 0x0F0F0F0F )
				{
					lReturn -= 4;
				}

				if ( aValue & 0x33333333 )
				{
					lReturn -= 2;
				}

				if ( aValue & 0x55555555 )
				{
					lReturn -= 1;
				}

				return lReturn;
			}
		private:
			static const size_t MAX_REQUEST_PER_PACKET = 1500/8/2;
			std::vector< ValWord< uint32_t > > mValwords;
			std::vector< ValVector< uint32_t > > mValblocks;
			std::string mId;
	};



}

#endif

