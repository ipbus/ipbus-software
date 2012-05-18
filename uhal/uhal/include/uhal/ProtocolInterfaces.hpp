/**
	@file
	@author Andrew W. Rose
	@date 2012
*/

#ifndef _uhal_ProtocolInterfaces_hpp_
#define _uhal_ProtocolInterfaces_hpp_

#include "uhal/IPbusPacketInfo.hpp"

namespace uhal
{

	//! An abstract base class defining the interface to a Transport Protocol
	class TransportProtocol
	{
		public:
			/**
				Default constructor
			*/
			TransportProtocol() {}

			/**
				Destructor
			*/
			virtual ~TransportProtocol() {}

			/**
				Pure virtual function in which the concrete implementations should flush the queue of pending IPbus transactions
			*/
			virtual void Dispatch() = 0;

	};


	//! An abstract base class defining the interface to a Packing Protocol
	class PackingProtocol
	{
		public:
			/**
				Default constructor
			*/
			PackingProtocol() {}

			/**
				Destructor
			*/
			virtual ~PackingProtocol() {}

			/**
				Pure virtual function in which the concrete implementations should add an IPbusPacketInfo to the queue of pending IPbus transactions
				@param aIPbusPacketInfo an IPbusPacketInfo to be added to the queue of pending IPbus transactions
				@param aId an identifier that may be required by clients that handle more than one target device
			*/
			virtual void pack ( IPbusPacketInfo& aIPbusPacketInfo , const uint64_t& aId = 0 ) = 0;

			/**
				A virtual function which will be called immediately prior to a call to the Transport Protocol's Dispatch() function. Can be used for finalizing the queue for dispatch.
			*/
			virtual void PreDispatch() {}

			/**
				A virtual function which will be called immediately after to a call to the Transport Protocol's Dispatch() function. Can be used for testing that the replys are as expected.
			*/
			virtual void PostDispatch() {}

	};



}

#endif
