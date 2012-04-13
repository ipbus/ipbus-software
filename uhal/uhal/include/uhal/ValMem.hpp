#ifndef _uhal_ValMem_hpp_
#define _uhal_ValMem_hpp_

#include "uhal/exception.hpp"
#include <boost/shared_ptr.hpp>

#include <vector>
#include <iostream>

#include "uhal/log.hpp"

namespace uhal
{
	class NonValidatedMemory: public uhal::exception {  };
	class ValMemImutabilityViolation: public uhal::exception { };

	// Forward declare IPbusPacketInfo so it can be our friend
	class IPbusPacketInfo;

	template< typename T > class ValWord;
	template< typename T > class ValVector;

	template< typename T >
	struct _ValWord_
	{
		public:
			T value;
			bool valid;
			uint32_t mask;
		private:
			friend class ValWord<T>;

		_ValWord_ ( const T& aValue , const bool& aValid , const uint32_t aMask ) try :
				value ( aValue ) ,
					  valid ( aValid ) ,
					  mask ( aMask )
				{}
			catch ( const std::exception& aExc )
			{
				pantheios::log_EXCEPTION ( aExc );
				throw uhal::exception ( aExc );
			}

		public:
			virtual ~_ValWord_() {}
	};


	template< typename T >
	struct _ValVector_
	{
		public:
			std::vector<T> value;
			bool valid;
		private:
			friend class ValVector<T>;

		_ValVector_ ( const std::vector<T>& aValue , const bool& aValid ) try :
				value ( aValue ) ,
					  valid ( aValid )
				{}
			catch ( const std::exception& aExc )
			{
				pantheios::log_EXCEPTION ( aExc );
				throw uhal::exception ( aExc );
			}

		public:
			virtual ~_ValVector_() {}
	};


	template< typename T >
	class ValWord
	{
			friend class IPbusPacketInfo;

		public:
			ValWord ( const T& aValue , const uint32_t& aMask = 0xFFFFFFFF );
			ValWord ( const ValWord<T>& aVal );
			ValWord();
			bool valid();
			void valid ( bool aValid );
			ValWord& operator = ( const T& aValue );
			// operator const T&();
			// const T& value() const;
			operator T();
			T value() const;

			void value ( const T& aValue );

			const uint32_t& mask() const;
			void mask ( const uint32_t& aMask );

		private:
			// boost::shared_ptr<bool> mValid;
			// boost::shared_ptr<T> mValue;
			boost::shared_ptr< _ValWord_<T> > mMembers;

	};

	template< typename T >
	class ValVector
	{
			friend class IPbusPacketInfo;

		public:
			typedef typename std::vector< T >::iterator iterator;
			typedef typename std::vector< T >::const_iterator const_iterator;
			typedef typename std::vector< T >::reverse_iterator reverse_iterator;
			typedef typename std::vector< T >::const_reverse_iterator const_reverse_iterator;

		public:
			ValVector ( const std::vector<T>& aValues );
			ValVector ( const ValVector& aValues );
			ValVector ( uint32_t aSize );
			ValVector();
			bool valid();
			void valid ( bool aValid );

			template <class InputIterator> void assign ( InputIterator aFirst , InputIterator aLast )
			{
				try
				{
					if ( !/* *mValid */ mMembers->valid )
					{
						/* mValues-> */ mMembers->value.assign ( aFirst , aLast );
					}
					else
					{
						pantheios::log_ERROR ( "Throwing at " , ThisLocation() );
						throw ValMemImutabilityViolation();
					}
				}
				catch ( const std::exception& aExc )
				{
					pantheios::log_EXCEPTION ( aExc );
					throw uhal::exception ( aExc );
				}
			}

			void push_back ( const T& aValue );
			const T& operator[] ( std::size_t aSize ) const;
			const T& at ( std::size_t aSize ) const;
			std::size_t size() const;
			void clear();
			const_iterator begin() const;
			const_iterator end() const;
			const_reverse_iterator rbegin() const;
			const_reverse_iterator rend() const;

		private:
			// boost::shared_ptr<bool> mValid;
			// boost::shared_ptr<std::vector<T> > mValues;
			boost::shared_ptr< _ValVector_<T> > mMembers;

	};




}

#endif

