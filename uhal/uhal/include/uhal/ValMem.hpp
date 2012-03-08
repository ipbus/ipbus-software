#ifndef _uhal_ValMem_hpp_
#define _uhal_ValMem_hpp_

#include <boost/shared_ptr.hpp>

#include <vector>
#include <iostream>

namespace uhal
{
	class NonValidatedMemory: public std::exception {  };
	class ValMemImutabilityViolation: public std::exception { };


	template< typename T >
	class ValWord
	{
		public:
			ValWord ( const T& aValue );
			ValWord ( const ValWord<T>& aVal );
			ValWord();
			bool valid();
			void valid ( bool aValid );
			ValWord& operator = ( const T& aValue );
			operator const T&();
			const T& value() const;
			void value ( const T& aValue );

		private:
			boost::shared_ptr<bool> mValid;
			boost::shared_ptr<T> mValue;

	};

	template< typename T >
	class ValVector
	{
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
				if ( !*mValid )
				{
					mValues->assign ( aFirst , aLast );
				}
				else
				{
					throw ValMemImutabilityViolation();
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
			boost::shared_ptr<bool> mValid;
			boost::shared_ptr<std::vector<T> > mValues;

	};




}

#endif

