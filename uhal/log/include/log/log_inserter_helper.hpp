
#ifndef _log_inserters_helper_hpp_
#define _log_inserters_helper_hpp_

namespace uhal{



	template< typename T >
	void log_inserter( const T& );



	template< typename T >
	struct RefWrapper
	{	
		protected:
			RefWrapper( const T& aT ) : mT( aT ){}
			virtual ~RefWrapper() {}

		public:
			const T& value() const { return mT; }

		private:
			const T& mT;
	};

}

#endif
