
#ifndef _log_backend_colour_terminal_hpp_
#define _log_backend_colour_terminal_hpp_

#include <stdio.h>
#include <stdint.h>

namespace uhal
{

	template< typename T >
	class log_formatter
	{
		public:
			static void head();
			static void tail();
		private:
		
			//Trying to specialize log_formatter directly, I was getting errors of explicit specialization after instantiation
			template< typename U >
			struct template_specialization_helper{
				static void print();
			};
		
	};
	
	
	void put ( const char& aChar );
	void put ( const char* aStr );
	void put ( const char* aStart , const uint32_t& aSize );
	
}
	
#include <uhal/log/log_backend.colour_terminal.hxx>

#endif
