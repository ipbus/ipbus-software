#include "BoostSpiritGrammars/URLGrammar.hpp"


namespace BoostSpiritGrammars
{
	
	URLGrammar::URLGrammar() :
		URLGrammar::base_type( data_pair )
	{
		using namespace boost::spirit;
		data_pair = data_pair_1 > data_pair_2;
		data_pair_1 = +(qi::char_ - "/") > "/";
		data_pair_2 = +(qi::char_);
	}	
	
	
}

