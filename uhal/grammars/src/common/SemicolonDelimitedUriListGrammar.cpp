#include "uhal/grammars/SemicolonDelimitedUriListGrammar.hpp"


namespace grammars
{
	SemicolonDelimitedUriListGrammar::SemicolonDelimitedUriListGrammar() :
		SemicolonDelimitedUriListGrammar::base_type ( data_pairs_vector )
	{
		using namespace boost::spirit;
		data_pairs_vector = *data_pairs;
		data_pairs = data_pairs_1 > data_pairs_2;
		data_pairs_1 = *qi::lit ( ";" ) >> + ( qi::char_ - "://" ) > "://";
		data_pairs_2 = * ( qi::char_ - qi::lit ( ";" ) ) >> *qi::lit ( ";" );
	}

}

