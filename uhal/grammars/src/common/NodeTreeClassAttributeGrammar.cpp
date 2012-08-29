#include "uhal/grammars/NodeTreeClassAttributeGrammar.hpp"

namespace grammars
{
  NodeTreeClassAttributeGrammar::NodeTreeClassAttributeGrammar() :
    NodeTreeClassAttributeGrammar::base_type ( start )
  {
    using namespace boost::spirit;
    start = classname > - ( data_pairs_vector );
    classname = + ( qi::char_ - qi::lit ( ";" ) ) ;
    data_pairs_vector 	= qi::lit ( ";" ) > *data_pairs;
    data_pairs = data_pairs_1 > data_pairs_2;
    data_pairs_1 = + ( qi::char_ - qi::lit ( "=" ) ) > qi::lit ( "=" );
    data_pairs_2 = * ( qi::char_ - qi::lit ( ";" ) ) >> - ( qi::lit ( ";" ) );
  }

}

