/*
---------------------------------------------------------------------------

    This file is part of uHAL.

    uHAL is a hardware access library and programming framework
    originally developed for upgrades of the Level-1 trigger of the CMS
    experiment at CERN.

    uHAL is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    uHAL is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with uHAL.  If not, see <http://www.gnu.org/licenses/>.


      Andrew Rose, Imperial College, London
      email: awr01 <AT> imperial.ac.uk

      Marc Magrans de Abril, CERN
      email: marc.magrans.de.abril <AT> cern.ch

---------------------------------------------------------------------------
*/

// #include "uhal/grammars/OldHalAddressFileGrammar.hpp"


// namespace grammars
// {

// OldHalSkipParser::OldHalSkipParser() :
// OldHalSkipParser::base_type ( skip )
// {
// using namespace boost::spirit;
// skip = ascii::blank | ( qi::lit ( "*" ) >> * ( qi::char_ - qi::eol ) >> - ( qi::eol ) );
// }


// OldHalEntryGrammar::OldHalEntryGrammar() :
// OldHalEntryGrammar::base_type ( OldHalEntryVector )
// {
// using namespace boost::spirit;
// OldHalEntryVector = *OldHalEntry;
// OldHalEntry = key >> AM >> width >> address >> mask >> read >> write >> description;
// //BChannelLongExt		39			2			000000c2    00000001    0    		1    		0= int, 1=ext regs
// key = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
// AM = qi::lexeme[ uint_ >> + ( ascii::blank ) ];
// width = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
// address = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
// mask = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
// read = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
// write = qi::lexeme[ + ( qi::char_-ascii::blank ) ];
// description = qi::lexeme[ * ( qi::char_-qi::eol ) >> qi::eol ];
// }

// }

