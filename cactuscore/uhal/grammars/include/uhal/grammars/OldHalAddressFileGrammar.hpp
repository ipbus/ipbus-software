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

// #ifndef _uhal_OldHalAddressFileGrammar_hpp_
// #define _uhal_OldHalAddressFileGrammar_hpp_

// #include <boost/fusion/include/adapt_struct.hpp>
// #include <boost/fusion/adapted/std_pair.hpp>
// #include <boost/spirit/include/qi.hpp>

// #include <vector>
// #include <string>
// #include <iostream>



// namespace uhal
// {
// //! Struct to store an http response received from a server when parsed by boost spirit
// struct OldHalEntryType
// {
// //! the HAL key
// std::string key;
// //! the HAL AM
// uint8_t AM;
// //! the HAL width
// std::string width;
// //! the HAL address
// std::string address;
// //! the HAL mask
// std::string mask;
// //! the HAL read
// std::string read;
// //! the HAL write
// std::string write;
// //! the HAL description
// std::string description;
// };


// std::ostream& operator<< ( std::ostream& aStr , const OldHalEntryType& aOldHalEntry )
// {
// aStr << " > key = '" << aOldHalEntry.key << "'\n";
// aStr << " > AM = '" << aOldHalEntry.AM << "'\n";
// aStr << " > width = '" << aOldHalEntry.width << "'\n";
// aStr << " > address = '" << aOldHalEntry.address << "'\n";
// aStr << " > mask = '" << aOldHalEntry.mask << "'\n";
// //aStr << " > permissions = " << (aOldHalEntry.read?"r":"") << (aOldHalEntry.write?"w":"") << "\n";
// aStr << " > description = '" << aOldHalEntry.description << "'" << std::endl;
// return aStr;
// }

// }

// // Call to BOOST_FUSION_ADAPT_STRUCT must be at global scope
// //! A boost::fusion adaptive struct used by the boost::qi parser
// BOOST_FUSION_ADAPT_STRUCT (
// uhal::OldHalEntryType,
// ( std::string , key )
// ( uint8_t , AM )
// ( std::string , width )
// ( std::string , address )
// ( std::string , mask )
// ( std::string , read )
// ( std::string , write )
// ( std::string , description )
// );



// namespace grammars
// {

// struct OldHalSkipParser : boost::spirit::qi::grammar< std::vector<uint8_t>::iterator >
// {
// OldHalSkipParser();
// boost::spirit::qi::rule< std::vector<uint8_t>::iterator > skip;
// };


// struct OldHalEntryGrammar : boost::spirit::qi::grammar< std::vector<uint8_t>::iterator , std::vector< uhal::OldHalEntryType >() , OldHalSkipParser >
// {
// //! Default Constructor where we will define the boost::qi rules relating the members
// OldHalEntryGrammar();

// //! Boost spirit parsing rule for parsing the incoming http packet info
// boost::spirit::qi::rule< std::vector<uint8_t>::iterator,		std::vector< uhal::OldHalEntryType >(),					OldHalSkipParser > OldHalEntryVector;
// //! Boost spirit parsing rule for parsing the incoming http packet
// boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		uhal::OldHalEntryType(),								OldHalSkipParser > OldHalEntry;


// boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > key;
// boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		uint8_t(),												OldHalSkipParser > AM;
// boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > width;
// boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > address;
// boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > mask;
// boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > read;
// boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > write;
// boost::spirit::qi::rule< std::vector<uint8_t>::iterator, 		std::string(),											OldHalSkipParser > description;

// };


// }






// #endif
