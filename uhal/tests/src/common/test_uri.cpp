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

      Tom Williams, Rutherford Appleton Laboratory, Oxfordshire
      email: tom.williams <AT> cern.ch

---------------------------------------------------------------------------
*/

#include "uhal/grammars/URI.hpp"
#include "uhal/grammars/URIGrammar.hpp"

#include <boost/test/unit_test.hpp>


namespace uhal {
namespace tests {

URI parseURI(const std::string& aUri)
{
  URI lUri;

  grammars::URIGrammar lGrammar;
  std::string::const_iterator lBegin ( aUri.begin() );
  std::string::const_iterator lEnd ( aUri.end() );
  boost::spirit::qi::phrase_parse ( lBegin , lEnd , lGrammar , boost::spirit::ascii::space , lUri );

  return lUri;
}


BOOST_AUTO_TEST_SUITE( grammars )

BOOST_AUTO_TEST_SUITE( uri )

BOOST_AUTO_TEST_CASE (udp)
{
  URI lURI = parseURI("ipbusudp-2.0://someHost.xyz:2468");

  BOOST_CHECK_EQUAL(lURI.mProtocol, "ipbusudp-2.0");
  BOOST_CHECK_EQUAL(lURI.mHostname, "someHost.xyz");
  BOOST_CHECK_EQUAL(lURI.mPort, "2468");
  BOOST_CHECK_EQUAL(lURI.mPath, "");
  BOOST_CHECK_EQUAL(lURI.mExtension, "");
  BOOST_CHECK(lURI.mArguments.empty());
}

BOOST_AUTO_TEST_CASE (controlhub)
{
  URI lURI = parseURI("chtcp-2.0://someHost.xyz:2468?target=other-host.domain:3579");

  BOOST_CHECK_EQUAL(lURI.mProtocol, "chtcp-2.0");
  BOOST_CHECK_EQUAL(lURI.mHostname, "someHost.xyz");
  BOOST_CHECK_EQUAL(lURI.mPort, "2468");
  BOOST_CHECK_EQUAL(lURI.mPath, "");
  BOOST_CHECK_EQUAL(lURI.mExtension, "");
  BOOST_CHECK_EQUAL(lURI.mArguments.size(), size_t(1));
  BOOST_CHECK_EQUAL(lURI.mArguments.at(0).first, "target");
  BOOST_CHECK_EQUAL(lURI.mArguments.at(0).second, "other-host.domain:3579");
}

BOOST_AUTO_TEST_CASE (pcie)
{
  URI lURI = parseURI("ipbuspcie-2.0:///dev/aFile,/dev/anotherFile");

  BOOST_CHECK_EQUAL(lURI.mProtocol, "ipbuspcie-2.0");
  BOOST_CHECK_EQUAL(lURI.mHostname, "/dev/aFile,/dev/anotherFile");
  BOOST_CHECK_EQUAL(lURI.mPort, "");
  BOOST_CHECK_EQUAL(lURI.mPath, "");
  BOOST_CHECK_EQUAL(lURI.mExtension, "");
  BOOST_CHECK(lURI.mArguments.empty());

  lURI = parseURI("ipbuspcie-2.0:///dev/aFile,/dev/anotherFile?events=/path/to/someOtherFile");

  BOOST_CHECK_EQUAL(lURI.mProtocol, "ipbuspcie-2.0");
  BOOST_CHECK_EQUAL(lURI.mHostname, "/dev/aFile,/dev/anotherFile");
  BOOST_CHECK_EQUAL(lURI.mPort, "");
  BOOST_CHECK_EQUAL(lURI.mPath, "");
  BOOST_CHECK_EQUAL(lURI.mExtension, "");
  BOOST_CHECK_EQUAL(lURI.mArguments.size(), size_t(1));
  BOOST_CHECK_EQUAL(lURI.mArguments.at(0).first, "events");
  BOOST_CHECK_EQUAL(lURI.mArguments.at(0).second, "/path/to/someOtherFile");

  lURI = parseURI("ipbuspcie-2.0:///dev/aFile,/dev/anotherFile?events=/path/to/someOtherFile&sleep=20");

  BOOST_CHECK_EQUAL(lURI.mProtocol, "ipbuspcie-2.0");
  BOOST_CHECK_EQUAL(lURI.mHostname, "/dev/aFile,/dev/anotherFile");
  BOOST_CHECK_EQUAL(lURI.mPort, "");
  BOOST_CHECK_EQUAL(lURI.mPath, "");
  BOOST_CHECK_EQUAL(lURI.mExtension, "");
  BOOST_CHECK_EQUAL(lURI.mArguments.at(0).first, "events");
  BOOST_CHECK_EQUAL(lURI.mArguments.at(0).second, "/path/to/someOtherFile");
  BOOST_CHECK_EQUAL(lURI.mArguments.at(1).first, "sleep");
  BOOST_CHECK_EQUAL(lURI.mArguments.at(1).second, "20");
}


BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE_END()

} // end ns tests
} // end ns uhal
