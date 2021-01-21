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

#include <iomanip>
#include <typeinfo>

#include "uhal/NodeTreeBuilder.hpp"
#include "uhal/utilities/xml.hpp"
#include "uhal/uhal.hpp"

#include "uhal/detail/utilities.hpp"
#include "uhal/tests/DummyDerivedNode.hpp"
#include "uhal/tests/fixtures.hpp"

#include <boost/filesystem.hpp>
#include <boost/test/unit_test.hpp>



namespace uhal {


std::ostream& operator<< (std::ostream& aStream, const std::pair<const Node*, const Node*>& aNodes)
{
  aStream << "{";
  if (aNodes.first != NULL)
    aStream << "'" << aNodes.first->getPath() << "'";
  else
    aStream << "NULL";

  aStream << ", ";
  if (aNodes.second != NULL)
    aStream << "'" << aNodes.second->getPath() << "'";
  else
    aStream << "NULL";

  aStream << "}";
  return aStream;
}


namespace tests {

struct NodeProperties {
  NodeProperties(const std::string& aIdPath, const uint32_t aAddress, const defs::BlockReadWriteMode aMode, const defs::NodePermission aPermission, const size_t aSize, const std::string& aModule, const size_t aNrDescendants);
  ~NodeProperties() {}

  std::string path;
  std::string id;
  uint32_t address;
  defs::BlockReadWriteMode mode;
  defs::NodePermission permission;
  size_t size;
  uint32_t mask;
  std::string description;
  std::string module;
  std::vector<std::string> descendantIds;
  std::map<std::string, const std::type_info*> descendantTypes;
  std::string tags;
  std::unordered_map<std::string,std::string> parameters;
  std::unordered_map<std::string,std::string> fwInfo;
  const std::type_info* type;
};


struct SimpleAddressTableFixture {
public:
  SimpleAddressTableFixture();
  ~SimpleAddressTableFixture() {}

  const std::string addrTableStr;
  pugi::xml_document addrTableDoc;

  std::vector<NodeProperties> nodeProperties;
};


struct AddressTableOverlapFixture {
public:
  AddressTableOverlapFixture();
  ~AddressTableOverlapFixture() {}

  const std::string addrTableStr;
  pugi::xml_document addrTableDoc;

  std::vector<NodeProperties> nodeProperties;
};


//! Fixture for tests based on 'dummy' address files
struct DummyAddressFileFixture : public AbstractFixture {
public:
  DummyAddressFileFixture();
  ~DummyAddressFileFixture() {}

  std::string getAddrFileAbsPath(const std::string& aSuffix)
  {
    std::string lResult(addrFileAbsPath);
    lResult.replace(addrFileAbsPath.size() - 11, 7, aSuffix);
    return lResult;
  }

  const std::string addrFileURI;
  const std::string addrFileAbsPath;
  std::string addrFileLevel2AbsPath;
  std::string addrFileLevel3AbsPath;
  std::vector<NodeProperties> nodeProperties;
};



NodeProperties::NodeProperties(const std::string& aIdPath, const uint32_t aAddress, const defs::BlockReadWriteMode aMode, const defs::NodePermission aPermission, const size_t aSize, const std::string& aModule, const size_t aNrDescendants) :
  path(aIdPath),
  id((aIdPath.rfind('.') == std::string::npos) ? aIdPath : aIdPath.substr(aIdPath.rfind('.') + 1)),
  address(aAddress),
  mode(aMode),
  permission(aPermission),
  size(aSize),
  mask(defs::NOMASK),
  module(aModule),
  descendantIds(aNrDescendants),
  type(&typeid(Node))
{
}


SimpleAddressTableFixture::SimpleAddressTableFixture() :
  addrTableStr("<node>"
    "<node id='regA' address='0x0' />"
    "<node id='regB' address='0x1' />"
    "<node id='ram1' address='0x100' mode='block' size='256' />"
    "<node id='ram2' address='0x210' mode='block' size='16' />"
    "<node id='aPort' address='0x300' mode='port' size='1024' />"
    "</node>")
{
  BOOST_REQUIRE(addrTableDoc.load_string(addrTableStr.c_str()));

  nodeProperties.push_back(NodeProperties("regA", 0, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.push_back(NodeProperties("regB", 1, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.push_back(NodeProperties("ram1", 256, defs::INCREMENTAL, defs::READWRITE, 256, "", 0));
  nodeProperties.push_back(NodeProperties("ram2", 528, defs::INCREMENTAL, defs::READWRITE, 16, "", 0));
  nodeProperties.push_back(NodeProperties("aPort", 768, defs::NON_INCREMENTAL, defs::READWRITE, 1024, "", 0));
}


AddressTableOverlapFixture::AddressTableOverlapFixture() :
  addrTableStr("<node>"
    "<node id='reg1' address='0x0'/>"
    "<node id='reg2' address='0x1'/>"

    "<node id='reg3' address='0x2'>"
    "  <node id='A' mask='0x0000ffff'/>"
    "  <node id='B' mask='0xffff0000'/>"
    "</node>"
    "<node id='reg4' address='0x3'>"
    "  <node id='A' mask='0x000000ff'/>"
    "  <node id='B' mask='0x0000ff00'/>"
    "  <node id='C' mask='0x00ff0000'/>"
    "  <node id='D' mask='0xff000000'/>"
    "</node>"

    "<node id='port1' address='0x4' mode='port' size='256'/>"
    "<node id='port2' address='0x5' mode='port' size='1024'/>"
    "<node id='ram1' address='0x08' mode='block' size='8'/>"
    "<node id='ram2' address='0x10' mode='block' size='16'/>"

    "<node id='module1' address='0x40'>"
    "  <node id='reg1' address='0x0'/>"
    "  <node id='reg2' address='0x1'>"
    "    <node id='mask1' mask='0x00000fff'/>"
    "    <node id='mask2' mask='0x00fff000'/>"
    "    <node id='mask3' mask='0xff000000'/>"
    "  </node>"
    "  <node id='port' address='0x2' mode='port' size='256'/>"
    "  <node id='ram' address='0x10' mode='block' size='16'/>"
    "</node>"

    "<node id='module2' address='0x60'>"
    "  <node id='regA' address='0x0'/>"
    "  <node id='regB' address='0x1'>"
    "    <node id='A' mask='0x00000fff'/>"
    "    <node id='B' mask='0x00fff000'/>"
    "    <node id='C' mask='0xff000000'/>"
    "  </node>"
    "  <node id='fifo' address='0x2' mode='port' size='256'/>"
    "  <node id='block' address='0x10' mode='block' size='16'/>"
    "</node>"
    "</node>")
{
  BOOST_REQUIRE(addrTableDoc.load_string(addrTableStr.c_str()));

  nodeProperties.push_back(NodeProperties("reg1", 0, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.push_back(NodeProperties("reg2", 1, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.push_back(NodeProperties("reg3", 2, defs::SINGLE, defs::READWRITE, 1, "", 2));
  nodeProperties.push_back(NodeProperties("reg3.A", 2, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xffff;
  nodeProperties.push_back(NodeProperties("reg3.B", 2, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xffff0000;
  nodeProperties.push_back(NodeProperties("reg4", 3, defs::SINGLE, defs::READWRITE, 1, "", 4));
  nodeProperties.push_back(NodeProperties("reg4.A", 3, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xff;
  nodeProperties.push_back(NodeProperties("reg4.B", 3, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xff00;
  nodeProperties.push_back(NodeProperties("reg4.C", 3, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xff0000;
  nodeProperties.push_back(NodeProperties("reg4.D", 3, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xff000000;

  nodeProperties.push_back(NodeProperties("port1", 4, defs::NON_INCREMENTAL, defs::READWRITE, 256, "", 0));
  nodeProperties.push_back(NodeProperties("port2", 5, defs::NON_INCREMENTAL, defs::READWRITE, 1024, "", 0));
  nodeProperties.push_back(NodeProperties("ram1", 8, defs::INCREMENTAL, defs::READWRITE, 8, "", 0));
  nodeProperties.push_back(NodeProperties("ram2", 16, defs::INCREMENTAL, defs::READWRITE, 16, "", 0));

  nodeProperties.push_back(NodeProperties("module1", 64, defs::HIERARCHICAL, defs::READWRITE, 1, "", 7));
  nodeProperties.push_back(NodeProperties("module1.reg1", 64, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.push_back(NodeProperties("module1.reg2", 65, defs::SINGLE, defs::READWRITE, 1, "", 3));
  nodeProperties.push_back(NodeProperties("module1.reg2.mask1", 65, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xfff;
  nodeProperties.push_back(NodeProperties("module1.reg2.mask2", 65, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xfff000;
  nodeProperties.push_back(NodeProperties("module1.reg2.mask3", 65, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xff000000;
  nodeProperties.push_back(NodeProperties("module1.port", 66, defs::NON_INCREMENTAL, defs::READWRITE, 256, "", 0));
  nodeProperties.push_back(NodeProperties("module1.ram", 80, defs::INCREMENTAL, defs::READWRITE, 16, "", 0));

  nodeProperties.push_back(NodeProperties("module2", 96, defs::HIERARCHICAL, defs::READWRITE, 1, "", 7));
  nodeProperties.push_back(NodeProperties("module2.regA", 96, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.push_back(NodeProperties("module2.regB", 97, defs::SINGLE, defs::READWRITE, 1, "", 3));
  nodeProperties.push_back(NodeProperties("module2.regB.A", 97, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xfff;
  nodeProperties.push_back(NodeProperties("module2.regB.B", 97, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xfff000;
  nodeProperties.push_back(NodeProperties("module2.regB.C", 97, defs::SINGLE, defs::READWRITE, 1, "", 0));
  nodeProperties.back().mask = 0xff000000;
  nodeProperties.push_back(NodeProperties("module2.fifo", 98, defs::NON_INCREMENTAL, defs::READWRITE, 256, "", 0));
  nodeProperties.push_back(NodeProperties("module2.block", 112, defs::INCREMENTAL, defs::READWRITE, 16, "", 0));

  for (size_t i=0; i < nodeProperties.size(); i++) {
    BOOST_REQUIRE_LE(nodeProperties.at(i).descendantIds.size(), (nodeProperties.size() - i - 1));
    for (size_t j=0; j < nodeProperties.at(i).descendantIds.size(); j++) {
      std::string lRelativePath = nodeProperties.at(i+j+1).path.substr(nodeProperties.at(i).path.empty() ? 0 : nodeProperties.at(i).path.size() + 1);
      nodeProperties.at(i).descendantIds.at(j) = lRelativePath;
      nodeProperties.at(i).descendantTypes[lRelativePath] = nodeProperties.at(i+j+1).type;
    }
  }
}


DummyAddressFileFixture::DummyAddressFileFixture() :
  AbstractFixture(),
  addrFileURI(getAddressFileURI()),
  addrFileAbsPath(boost::filesystem::absolute(boost::filesystem::path(addrFileURI.substr(7))).native()),
  addrFileLevel2AbsPath(addrFileAbsPath),
  addrFileLevel3AbsPath(addrFileAbsPath)
{
  addrFileLevel2AbsPath.replace(addrFileAbsPath.size() - 11, 0, "level2_");
  addrFileLevel3AbsPath.replace(addrFileAbsPath.size() - 11, 0, "level3_");

  nodeProperties.push_back(NodeProperties("", 0, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileAbsPath, 55));

  nodeProperties.push_back(NodeProperties("REG", 1, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("REG_READ_ONLY", 2, defs::SINGLE, defs::READ, 1, addrFileAbsPath, 0));
  nodeProperties.push_back(NodeProperties("REG_WRITE_ONLY", 3, defs::SINGLE, defs::WRITE, 1, addrFileAbsPath, 0));
  nodeProperties.push_back(NodeProperties("REG_UPPER_MASK", 4, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().mask = 0xffff0000;
  nodeProperties.push_back(NodeProperties("REG_LOWER_MASK", 4, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().mask = 0xffff;
  nodeProperties.push_back(NodeProperties("REG_MASKED_READ_ONLY", 5, defs::SINGLE, defs::READ, 1, addrFileAbsPath, 0));
  nodeProperties.back().mask = 0xffff0000;
  nodeProperties.push_back(NodeProperties("REG_MASKED_WRITE_ONLY", 5, defs::SINGLE, defs::WRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().mask = 0xffff;
  nodeProperties.push_back(NodeProperties("REG_PARS", 6, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().parameters["arg0"] = "val100";
  nodeProperties.back().parameters["arg1"] = "val101";
  nodeProperties.push_back(NodeProperties("FIFO", 0x100, defs::NON_INCREMENTAL, defs::READWRITE, 268435456, addrFileAbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("REG_OUT_OF_ORDER", 6, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.push_back(NodeProperties("MEM", 0x100000, defs::INCREMENTAL, defs::READWRITE, 262144, addrFileAbsPath, 0));
  nodeProperties.back().description = "A block memory in an example XML file";

  nodeProperties.push_back(NodeProperties("SUBSYSTEM1", 0x210001, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileAbsPath, 5));
  nodeProperties.back().parameters["arg0"] = "val200";
  nodeProperties.back().parameters["arg1"] = "val201";
  nodeProperties.back().parameters["arg2"] = "val202";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM1.REG", 0x210002, defs::SINGLE, defs::READWRITE, 1, addrFileLevel2AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM1.MEM", 0x210003, defs::INCREMENTAL, defs::READWRITE, 262144, addrFileLevel2AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM1.SUBMODULE", 0x270001, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileLevel2AbsPath, 2));
  nodeProperties.back().parameters["arg0"] = "val300";
  nodeProperties.back().parameters["arg1"] = "val301";
  nodeProperties.back().parameters["arg2"] = "val10302";
  nodeProperties.back().parameters["arg3"] = "val10303";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM1.SUBMODULE.REG", 0x270002, defs::SINGLE, defs::READWRITE, 1, addrFileLevel3AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM1.SUBMODULE.MEM", 0x270003, defs::INCREMENTAL, defs::READWRITE, 256, addrFileLevel3AbsPath, 0));
  nodeProperties.back().tags = "test";

  nodeProperties.push_back(NodeProperties("SUBSYSTEM2", 0x310001, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileAbsPath, 5));
  nodeProperties.back().parameters["arg0"] = "val10000";
  nodeProperties.back().parameters["arg1"] = "val201";
  nodeProperties.back().parameters["arg2"] = "val202";
  nodeProperties.back().parameters["arg5"] = "val10005";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM2.REG", 0x310002, defs::SINGLE, defs::READWRITE, 1, addrFileLevel2AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM2.MEM", 0x310003, defs::INCREMENTAL, defs::READWRITE, 262144, addrFileLevel2AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM2.SUBMODULE", 0x370001, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileLevel2AbsPath, 2));
  nodeProperties.back().parameters["arg0"] = "val300";
  nodeProperties.back().parameters["arg1"] = "val301";
  nodeProperties.back().parameters["arg2"] = "val10302";
  nodeProperties.back().parameters["arg3"] = "val10303";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM2.SUBMODULE.REG", 0x370002, defs::SINGLE, defs::READWRITE, 1, addrFileLevel3AbsPath, 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM2.SUBMODULE.MEM", 0x370003, defs::INCREMENTAL, defs::READWRITE, 256, addrFileLevel3AbsPath, 0));
  nodeProperties.back().tags = "test";

  nodeProperties.push_back(NodeProperties("SMALL_MEM", 0x400000, defs::INCREMENTAL, defs::READWRITE, 256, addrFileAbsPath, 0));
  nodeProperties.push_back(NodeProperties("LARGE_MEM", 0x1000000, defs::INCREMENTAL, defs::READWRITE, 26214400, addrFileAbsPath, 0));

  nodeProperties.push_back(NodeProperties("SUBSYSTEM3", 0x600000, defs::HIERARCHICAL, defs::READWRITE, 1, addrFileAbsPath, 28));

  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDNODE", 0x600000, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 4));
  nodeProperties.back().type = &typeid(DummyParentNode);
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDNODE.REG", 0x600001, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDNODE.REG_WRITE_ONLY", 0x600003, defs::SINGLE, defs::WRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDNODE.REG_UPPER_MASK", 0x600004, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.back().mask = 0xFFFF0000;
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDNODE.REG_LOWER_MASK", 0x600004, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.back().mask = 0xFFFF;

  // 0x600000 + 0x10 + 0x10010
  const uint32_t lDerivedModule1Addr = 0x600000 + (0x10 | 0x10010);
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE1", lDerivedModule1Addr, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 2));
  nodeProperties.back().type = &typeid(DummyChildNode);
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE1.REG", lDerivedModule1Addr + 1, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level2_module1"), 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE1.MEM", lDerivedModule1Addr + 2, defs::INCREMENTAL, defs::READWRITE, 262144, getAddrFileAbsPath("derived_level2_module1"), 0));
  nodeProperties.back().tags = "test";

  const uint32_t lDerivedModule2Addr = 0x600000 + (0x30 | 0x10010 | 0x10010);
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE2", lDerivedModule2Addr, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 4));
  nodeProperties.back().parameters["arg3"] = "val3";
  nodeProperties.back().type = &typeid(DummyChildNode);
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE2.REG", lDerivedModule2Addr + 1, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_plain"), 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE2.REG_WRITE_ONLY", lDerivedModule2Addr + 3, defs::SINGLE, defs::WRITE, 1, getAddrFileAbsPath("derived_level3_plain"), 0));
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE2.REG_UPPER_MASK", lDerivedModule2Addr + 4, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_plain"), 0));
  nodeProperties.back().mask = 0xFFFF0000;
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE2.REG_LOWER_MASK", lDerivedModule2Addr + 4, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_plain"), 0));
  nodeProperties.back().mask = 0xFFFF;

  const uint32_t lDerivedModule3Addr = 0x600000 + (0x50 | 0x10010 | 0x10010);
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE3", lDerivedModule3Addr, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 4));
  nodeProperties.back().parameters["arg3"] = "val3";
  nodeProperties.back().type = &typeid(DummyChildNode);
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE3.REG", lDerivedModule3Addr + 1, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE3.REG_WRITE_ONLY", lDerivedModule3Addr + 3, defs::SINGLE, defs::WRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE3.REG_UPPER_MASK", lDerivedModule3Addr + 4, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().mask = 0xFFFF0000;
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE3.REG_LOWER_MASK", lDerivedModule3Addr + 4, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().mask = 0xFFFF;

  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE4", 0x610070, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 4));
  nodeProperties.back().type = &typeid(DummyChildNode);
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE4.REG", 0x610071, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().tags = "test";
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE4.REG_WRITE_ONLY", 0x610073, defs::SINGLE, defs::WRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE4.REG_UPPER_MASK", 0x610074, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().mask = 0xFFFF0000;
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.DERIVEDMODULE4.REG_LOWER_MASK", 0x610074, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_level3_class"), 0));
  nodeProperties.back().mask = 0xFFFF;

  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.BADNODE", 0x600100, defs::HIERARCHICAL, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 4));
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.BADNODE.REG", 0x600101, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.BADNODE.REG_WRITE_ONLY", 0x600103, defs::SINGLE, defs::WRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.BADNODE.REG_UPPER_MASK", 0x600104, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.back().mask = 0xFFFF0000;
  nodeProperties.push_back(NodeProperties("SUBSYSTEM3.BADNODE.REG_LOWER_MASK", 0x600104, defs::SINGLE, defs::READWRITE, 1, getAddrFileAbsPath("derived_address"), 0));
  nodeProperties.back().mask = 0xFFFF;

  nodeProperties.push_back(NodeProperties("IPBUS_ENDPOINT", 0x700000, defs::SINGLE, defs::READWRITE, 1, addrFileAbsPath, 0));
  nodeProperties.back().fwInfo["type"] = "endpoint";
  nodeProperties.back().fwInfo["width"] = "0x10";

  for (size_t i=0; i < nodeProperties.size(); i++) {
    BOOST_REQUIRE_LE(nodeProperties.at(i).descendantIds.size(), (nodeProperties.size() - i - 1));
    for (size_t j=0; j < nodeProperties.at(i).descendantIds.size(); j++) {
      std::string lRelativePath = nodeProperties.at(i+j+1).path.substr(nodeProperties.at(i).path.empty() ? 0 : nodeProperties.at(i).path.size() + 1);
      nodeProperties.at(i).descendantIds.at(j) = lRelativePath;
      nodeProperties.at(i).descendantTypes[lRelativePath] = nodeProperties.at(i+j+1).type;
    }
  }
}


void checkProperties(const uhal::Node& aNode, const NodeProperties& aExpected)
{
  BOOST_CHECK_EQUAL(aNode.getPath(), aExpected.path);
  BOOST_CHECK_EQUAL(aNode.getId(), aExpected.id);
  BOOST_CHECK_EQUAL(aNode.getAddress(), aExpected.address);
  BOOST_CHECK_EQUAL(aNode.getPermission(), aExpected.permission);
  BOOST_CHECK_EQUAL(aNode.getSize(), aExpected.size);
  BOOST_CHECK_EQUAL(aNode.getMask(), aExpected.mask);
  BOOST_CHECK_EQUAL(aNode.getMode(), aExpected.mode);

  BOOST_CHECK_EQUAL(aNode.getDescription(), aExpected.description);
  BOOST_CHECK_EQUAL(aNode.getModule(), aExpected.module);
  BOOST_CHECK_EQUAL(aNode.getTags(), aExpected.tags);

  // Validate parameters map
  BOOST_CHECK_EQUAL(aNode.getParameters().size(), aExpected.parameters.size());
  for (std::unordered_map<std::string,std::string>::const_iterator lIt=aExpected.parameters.begin(); lIt!=aExpected.parameters.end(); lIt++) {
    BOOST_CHECK_MESSAGE(aNode.getParameters().count(lIt->first) == size_t(1), "Parameter '" << lIt->first << "' was not found");
    if (aNode.getParameters().count(lIt->first) == size_t(1)){
      BOOST_CHECK_MESSAGE(aNode.getParameters().find(lIt->first)->second == lIt->second, "Parameter '" << lIt->first << "' has value '" << aNode.getParameters().find(lIt->first)->second << "'; expected value '" << lIt->second <<  "'");
    }
  }
  for (std::unordered_map<std::string,std::string>::const_iterator lIt=aNode.getParameters().begin(); lIt!=aNode.getParameters().end(); lIt++) {
    BOOST_CHECK_MESSAGE(aExpected.parameters.count(lIt->first) == size_t(1), "Node has unexpected parameter '" << lIt->first << "'" << " (value: '" << lIt->second << "')");
  }

  // Validate firmware info map
  BOOST_CHECK_EQUAL(aNode.getFirmwareInfo().size(), aExpected.fwInfo.size());
  for (std::unordered_map<std::string,std::string>::const_iterator lIt=aExpected.fwInfo.begin(); lIt!=aExpected.fwInfo.end(); lIt++) {
    BOOST_CHECK_MESSAGE(aNode.getFirmwareInfo().count(lIt->first) == size_t(1), "FW info '" << lIt->first << "' was not found");
    if (aNode.getFirmwareInfo().count(lIt->first) == size_t(1)){
      BOOST_CHECK_MESSAGE(aNode.getFirmwareInfo().find(lIt->first)->second == lIt->second, "FW info '" << lIt->first << "' has value '" << aNode.getFirmwareInfo().find(lIt->first)->second << "'; expected value '" << lIt->second <<  "'");
    }
  }
  for (std::unordered_map<std::string,std::string>::const_iterator lIt=aNode.getFirmwareInfo().begin(); lIt!=aNode.getFirmwareInfo().end(); lIt++) {
    BOOST_CHECK_MESSAGE(aExpected.fwInfo.count(lIt->first) == size_t(1), "Node has unexpected FW info '" << lIt->first << "'" << " (value: '" << lIt->second << "')");
  }
}


void checkExceptionsThrownByReadWrite(const uhal::Node& aNode, const NodeProperties& aProperties)
{
  // Single-word read/write methods - check exceptions thrown when they should be
  if (aProperties.permission == defs::READ)
    BOOST_CHECK_THROW(aNode.write(1), uhal::exception::WriteAccessDenied);
  if (aProperties.permission == defs::WRITE) {
    BOOST_CHECK_THROW(aNode.read(), uhal::exception::ReadAccessDenied);
    if (aProperties.mask != 0xFFFFFFFF)
      BOOST_CHECK_THROW(aNode.write(1), uhal::exception::WriteAccessDenied);
  }

  // Block read/write methods - check exceptions thrown when they should be 
  if (aProperties.mode == defs::SINGLE) {
    for (size_t n=0; n <= 0xF; n++) {
      if (n == 1)
        continue;
      BOOST_CHECK_THROW(aNode.readBlock(n), exception::BulkTransferOnSingleRegister);
      BOOST_CHECK_THROW(aNode.writeBlock(std::vector<uint32_t>(n)), exception::BulkTransferOnSingleRegister);
    }
    if (aProperties.permission == defs::READ)
      BOOST_CHECK_THROW(aNode.writeBlock(std::vector<uint32_t>(1)), exception::WriteAccessDenied);
    if (aProperties.permission == defs::WRITE)
      BOOST_CHECK_THROW(aNode.readBlock(1), exception::ReadAccessDenied);
  }
  else if (aProperties.mode != defs::HIERARCHICAL) {
    for (size_t n=0; n <= aNode.getSize(); n++) {
      if (aProperties.permission == defs::WRITE)
        BOOST_CHECK_THROW(aNode.readBlock(n), exception::ReadAccessDenied);
      if (aProperties.permission == defs::READ)
        BOOST_CHECK_THROW(aNode.writeBlock(std::vector<uint32_t>(n)), exception::WriteAccessDenied);
    }

    for (size_t n=aNode.getSize()+1; n <= aNode.getSize() + 0x10; n++) {
      BOOST_CHECK_THROW(aNode.readBlock(n), exception::BulkTransferRequestedTooLarge);
      BOOST_CHECK_THROW(aNode.writeBlock(std::vector<uint32_t>(n)), exception::BulkTransferRequestedTooLarge);
    }
  }

  // Block read/write offset methods - check exceptions thrown when they should be 
  switch (aProperties.mode) {
    case defs::SINGLE :
      for (size_t i=0; i <= 0xF; i++) {
        // std::cout << " i = 0x" << std::hex << i << std::endl;
        for (size_t j=0; j <= 0xF; j++) {
          BOOST_CHECK_THROW(aNode.readBlockOffset(i, j), exception::BulkTransferOffsetRequestedForSingleRegister);
          BOOST_CHECK_THROW(aNode.writeBlockOffset(std::vector<uint32_t>(i), j), exception::BulkTransferOffsetRequestedForSingleRegister);
        }
      }
      break;

    case defs::NON_INCREMENTAL :
      for (size_t i=0; i <= 0xF; i++) {
        for (size_t j=0; j <= 0xF; j++) {
          BOOST_CHECK_THROW(aNode.readBlockOffset(i, j), exception::BulkTransferOffsetRequestedForFifo);
          BOOST_CHECK_THROW(aNode.writeBlockOffset(std::vector<uint32_t>(i), j), exception::BulkTransferOffsetRequestedForFifo);
        }
      }
      break;

    case defs::INCREMENTAL :
      for (size_t lOffset=0; lOffset <= 0xF; lOffset++) {
        for (size_t n = aNode.getSize() + 1 - lOffset; n <= aNode.getSize() + 0xF - lOffset; n++) {
          BOOST_CHECK_THROW(aNode.readBlockOffset(n, lOffset), exception::BulkTransferRequestedTooLarge);
          BOOST_CHECK_THROW(aNode.writeBlockOffset(std::vector<uint32_t>(n), lOffset), exception::BulkTransferRequestedTooLarge);
        }
      }
      break;

    case defs::HIERARCHICAL :
      break;
  }
}


void checkDescendants (const uhal::Node& aNode, const NodeProperties& aProperties)
{
  // 1) Given an empty ID string, 'getNode' should return same object
  BOOST_CHECK_EQUAL(&aNode.getNode(""), &aNode);

  // 2) 'getNode' should throw given an invalid ID
  BOOST_CHECK_THROW(aNode.getNode("."), exception::NoBranchFoundWithGivenUID);
  BOOST_CHECK_THROW(aNode.getNode("some_invalid_id"), exception::NoBranchFoundWithGivenUID);

  // 3) 'getNodes' should return relative IDs of all descendants (order is not defined)
  std::vector<std::string> lReturnedIds(aNode.getNodes());
  BOOST_CHECK_EQUAL(lReturnedIds.size(), aProperties.descendantIds.size());
  std::vector<std::string> lExpectedIds(aProperties.descendantIds);
  std::sort(lReturnedIds.begin(), lReturnedIds.end());
  std::sort(lExpectedIds.begin(), lExpectedIds.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(lReturnedIds.begin(), lReturnedIds.end(), lExpectedIds.begin(), lExpectedIds.end());

  for (std::vector<std::string>::const_iterator lIt=lReturnedIds.begin(); lIt != lReturnedIds.end(); lIt++) {
    const std::type_info& lType = *aProperties.descendantTypes.at(*lIt);

    // 4) Each descendant should be accessible through 'getNode' method
    BOOST_CHECK_NO_THROW(aNode.getNode(*lIt));
    BOOST_CHECK(typeid(aNode.getNode(*lIt)) == lType);
    BOOST_CHECK_EQUAL(&aNode.getNode(*lIt), &aNode.getNode(*lIt).getNode(""));

    BOOST_CHECK_THROW(aNode.getNode("." + *lIt), exception::NoBranchFoundWithGivenUID);
    BOOST_CHECK_THROW(aNode.getNode(*lIt + "."), exception::NoBranchFoundWithGivenUID);

    // 5) Templated 'getNode' method should return pointer to same object instance as non-templated method, but throw for invalid casts
    BOOST_CHECK_EQUAL(&aNode.getNode<Node>(*lIt), &aNode.getNode(*lIt));
    if (lType == typeid(Node)) {
      BOOST_CHECK_THROW(aNode.getNode<DummyParentNode>(*lIt), exception::BadNodeCast);
      BOOST_CHECK_THROW(aNode.getNode<DummyChildNode>(*lIt), exception::BadNodeCast);
    }
    else if (lType == typeid(DummyParentNode)) {
      BOOST_CHECK_EQUAL(static_cast<const Node*>(&aNode.getNode<DummyParentNode>(*lIt)), &aNode.getNode(*lIt));
      BOOST_CHECK_THROW(aNode.getNode<DummyChildNode>(*lIt), exception::BadNodeCast);
    }
    else if (lType == typeid(DummyChildNode)) {
      BOOST_CHECK_EQUAL(static_cast<const Node*>(&aNode.getNode<DummyParentNode>(*lIt)), &aNode.getNode(*lIt));
      BOOST_CHECK_EQUAL(static_cast<const Node*>(&aNode.getNode<DummyChildNode>(*lIt)), &aNode.getNode(*lIt));
    }
    else {
      BOOST_CHECK_MESSAGE(false, "Unit tests have not been written to handle node class '" << aProperties.type->name() << "'");
    }

    // 6) For each descendant, the same node object should be returned by calling 'getNode' once using relative ID path, or twice using 2 parts of relative ID path
    size_t lPositionOfDot(lIt->find("."));
    while (lPositionOfDot != std::string::npos) {
      std::string lPathPart1(lIt->substr(0, lPositionOfDot));
      std::string lPathPart2(lIt->substr(lPositionOfDot+1));

      BOOST_CHECK_EQUAL(&aNode.getNode(*lIt), &aNode.getNode(lPathPart1).getNode(lPathPart2));

      lPositionOfDot = lIt->find(".", lPositionOfDot + 1);
    }
  }

  // TODO: Add test that vector returned by getNodes("someRegex") is correct
}


bool nodeAddrCompare (const Node* aNodeL, const Node* aNodeR)
{
  return ( aNodeL->getAddress() < aNodeR->getAddress() );
}


void checkIteration(const uhal::Node& aNode, const NodeProperties& aProperties)
{
  // Check that sequence of nodes returned by iterator is correct
  std::vector<const uhal::Node*> lExpectedNodes;
  lExpectedNodes.push_back(&aNode);
  for (std::vector<std::string>::const_iterator lIt = aProperties.descendantIds.begin(); lIt != aProperties.descendantIds.end(); lIt++)
    lExpectedNodes.push_back(&aNode.getNode(*lIt));
  std::stable_sort(lExpectedNodes.begin(), lExpectedNodes.end(), nodeAddrCompare);

  size_t lItCount = 0;
  for (uhal::Node::const_iterator lIt = aNode.begin(); lIt != aNode.end(); lIt++, lItCount++) {
    if (lItCount < lExpectedNodes.size())
      BOOST_CHECK_EQUAL(&*lIt, lExpectedNodes.at(lItCount));
    else
      BOOST_CHECK(false);
  }
  BOOST_CHECK_EQUAL(lItCount, lExpectedNodes.size());
}


void checkLineage(const uhal::Node& aTopNode, const uhal::Node& aNode)
{
  // 1) Check lineage returned with top-most node as argument
  std::vector<const Node*> lExpected(1, &aTopNode);
  const std::string lPath(aNode.getPath());

  size_t lDotIndex = aNode.getPath().find('.');
  while (lDotIndex != std::string::npos) {
    lExpected.push_back(&aTopNode.getNode(lPath.substr(0, lDotIndex)));
    lDotIndex = aNode.getPath().find('.', lDotIndex + 1);
  }

  const std::vector<const Node*> lReturned = aNode.getLineage(aTopNode);
  BOOST_CHECK_EQUAL_COLLECTIONS(lReturned.begin(), lReturned.end(), lExpected.begin(), lExpected.end());

  // 2) Check lineage returned with intermediate-level nodes as argument (e.g. top node's child, grandchild, etc)
  for (size_t i = 1; i < lExpected.size(); i++) {
    const std::vector<const Node*> lExpected2(lExpected.begin()+i, lExpected.end());
    const std::vector<const Node*> lReturned2(aNode.getLineage(*lExpected.at(i)));

    BOOST_CHECK_EQUAL_COLLECTIONS(lReturned2.begin(), lReturned2.end(), lExpected2.begin(), lExpected2.end());
  }
}


void checkNodeTree(const uhal::Node& aNode, const std::vector<NodeProperties>& aExpectedProperties)
{
  for (std::vector<NodeProperties>::const_iterator lIt = aExpectedProperties.begin(); lIt != aExpectedProperties.end(); lIt++) {
    BOOST_TEST_MESSAGE("Node '" << lIt->path << "'");
    const uhal::Node& lNode = (lIt->path.empty() ? aNode : aNode.getNode(lIt->path));

    checkProperties(lNode, *lIt);
    checkExceptionsThrownByReadWrite(lNode, *lIt);
    checkDescendants(lNode, *lIt);
    checkIteration(lNode, *lIt);

    if (&lNode != &aNode)
      checkLineage(aNode, lNode);
    else
      BOOST_CHECK_THROW(lNode.getLineage(aNode), std::runtime_error);
  }
}


pugi::xml_node getNthChild(const pugi::xml_node& aNode, const size_t aIndex)
{
  pugi::xml_node lNode = aNode.first_child();
  for (size_t i = 0; i < aIndex; i++)
    lNode = lNode.next_sibling();
  return lNode;
}

void setAttribute(pugi::xml_node aNode, const std::string& aName, const std::string& aValue)
{
  aNode.remove_attribute(aName.c_str());
  aNode.append_attribute(aName.c_str()).set_value(aValue.c_str());
}


BOOST_AUTO_TEST_SUITE( nodes )


BOOST_FIXTURE_TEST_CASE (dummy_address_files, DummyAddressFileFixture) {
  const std::shared_ptr<uhal::Node> lTopNode(NodeTreeBuilder::getInstance().getNodeTree(addrFileURI, boost::filesystem::current_path() / "."));

  checkNodeTree(*lTopNode, nodeProperties);
}


BOOST_FIXTURE_TEST_CASE (address_description, DummyAddressFileFixture) {
  const std::shared_ptr<uhal::Node> lTopNode(NodeTreeBuilder::getInstance().getNodeTree(addrFileURI, boost::filesystem::current_path() / "."));

  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0, 0), "no matching nodes");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 1, 0), "node \"REG\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 2, 0), "node \"REG_READ_ONLY\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 3, 0), "node \"REG_WRITE_ONLY\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 4, 0), "nodes \"REG_UPPER_MASK\", \"REG_LOWER_MASK\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 4, 1), "2 nodes match");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 4, 2), "nodes \"REG_UPPER_MASK\", \"REG_LOWER_MASK\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 4, 3), "nodes \"REG_UPPER_MASK\", \"REG_LOWER_MASK\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 5, 0), "nodes \"REG_MASKED_READ_ONLY\", \"REG_MASKED_WRITE_ONLY\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 5, 1), "2 nodes match");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 5, 2), "nodes \"REG_MASKED_READ_ONLY\", \"REG_MASKED_WRITE_ONLY\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 5, 3), "nodes \"REG_MASKED_READ_ONLY\", \"REG_MASKED_WRITE_ONLY\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 6, 0), "nodes \"REG_PARS\", \"REG_OUT_OF_ORDER\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 6, 1), "2 nodes match");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 6, 2), "nodes \"REG_PARS\", \"REG_OUT_OF_ORDER\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 6, 3), "nodes \"REG_PARS\", \"REG_OUT_OF_ORDER\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 7, 0), "no matching nodes");

  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x0fffff, 0), "no matching nodes");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x100000, 0), "node \"MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x100001, 0), "node \"MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x100010, 0), "node \"MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x130000, 0), "node \"MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x13ffff, 0), "node \"MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x140000, 0), "no matching nodes");

  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x200000, 0), "no matching nodes");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x210002, 0), "node \"SUBSYSTEM1.REG\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x210003, 0), "node \"SUBSYSTEM1.MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x210004, 0), "node \"SUBSYSTEM1.MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x250002, 0), "node \"SUBSYSTEM1.MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x250003, 0), "no matching nodes");

  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x270001, 0), "no matching nodes");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x270002, 0), "node \"SUBSYSTEM1.SUBMODULE.REG\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x270003, 0), "node \"SUBSYSTEM1.SUBMODULE.MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x270004, 0), "node \"SUBSYSTEM1.SUBMODULE.MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x270102, 0), "node \"SUBSYSTEM1.SUBMODULE.MEM\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x270103, 0), "no matching nodes");

  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x600000, 0), "no matching nodes");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x600001, 0), "node \"SUBSYSTEM3.DERIVEDNODE.REG\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x600002, 0), "no matching nodes");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x600003, 0), "node \"SUBSYSTEM3.DERIVEDNODE.REG_WRITE_ONLY\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x600004, 0), "nodes \"REG_UPPER_MASK\", \"REG_LOWER_MASK\" under \"SUBSYSTEM3.DERIVEDNODE\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x600004, 1), "2 nodes under \"SUBSYSTEM3.DERIVEDNODE\" match");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x600004, 2), "nodes \"REG_UPPER_MASK\", \"REG_LOWER_MASK\" under \"SUBSYSTEM3.DERIVEDNODE\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x600004, 3), "nodes \"REG_UPPER_MASK\", \"REG_LOWER_MASK\" under \"SUBSYSTEM3.DERIVEDNODE\"");
  BOOST_CHECK_EQUAL(detail::getAddressDescription(*lTopNode, 0x600005, 0), "no matching nodes");
}


BOOST_AUTO_TEST_SUITE( simple )

BOOST_FIXTURE_TEST_CASE (valid_default, SimpleAddressTableFixture)
{
  std::shared_ptr<Node> lNode(NodeTreeBuilder::getInstance().build(addrTableDoc.child ( "node" ), boost::filesystem::path()));
  checkNodeTree(*lNode, nodeProperties);
}

BOOST_FIXTURE_TEST_CASE (missing_ID, SimpleAddressTableFixture)
{
  for (size_t i = 0; i < nodeProperties.size(); i++) {
    BOOST_TEST_MESSAGE("Removing 'id' attribute from node " << i);

    pugi::xml_document lDoc;
    lDoc.load_string(addrTableStr.c_str());
    getNthChild(lDoc.child("node"), i).remove_attribute("id");

    BOOST_CHECK_THROW(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()), exception::NoRulesPassed);
  }
}

BOOST_FIXTURE_TEST_CASE (invalid_ID, SimpleAddressTableFixture)
{
  std::vector<std::string> lBadValues;
  lBadValues.push_back("");
  lBadValues.push_back(" ");
  lBadValues.push_back("   ");
  lBadValues.push_back(".");
  lBadValues.push_back("bob.");
  lBadValues.push_back(".bob");
  lBadValues.push_back("some.value");

  for (std::vector<std::string>::const_iterator lIt = lBadValues.begin(); lIt != lBadValues.end(); lIt++)
  {
    for (size_t i = 0; i < nodeProperties.size(); i++) {
      BOOST_TEST_MESSAGE("Setting 'id' attribute of node " << i << " to '" << *lIt << "'");

      pugi::xml_document lDoc;
      lDoc.load_string(addrTableStr.c_str());
      setAttribute(getNthChild(lDoc.child("node"), i), "id", *lIt);

      BOOST_CHECK_THROW(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()), exception::NodeAttributeIncorrectValue);
    }
  }
}

BOOST_FIXTURE_TEST_CASE (invalid_address, SimpleAddressTableFixture)
{
  std::vector<std::string> lBadValues;
  lBadValues.push_back("");
  lBadValues.push_back(" ");
  lBadValues.push_back("   ");
  lBadValues.push_back("-1");
  lBadValues.push_back("-0x11");
  lBadValues.push_back("0x");
  lBadValues.push_back("x");
  lBadValues.push_back("3.14");
  lBadValues.push_back("bob");
  lBadValues.push_back("0x1234bob5678");

  // lBadValues.push_back("0x100000000");
  // lBadValues.push_back("0x1FFF1234abcd");

  for (std::vector<std::string>::const_iterator lIt = lBadValues.begin(); lIt != lBadValues.end(); lIt++)
  {
    for (size_t i = 0; i < nodeProperties.size(); i++) {
      BOOST_TEST_MESSAGE("Setting 'address' attribute of node " << i << " to '" << *lIt << "'");

      pugi::xml_document lDoc;
      lDoc.load_string(addrTableStr.c_str());
      setAttribute(getNthChild(lDoc.child("node"), i), "address", *lIt);

      BOOST_CHECK_THROW(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()), exception::NodeAttributeIncorrectValue);
    }
  }
}

BOOST_FIXTURE_TEST_CASE (invalid_mask, SimpleAddressTableFixture)
{
  std::vector<std::string> lBadValues;
  lBadValues.push_back("");
  lBadValues.push_back(" ");
  lBadValues.push_back("   ");
  lBadValues.push_back("-1");
  lBadValues.push_back("42a");
  lBadValues.push_back("-0x11");
  lBadValues.push_back("0x");
  lBadValues.push_back("x");
  lBadValues.push_back("3.14");
  lBadValues.push_back("bob");
  lBadValues.push_back("0x1234bob5678");

  // lBadValues.push_back("0x100000000");
  // lBadValues.push_back("0x1FFF1234abcd");

  for (std::vector<std::string>::const_iterator lIt = lBadValues.begin(); lIt != lBadValues.end(); lIt++)
  {
    for (size_t i = 0; i < 2; i++) {
      BOOST_TEST_MESSAGE("Setting 'mask' attribute of node " << i << " to '" << *lIt << "'");

      pugi::xml_document lDoc;
      lDoc.load_string(addrTableStr.c_str());
      setAttribute(getNthChild(lDoc.child("node"), i), "mask", *lIt);

      BOOST_CHECK_THROW(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()), exception::NodeAttributeIncorrectValue);
    }
  }
}

BOOST_FIXTURE_TEST_CASE (invalid_size, SimpleAddressTableFixture)
{
  std::vector<std::string> lBadValues;
  lBadValues.push_back("");
  lBadValues.push_back(" ");
  lBadValues.push_back("   ");
  lBadValues.push_back("-1");
  lBadValues.push_back("42a");
  lBadValues.push_back("-0x11");
  lBadValues.push_back("0x");
  lBadValues.push_back("x");
  lBadValues.push_back("3.14");
  lBadValues.push_back("bob");
  lBadValues.push_back("0x1234bob5678");

  // lBadValues.push_back("0x100000000");
  // lBadValues.push_back("0x1FFF1234abcd");

  for (std::vector<std::string>::const_iterator lIt = lBadValues.begin(); lIt != lBadValues.end(); lIt++)
  {
    for (size_t i = 2; i < nodeProperties.size(); i++) {
      BOOST_TEST_MESSAGE("Setting 'size' attribute of node " << i << " to '" << *lIt << "'");

      pugi::xml_document lDoc;
      lDoc.load_string(addrTableStr.c_str());
      setAttribute(getNthChild(lDoc.child("node"), i), "size", *lIt);

      BOOST_CHECK_THROW(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()), exception::NodeAttributeIncorrectValue);
    }
  }
}

BOOST_FIXTURE_TEST_CASE (invalid_permission, SimpleAddressTableFixture)
{
  std::vector<std::string> lBadValues;
  lBadValues.push_back("bob");
  lBadValues.push_back("some_invalid_string");
  lBadValues.push_back("");
  lBadValues.push_back("R");
  lBadValues.push_back("W");
  lBadValues.push_back("RW");
  lBadValues.push_back("WR");
  lBadValues.push_back("READ");
  lBadValues.push_back("WRITE");
  lBadValues.push_back("READWRITE");
  lBadValues.push_back("WRITEREAD");

  lBadValues.push_back("r ");

  for (std::vector<std::string>::const_iterator lIt = lBadValues.begin(); lIt != lBadValues.end(); lIt++)
  {
    for (size_t i = 0; i < nodeProperties.size(); i++) {
      BOOST_TEST_MESSAGE("Setting 'permission' attribute of node " << i << " to '" << *lIt << "'");

      pugi::xml_document lDoc;
      lDoc.load_string(addrTableStr.c_str());
      setAttribute(getNthChild(lDoc.child("node"), i), "permission", *lIt);

      BOOST_CHECK_THROW(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()), exception::NodeAttributeIncorrectValue);
    }
  }
}

BOOST_FIXTURE_TEST_CASE (invalid_mode, SimpleAddressTableFixture)
{
  std::vector<std::string> lBadValues;
  lBadValues.push_back("bob");
  lBadValues.push_back("some_invalid_string");
  lBadValues.push_back("");
  lBadValues.push_back("SINGLE");
  lBadValues.push_back("BLOCK");
  lBadValues.push_back("INCREMENTAL");
  lBadValues.push_back("INC");
  lBadValues.push_back("PORT");
  lBadValues.push_back("NON-INCREMENTAL");
  lBadValues.push_back("NON-INC");

  lBadValues.push_back("single ");

  for (std::vector<std::string>::const_iterator lIt = lBadValues.begin(); lIt != lBadValues.end(); lIt++)
  {
    for (size_t i = 0; i < nodeProperties.size(); i++) {
      BOOST_TEST_MESSAGE("Setting 'mode' attribute of node " << i << " to '" << *lIt << "'");

      pugi::xml_document lDoc;
      lDoc.load_string(addrTableStr.c_str());
      setAttribute(getNthChild(lDoc.child("node"), i), "mode", *lIt);

      BOOST_CHECK_THROW(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()), exception::NodeAttributeIncorrectValue);
    }
  }
}

BOOST_FIXTURE_TEST_CASE (size_without_node_attribute, SimpleAddressTableFixture)
{
  for (size_t i = 0; i < 2; i++) {
    BOOST_TEST_MESSAGE("Adding 'size' attribute to node " << i << ", that doesn't have 'mode' attribute");

    pugi::xml_document lDoc;
    lDoc.load_string(addrTableStr.c_str());
    setAttribute(getNthChild(lDoc.child("node"), i), "size", "42");

    // This should change to 'BOOST_CHECK_THROW' in future release (after grace period discussed in issue #194 has ended)
    BOOST_CHECK_NO_THROW(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));
  }
}


BOOST_AUTO_TEST_SUITE_END()


BOOST_AUTO_TEST_SUITE( overlapChecks )


BOOST_FIXTURE_TEST_CASE (no_overlap, AddressTableOverlapFixture)
{
  std::shared_ptr<Node> lNode(NodeTreeBuilder::getInstance().build(addrTableDoc.child ( "node" ), boost::filesystem::path()));

  checkNodeTree(*lNode, nodeProperties);

  BOOST_CHECK_EQUAL(detail::getAddressOverlaps(*lNode).size(), 0);
}


BOOST_FIXTURE_TEST_CASE (overlap_unmasked_registers, AddressTableOverlapFixture)
{
  // Move reg1 to same address as reg2 (0x1)
  pugi::xml_document lDoc;
  lDoc.load_string(addrTableStr.c_str());
  setAttribute(getNthChild(lDoc.child("node"), 0), "address", "0x1");
  std::shared_ptr<const Node> lNode(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));

  // Expected overlaps
  std::vector<std::pair<const Node*, const Node*> > lExpected;
  lExpected.push_back( std::make_pair(&lNode->getNode("reg1"), &lNode->getNode("reg2")) );

  // Compare expectations with result
  std::vector<std::pair<const Node*, const Node*> > lResult = detail::getAddressOverlaps(*lNode);
  BOOST_CHECK_EQUAL_COLLECTIONS(lResult.begin(), lResult.end(), lExpected.begin(), lExpected.end());
}


BOOST_FIXTURE_TEST_CASE (overlap_masked_registers_1, AddressTableOverlapFixture)
{
  // Move reg4 to same address as reg3 (0x2)
  pugi::xml_document lDoc;
  lDoc.load_string(addrTableStr.c_str());
  setAttribute(getNthChild(lDoc.child("node"), 3), "address", "0x2");
  std::shared_ptr<const Node> lNode(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));

  // Expected overlaps
  std::vector<std::pair<const Node*, const Node*> > lExpected;
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3"), &lNode->getNode("reg4")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3"), &lNode->getNode("reg4.A")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3"), &lNode->getNode("reg4.B")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3"), &lNode->getNode("reg4.C")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3"), &lNode->getNode("reg4.D")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3.A"), &lNode->getNode("reg4")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3.A"), &lNode->getNode("reg4.A")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3.A"), &lNode->getNode("reg4.B")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3.B"), &lNode->getNode("reg4")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3.B"), &lNode->getNode("reg4.C")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3.B"), &lNode->getNode("reg4.D")) );

  // Compare expectations with result
  std::vector<std::pair<const Node*, const Node*> > lResult = detail::getAddressOverlaps(*lNode);
  BOOST_CHECK_EQUAL_COLLECTIONS(lResult.begin(), lResult.end(), lExpected.begin(), lExpected.end());
}


BOOST_FIXTURE_TEST_CASE (overlap_masked_registers_2, AddressTableOverlapFixture)
{
  // Change mask of reg4.A to 0x10100ff to overlap with reg4.C and reg4.D
  pugi::xml_document lDoc;
  lDoc.load_string(addrTableStr.c_str());
  setAttribute(getNthChild(getNthChild(lDoc.child("node"), 3), 0), "mask", "0x10100ff");
  std::shared_ptr<const Node> lNode(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));

  // Expected overlaps
  std::vector<std::pair<const Node*, const Node*> > lExpected;
  lExpected.push_back( std::make_pair(&lNode->getNode("reg4.A"), &lNode->getNode("reg4.C")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg4.A"), &lNode->getNode("reg4.D")) );

  // Compare expectations with result
  std::vector<std::pair<const Node*, const Node*> > lResult = detail::getAddressOverlaps(*lNode);
  BOOST_CHECK_EQUAL_COLLECTIONS(lResult.begin(), lResult.end(), lExpected.begin(), lExpected.end());
}


BOOST_FIXTURE_TEST_CASE (overlap_ports, AddressTableOverlapFixture)
{
  // Move port1 to same address as port2 (0x5)
  pugi::xml_document lDoc;
  lDoc.load_string(addrTableStr.c_str());
  setAttribute(getNthChild(lDoc.child("node"), 4), "address", "0x5");
  std::shared_ptr<const Node> lNode(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));

  // Expected overlaps
  std::vector<std::pair<const Node*, const Node*> > lExpected;
  lExpected.push_back( std::make_pair(&lNode->getNode("port1"), &lNode->getNode("port2")) );

  // Compare expectations with result
  std::vector<std::pair<const Node*, const Node*> > lResult = detail::getAddressOverlaps(*lNode);
  BOOST_CHECK_EQUAL_COLLECTIONS(lResult.begin(), lResult.end(), lExpected.begin(), lExpected.end());
}


BOOST_FIXTURE_TEST_CASE (overlap_unmasked_reg_vs_masked_reg, AddressTableOverlapFixture)
{
  // Move reg1 to same address as reg3 (0x2)
  pugi::xml_document lDoc;
  lDoc.load_string(addrTableStr.c_str());
  setAttribute(getNthChild(lDoc.child("node"), 0), "address", "0x2");
  std::shared_ptr<const Node> lNode(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));

  // Expected overlaps
  std::vector<std::pair<const Node*, const Node*> > lExpected;
  lExpected.push_back( std::make_pair(&lNode->getNode("reg1"), &lNode->getNode("reg3")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg1"), &lNode->getNode("reg3.A")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg1"), &lNode->getNode("reg3.B")) );

  // Compare expectations with result
  std::vector<std::pair<const Node*, const Node*> > lResult = detail::getAddressOverlaps(*lNode);
  BOOST_CHECK_EQUAL_COLLECTIONS(lResult.begin(), lResult.end(), lExpected.begin(), lExpected.end());
}


BOOST_FIXTURE_TEST_CASE (overlap_unmasked_reg_vs_port, AddressTableOverlapFixture)
{
  // Move reg1 to same address as port1 (0x4)
  pugi::xml_document lDoc;
  lDoc.load_string(addrTableStr.c_str());
  setAttribute(getNthChild(lDoc.child("node"), 0), "address", "0x4");
  std::shared_ptr<const Node> lNode(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));

  // Expected overlaps
  std::vector<std::pair<const Node*, const Node*> > lExpected;
  lExpected.push_back( std::make_pair(&lNode->getNode("reg1"), &lNode->getNode("port1")) );

  // Compare expectations with result
  std::vector<std::pair<const Node*, const Node*> > lResult = detail::getAddressOverlaps(*lNode);
  BOOST_CHECK_EQUAL_COLLECTIONS(lResult.begin(), lResult.end(), lExpected.begin(), lExpected.end());
}


BOOST_FIXTURE_TEST_CASE (overlap_masked_reg_vs_port, AddressTableOverlapFixture)
{
  // Move reg3 to same address as port1 (0x4)
  pugi::xml_document lDoc;
  lDoc.load_string(addrTableStr.c_str());
  setAttribute(getNthChild(lDoc.child("node"), 2), "address", "0x4");
  std::shared_ptr<const Node> lNode(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));

  // Expected overlaps
  std::vector<std::pair<const Node*, const Node*> > lExpected;
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3"), &lNode->getNode("port1")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3.A"), &lNode->getNode("port1")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("reg3.B"), &lNode->getNode("port1")) );

  // Compare expectations with result
  std::vector<std::pair<const Node*, const Node*> > lResult = detail::getAddressOverlaps(*lNode);
  BOOST_CHECK_EQUAL_COLLECTIONS(lResult.begin(), lResult.end(), lExpected.begin(), lExpected.end());
}


BOOST_FIXTURE_TEST_CASE (overlap_ram, AddressTableOverlapFixture)
{
  // Create list of nodes that will be moved
  //   pair.first = index of XML nodes
  //   pair.second = node paths that will be flagged by overlap check
  std::vector<std::pair<size_t, std::vector<std::string> > > lModifiedNodes(4);

  lModifiedNodes.at(0).first = 0;
  lModifiedNodes.at(0).second.push_back("reg1");

  lModifiedNodes.at(1).first = 2;
  lModifiedNodes.at(1).second.push_back("reg3");
  lModifiedNodes.at(1).second.push_back("reg3.A");
  lModifiedNodes.at(1).second.push_back("reg3.B");

  lModifiedNodes.at(2).first = 4;
  lModifiedNodes.at(2).second.push_back("port1");

  lModifiedNodes.at(3).first = 6;
  lModifiedNodes.at(3).second.push_back("ram1");

  // Target addresses that above nodes will be moved to. First, middle and last address of 'ram2'
  std::vector<std::string> lAddresses;
  lAddresses.push_back("0x10");
  lAddresses.push_back("0x1a");
  lAddresses.push_back("0x1f");

  for (std::vector<std::pair<size_t, std::vector<std::string> > >::const_iterator lNodeIt = lModifiedNodes.begin(); lNodeIt != lModifiedNodes.end(); lNodeIt++)
  {
    for (std::vector<std::string>::const_iterator lAddrIt = lAddresses.begin(); lAddrIt != lAddresses.end(); lAddrIt++)
    {
      BOOST_TEST_MESSAGE("Moving '" << lNodeIt->second.front() << "' to address " << *lAddrIt);

      // Move node to get an overlap
      pugi::xml_document lDoc;
      lDoc.load_string(addrTableStr.c_str());
      setAttribute(getNthChild(lDoc.child("node"), lNodeIt->first), "address", lAddrIt->c_str());
      std::shared_ptr<const Node> lNode(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));

      // Expected overlaps
      std::vector<std::pair<const Node*, const Node*> > lExpected;
      for (size_t i = 0; i < lNodeIt->second.size(); i++)
      {
        if (lAddrIt == lAddresses.begin())
          lExpected.push_back( std::make_pair(&lNode->getNode(lNodeIt->second.at(i)), &lNode->getNode("ram2")) );
        else
          lExpected.push_back( std::make_pair(&lNode->getNode("ram2"), &lNode->getNode(lNodeIt->second.at(i))) );
      }

      // Compare expectations with result
      std::vector<std::pair<const Node*, const Node*> > lResult = detail::getAddressOverlaps(*lNode);
      BOOST_CHECK_EQUAL_COLLECTIONS(lResult.begin(), lResult.end(), lExpected.begin(), lExpected.end());
    }
  }
}


BOOST_FIXTURE_TEST_CASE (overlap_multiple_1, AddressTableOverlapFixture)
{
  // Move module 1 to address 0x0
  pugi::xml_document lDoc;
  lDoc.load_string(addrTableStr.c_str());
  setAttribute(getNthChild(lDoc.child("node"), 8), "address", "0x0");
  std::shared_ptr<const Node> lNode(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));

  // Expected overlaps
  std::vector<std::pair<const Node*, const Node*> > lExpected;
  lExpected.push_back( std::make_pair(&lNode->getNode("reg1"), &lNode->getNode("module1.reg1")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2"), &lNode->getNode("reg2")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2.mask1"), &lNode->getNode("reg2")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2.mask2"), &lNode->getNode("reg2")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2.mask3"), &lNode->getNode("reg2")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.port"), &lNode->getNode("reg3")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.port"), &lNode->getNode("reg3.A")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.port"), &lNode->getNode("reg3.B")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.ram"), &lNode->getNode("ram2")) );

  // Compare expectations with result
  std::vector<std::pair<const Node*, const Node*> > lResult = detail::getAddressOverlaps(*lNode);
  BOOST_CHECK_EQUAL_COLLECTIONS(lResult.begin(), lResult.end(), lExpected.begin(), lExpected.end());
}


BOOST_FIXTURE_TEST_CASE (overlap_multiple_2, AddressTableOverlapFixture)
{
  // Move module 2 to same address as module 1 (0x40)
  pugi::xml_document lDoc;
  lDoc.load_string(addrTableStr.c_str());
  setAttribute(getNthChild(lDoc.child("node"), 9), "address", "0x40");
  std::shared_ptr<const Node> lNode(NodeTreeBuilder::getInstance().build(lDoc.child ( "node" ), boost::filesystem::path()));

  // Expected overlaps
  std::vector<std::pair<const Node*, const Node*> > lExpected;
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg1"), &lNode->getNode("module2.regA")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2"), &lNode->getNode("module2.regB")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2"), &lNode->getNode("module2.regB.A")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2"), &lNode->getNode("module2.regB.B")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2"), &lNode->getNode("module2.regB.C")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2.mask1"), &lNode->getNode("module2.regB")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2.mask1"), &lNode->getNode("module2.regB.A")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2.mask2"), &lNode->getNode("module2.regB")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2.mask2"), &lNode->getNode("module2.regB.B")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2.mask3"), &lNode->getNode("module2.regB")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.reg2.mask3"), &lNode->getNode("module2.regB.C")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.port"), &lNode->getNode("module2.fifo")) );
  lExpected.push_back( std::make_pair(&lNode->getNode("module1.ram"), &lNode->getNode("module2.block")) );

  // Compare expectations with result
  std::vector<std::pair<const Node*, const Node*> > lResult = detail::getAddressOverlaps(*lNode);
  BOOST_CHECK_EQUAL_COLLECTIONS(lResult.begin(), lResult.end(), lExpected.begin(), lExpected.end());
}


BOOST_AUTO_TEST_SUITE_END()


BOOST_AUTO_TEST_SUITE_END()

}
}
